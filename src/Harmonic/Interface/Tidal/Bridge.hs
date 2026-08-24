-- |
-- Module      : Harmonic.Interface.Tidal.Bridge
-- Description : TidalCycles interface for harmonic progressions
--
-- Bridge between the harmonic generation engine and TidalCycles live coding.
-- Chord selection via mininotation patterns (@Pattern Int@).
--
-- Two arrangement strategies:
--
-- * 'arrange' — onset-join with kinetics range gating: each note maps
--   through the chord active at its onset time, masked by kinetics signal.
--
-- * 'arrange'' — squeeze with kinetics range gating: each chord slot
--   gets the full input pattern compressed to fit.
--
-- Both take a progression modifier @(P.Progression -> P.Progression)@
-- and read the base progression from @kProg k@ via @innerJoin@.

module Harmonic.Interface.Tidal.Bridge
  ( -- * Voice Functions
    VoiceFunction
  , voiceRange
  , layerForVoicing

    -- * Chord Selection Helpers
  , warp
  , rep

    -- * Arrangement
  , arrange       -- onset-join with kinetics
  , arrange'      -- squeeze with kinetics

    -- * Parallelism Harmoniser
  , parallel      -- stack fixed-interval parallel voices over arrange output

    -- * Chord Lookup
  , lookupChordAt
  , lookupChord
  , lookupProgression

    -- * Progression Overlap (Re-exports from Arranger)
  , overlapF
  , overlapB
  , overlap

    -- * Eager-forcing helper (shared with LineHarmony)
  , forceAll
  ) where

-- Phase B imports
import qualified Harmonic.Rules.Types.Progression as P
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import Harmonic.Rules.Types.ProgressionContext (Layer(..))
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Interface.Tidal.Arranger as A
import Harmonic.Interface.Tidal.Form (Kinetics(..), IK)

import Data.List (nub)
import Data.Maybe (isJust)
import Sound.Tidal.Context hiding (voice)

-------------------------------------------------------------------------------
-- Voice Function Types
-------------------------------------------------------------------------------

-- |Voice function type: extracts integer pitch sequences from progression
type VoiceFunction = P.Progression -> [[Int]]

-- |Filter pattern events by MIDI note range
voiceRange :: (Int, Int) -> Pattern Int -> Pattern Int
voiceRange (lo, hi) = filterValues (\v -> v >= lo && v <= hi)

-- |Force every element of a nested 'Note' list to WHNF. Used to hoist the
-- per-bar voicing computation (which can be expensive for large mode
-- chroma) from the audio query thread to REPL evaluation time. Returns
-- @()@ so callers can compose via 'seq'.
forceAll :: [[Note]] -> ()
forceAll = foldr (\xs acc -> foldr seq acc xs) ()

-------------------------------------------------------------------------------
-- Chord Selection Helpers
-------------------------------------------------------------------------------

-- |Parse a mininotation chord selection pattern (bar-relative).
-- The @/N@ divisor specifies the number of bars the pattern spans.
--
-- @
-- let r = warp \"[1 2 3 4]\/4\"   -- 4 chords over 4 bars (1 per bar)
-- let r = warp \"[1 2]\/8\"       -- 2 chords over 8 bars (4 bars each)
-- @
warp :: String -> Pattern Int
warp s = slow 4 $ parseBP_E s

-- |Generate a sequential chord selection pattern from a progression.
-- Auto-derives length from the progression. Timing is bar-relative.
--
-- @
-- let r = rep s4 1     -- 4 chords over 4 bars (1 bar each)
-- let r = rep s4 0.5   -- 4 chords over 2 bars (half bar each)
-- @
rep :: PC.ProgressionContext -> Pattern Time -> Pattern Int
rep pc repVal =
  let n = PC.pcLength pc
  in slow (fromIntegral n * repVal * 4) $ fastcat $ map pure [1..n]

-------------------------------------------------------------------------------
-- Arrangement: arrange (onset-join)
-------------------------------------------------------------------------------

-- |Map notes through chords using onset-time lookup, with kinetics range gating.
--
-- The base progression and chord selection are read from @IK@.
-- The modifier function transforms the progression (e.g. @overlapF 0@, @id@).
-- Events are masked by the kinetics signal: only active when kSignal is
-- within the @(lo, hi)@ range. Form-driven dynamics (@kDynamic@) are applied
-- automatically.
--
-- Parameter order: context first (kinetics range, IK, MIDI range), then
-- interactive (voice function, modifier, patterns).
-- |Project the requested layer from a context together with its voicing
-- route. Strict-context routing: the S\/M layers of a genP-provenance
-- context are THE curated 5\/7-PC chroma (uniform per layer) and are
-- always voiced by 'A.strataModeFlow' — degree\/"key-signature" semantics:
-- pattern index @i@ plays the i-th scale degree of that bar's set. The T
-- layer always honours the user's 'VoiceFunction', as do S\/M layers of
-- gen\/gen4 contexts (ordinary-harmony duplicates of the triad layer).
--
-- Routing by provenance + layer replaces the old first-bar cardinality
-- sniff (@isOctaSM@), which mis-routed mixed-cardinality material in both
-- directions (triad-first-bar chroma escaped to the DP; 4-note-first-bar
-- harmony was captured by the chroma engine).
layerForVoicing :: Layer -> PC.ProgressionContext -> (Bool, P.Progression)
layerForVoicing lyr ctx =
  let chroma = lyr /= T && isJust (PC.pcProvenance ctx)
  in (chroma, PC.layer lyr ctx)

-- | Render scale-degree patterns into a playable 'ControlPattern', reading
-- pitches from the progression under the given voicing strategy.
--
-- The workhorse of the Tidal interface: every orchestral instrument in
-- "Harmonic.Interface.Tidal.Orchestra" is a thin wrapper around it.
--
-- @d1 $ arrange (0,1) k (-9,9) T flow id [\"0 1 2 3\"]@
arrange :: (Double, Double)                     -- ^ Kinetics range
        -> IK                                    -- ^ Performance context (kinetics + chord selection)
        -> (Int, Int)                            -- ^ Degree-index trim for the input patterns (scale degrees, not MIDI; instrument-range clipping happens later via clip)
        -> Layer                                 -- ^ Progression layer to voice (T | S | M)
        -> VoiceFunction                         -- ^ Voice function (flow, root, etc.)
        -> (P.Progression -> P.Progression)      -- ^ Progression modifier (overlapF 0, id, etc.)
        -> [Pattern Int]                         -- ^ Input patterns to harmonize
        -> Pattern ValueMap
arrange (lo, hi) (kin, chordPat) register lyr voiceFunc modifier pats =
  let -- Pre-compute note range filter ONCE (shared across all innerJoin invocations)
      ranged = voiceRange register (stack pats)
      -- Project the 3-layer kProg pattern to the requested layer once,
      -- carrying the provenance-based voicing route with it
      progPat = fmap (layerForVoicing lyr) (kProg kin)
      effectiveVF (chroma, p) =
        if chroma then A.strataModeFlow p else voiceFunc p
      -- Pre-compute voicings at construction time. The 'forced' binding's
      -- WHNF requires walking every inner list spine, which in turn forces
      -- the lazy 'strataModeFlow' \/ 'flow' voicing computation per bar.
      -- This hoists the work from the audio thread (where it would cause
      -- 'skip:' events on first query) to REPL evaluation time.
      allEvents = queryArc progPat (Arc 0 1000)
      uniqueProgs = nub (map value allEvents)
      cache = [ (key, let vs     = effectiveVF (fmap modifier key)
                          sc     = map (map fromIntegral) vs :: [[Note]]
                          nc     = length vs
                          forced = forceAll sc
                      in forced `seq` (sc, nc))
              | key <- uniqueProgs ]
      cacheForced = foldr (\(_, (s, _)) acc -> forceAll s `seq` acc) () cache
      lookupCache key = case lookup key cache of
        Just hit -> hit
        Nothing  -> let vs     = effectiveVF (fmap modifier key)
                        sc     = map (map fromIntegral) vs :: [[Note]]
                        forced = forceAll sc
                    in forced `seq` (sc, length vs)
  in cacheForced `seq` (|* pF "amp" (kDynamic kin)) $
     mask (fmap (\x -> x >= lo && x <= hi) (kSignal kin)) $
       innerJoin $ fmap (\key ->
         arrangeLookup (lookupCache key) chordPat ranged
       ) progPat

-- |Cached onset-join: takes pre-computed (scales, nChords) and pre-built ranged pattern.
arrangeLookup :: ([[Note]], Int)
              -> Pattern Int        -- ^ Chord selection pattern (1-indexed)
              -> Pattern Int        -- ^ Pre-computed range-filtered note pattern
              -> Pattern ValueMap
arrangeLookup (scales, nChords) chordPat ranged
  | nChords == 0 = silence
  | otherwise =
      let chordIdx = fmap (\i -> (i - 1) `mod` nChords) chordPat

          mapped = Pattern (\st ->
            let noteEvs = query ranged st
            in concatMap (\nEv -> case whole nEv of
              Nothing -> []
              Just wArc ->
                let onsetT  = start wArc
                    ci      = lookupChordAt onsetT chordIdx
                    sc      = scales !! (ci `mod` nChords)
                    noteVal = value nEv
                    scLen   = max 1 (length sc)
                    octave  = noteVal `div` scLen
                    idx     = noteVal `mod` scLen
                in [nEv { value = (sc !! idx) + fromIntegral (octave * 12) }]
              ) noteEvs
            ) Nothing Nothing

      in note mapped

-------------------------------------------------------------------------------
-- Arrangement: arrange' (squeeze)
-------------------------------------------------------------------------------

-- |Map notes through chords using squeeze, with kinetics range gating.
--
-- Same kinetics\/modifier pattern as 'arrange', but uses squeeze strategy:
-- each chord slot gets the full input pattern compressed to fit.
arrange' :: (Double, Double)                     -- ^ Kinetics range
         -> IK                                    -- ^ Performance context
         -> (Int, Int)                            -- ^ Degree-index trim for the input patterns (scale degrees, not MIDI; instrument-range clipping happens later via clip)
         -> Layer                                 -- ^ Progression layer (T | S | M)
         -> VoiceFunction                         -- ^ Voice function
         -> (P.Progression -> P.Progression)      -- ^ Progression modifier
         -> [Pattern Int]                         -- ^ Input patterns to harmonize
         -> Pattern ValueMap
arrange' (lo, hi) (kin, chordPat) register lyr voiceFunc modifier pats =
  let -- Pre-compute note range filter ONCE (shared across all innerJoin invocations)
      ranged = voiceRange register (stack pats)
      progPat = fmap (layerForVoicing lyr) (kProg kin)
      effectiveVF (chroma, p) =
        if chroma then A.strataModeFlow p else voiceFunc p
      -- Pre-compute voicings at construction time. See 'arrange' for the
      -- forced\/cacheForced rationale: hoists per-bar voicing computation
      -- from the audio thread to REPL evaluation time.
      allEvents = queryArc progPat (Arc 0 1000)
      uniqueProgs = nub (map value allEvents)
      cache = [ (key, let vs     = effectiveVF (fmap modifier key)
                          sc     = map (map fromIntegral) vs :: [[Note]]
                          nc     = length vs
                          forced = forceAll sc
                      in forced `seq` (sc, nc))
              | key <- uniqueProgs ]
      cacheForced = foldr (\(_, (s, _)) acc -> forceAll s `seq` acc) () cache
      lookupCache key = case lookup key cache of
        Just hit -> hit
        Nothing  -> let vs     = effectiveVF (fmap modifier key)
                        sc     = map (map fromIntegral) vs :: [[Note]]
                        forced = forceAll sc
                    in forced `seq` (sc, length vs)
  in cacheForced `seq` (|* pF "amp" (kDynamic kin)) $
     mask (fmap (\x -> x >= lo && x <= hi) (kSignal kin)) $
       innerJoin $ fmap (\key ->
         arrangeLookup' (lookupCache key) chordPat ranged
       ) progPat

-- |Cached squeeze: takes pre-computed (scales, nChords) and pre-built ranged pattern.
arrangeLookup' :: ([[Note]], Int)
               -> Pattern Int        -- ^ Chord selection pattern (1-indexed)
               -> Pattern Int        -- ^ Pre-computed range-filtered note pattern
               -> Pattern ValueMap
arrangeLookup' (scales, nChords) chordPat ranged
  | nChords == 0 = silence
  | otherwise =
      let chordIdx  = fmap (\i -> (i - 1) `mod` nChords) chordPat
          chordPats = map (\sc -> note (toScale sc ranged)) scales
      in squeeze chordIdx chordPats

-------------------------------------------------------------------------------
-- Parallelism Harmoniser
-------------------------------------------------------------------------------

-- |Stack fixed-interval parallel voices over an arranged ControlPattern.
-- The offset pattern is the FULL voice spec in absolute semitones: each note
-- of @pat@ is replaced by one copy per simultaneous offset, shifted by that
-- offset. Include @0@ to retain the original note; omit it to drop the root.
--
-- Comma = simultaneous voices, space = time-sequenced offsets (standard
-- mininotation, natively evaluated). Applied post-voicing\/post-range-filter,
-- so offsets are not gated by the @arrange@ MIDI range.
--
-- @
-- parallel "0 7"      $ arrange ... -- root + perfect fifth above
-- parallel "7"        $ arrange ... -- fifth only (root dropped)
-- parallel "[0,-5,4]" $ arrange ... -- root, fourth below, major third above
-- @
parallel :: Pattern Note -> ControlPattern -> ControlPattern
parallel offs pat = pat |+ note offs

-------------------------------------------------------------------------------
-- Chord Lookup
-------------------------------------------------------------------------------

-- |Point-query a chord selection pattern at a specific time.
-- Returns the chord index (0-indexed) active at time @t@.
-- Falls back to chord 0 if no events found.
lookupChordAt :: Time -> Pattern Int -> Int
lookupChordAt t cpat =
  case queryArc cpat (Arc t (t + 1/10000000)) of
    []    -> 0
    (e:_) -> value e

-- |Lookup a chord from a progression context by index with modulo wrap.
-- Operates on the triad layer (the harmonic content).
lookupChord :: PC.ProgressionContext -> Int -> H.Chord
lookupChord pc idx =
  let prog = PC.triadLayer pc
      len = P.progLength prog
      chords = P.progChords prog
      wrappedIdx = idx `mod` len
  in chords !! wrappedIdx

-- |Lookup progression (triad layer) as a pattern of voicings via 'A.flow'.
lookupProgression :: PC.ProgressionContext -> Pattern Int -> Pattern [Int]
lookupProgression pc idxPat =
  let prog = PC.triadLayer pc
      len = P.progLength prog
      voicings = A.flow prog
  in fmap (\idx -> voicings !! (idx `mod` len)) idxPat

-------------------------------------------------------------------------------
-- Progression Overlap (Re-exports from Arranger)
-------------------------------------------------------------------------------

-- |Forward overlap: merge pitches from n bars ahead
overlapF :: Int -> P.Progression -> P.Progression
overlapF = A.progOverlapF

-- |Backward overlap: merge pitches from n bars behind
overlapB :: Int -> P.Progression -> P.Progression
overlapB = A.progOverlapB

-- |Bidirectional overlap: merge pitches from n bars in both directions
overlap :: Int -> P.Progression -> P.Progression
overlap = A.progOverlap
