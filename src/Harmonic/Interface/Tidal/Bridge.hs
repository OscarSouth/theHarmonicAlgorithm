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

import Data.Foldable (toList)
import Data.Maybe (isJust)
import Sound.Tidal.Context hiding (voice)

-------------------------------------------------------------------------------
-- Voice Function Types
-------------------------------------------------------------------------------

-- |Voice function type: extracts integer pitch sequences from progression
type VoiceFunction = P.Progression -> [[Int]]

-- |Filter pattern events by DEGREE-INDEX range — the raw @Pattern Int@
-- values before the degree→pitch mapping, NOT MIDI notes. Instrument
-- range clipping in MIDI space happens later, via
-- 'Harmonic.Interface.Tidal.Orchestra.clip'.
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
warp str = slow 4 $ parseBP_E str

-- |Generate a sequential chord selection pattern from a progression.
-- Auto-derives length from the progression. Timing is bar-relative.
--
-- @
-- let r = rep s4 1     -- 4 chords over 4 bars (1 bar each)
-- let r = rep s4 0.5   -- 4 chords over 2 bars (half bar each)
-- @
rep :: PC.ProgressionContext -> Pattern Time -> Pattern Int
rep pc repVal =
  let barsN = PC.pcLength pc
  in slow (fromIntegral barsN * repVal * 4) $ fastcat $ map pure [1..barsN]

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
-- route. Chroma routing: the S\/M layers of a genP-provenance context are
-- THE curated 5\/7-PC chroma, and the S\/M layers of a chordscale-derived
-- gen \/ genJ context are the analysis pentatonic \/ mode chroma — both are
-- always voiced by 'A.strataModeFlow', degree\/"key-signature" semantics:
-- pattern index @i@ plays the i-th scale degree of that bar's set. The T
-- layer always honours the user's 'VoiceFunction', as do the S\/M layers
-- of genE contexts (independent partner triads — ordinary harmony, so
-- never chroma-routed) and of contexts whose layers are still literal
-- duplicates (hand-built material without
-- 'Harmonic.Evaluation.Analysis.KeyArea.chordscale').
--
-- Routing by provenance \/ derived-chroma detection replaces the old
-- first-bar cardinality sniff (@isOctaSM@), which mis-routed
-- mixed-cardinality material in both directions (triad-first-bar chroma
-- escaped to the DP; 4-note-first-bar harmony was captured by the chroma
-- engine). Derived detection requires BOTH a distinct mode layer AND
-- every mode bar at chroma cardinality (≥5) — so a subst-downgraded genE
-- context (distinct but triadic layers) and a bar-substituted derived
-- context (mixed cardinality after the edit) both fall back to the user's
-- 'VoiceFunction' rather than half-claiming degree semantics.
-- A genE context is excluded whole, combination selectors included. Its
-- @S@\/@M@ are partner triads, and its @SM@\/@TSM@ unions are polytonal
-- SONORITIES — three stacked triads, five tones — not scale forms, so the
-- cyclic DP is the right tool for them and 'A.hasBigChroma' correctly
-- declines to reroute a uniform 5-PC progression. Deliberate, not a gap.
layerForVoicing :: Layer -> PC.ProgressionContext -> (Bool, P.Progression)
layerForVoicing lyr ctx =
  let chromaBar cs = length (H.cadenceIntervals (H.stateCadence cs)) >= 5
      derived = PC.pcFamily ctx /= PC.FPoly
                && PC.modeLayer ctx /= PC.triadLayer ctx
                && all chromaBar (toList (P.unProgression (PC.modeLayer ctx)))
      chroma  = lyr /= T && (isJust (PC.pcProvenance ctx) || derived)
  in (chroma, PC.layer lyr ctx)

-- | Render scale-degree patterns into a playable 'ControlPattern', reading
-- pitches from the progression under the given voicing strategy.
--
-- The workhorse of the Tidal interface: every orchestral instrument in
-- "Harmonic.Interface.Tidal.Orchestra" is a thin wrapper around it.
--
-- @d1 $ arrange (0,1) k (-9,9) T flow id [\"0 1 2 3\"]@
arrange :: (Double, Double)                     -- ^ Kinetics gate: events pass only while 'kSignal' sits inside @(lo, hi)@ — the same predicate as 'Harmonic.Interface.Tidal.Form.ki', applied here so every instrument line carries its own activation band
        -> IK                                    -- ^ Performance context (kinetics + chord selection)
        -> (Int, Int)                            -- ^ Degree-index trim for the input patterns (scale degrees, not MIDI; instrument-range clipping happens later via clip)
        -> Layer                                 -- ^ Progression layer to voice — single (T | S | M) or synthesized combination (TS | TM | SM | TSM | PT)
        -> VoiceFunction                         -- ^ Voice function (flow, root, etc.)
        -> (P.Progression -> P.Progression)      -- ^ Progression modifier (overlapF 0, id, etc.)
        -> [Pattern Int]                         -- ^ Input patterns to harmonize
        -> Pattern ValueMap
arrange (lo, hi) (kin, chordPat) register lyr voiceFunc modifier pats =
  let -- Pre-compute note range filter ONCE (shared across all innerJoin invocations)
      ranged = voiceRange register (stack pats)
      -- The pattern carries the raw context; projection (layer synthesis
      -- for TS\/TM\/SM\/TSM\/PT), the modifier and the voicing all run at
      -- cache build. The audio thread only equality-matches the context —
      -- combination selectors never synthesize per query.
      progPat = kProg kin
      effectiveVF (chroma, p) =
        if chroma then A.strataModeFlow p else voiceFunc p
      voicingsOf ctx =
        let vs = effectiveVF (fmap modifier (layerForVoicing lyr ctx))
            sc = map (map fromIntegral) vs :: [[Note]]
        in forceAll sc `seq` (sc, length vs)
      -- Pre-compute voicings at construction time. The 'forceAll' walks
      -- every inner list spine, forcing the lazy voicing computation per
      -- bar — hoisting the work from the audio thread (where it would
      -- cause 'skip:' events on first query) to REPL evaluation time.
      -- Exact cache domain: the form's own distinct contexts (kProgs is
      -- already nub'd at form build). No time-window sampling (the old
      -- @queryArc … (Arc 0 1000)@ allocated 1000 events per instrument
      -- and silently missed progressions past the horizon, forcing a
      -- full voice-leading solve on the audio thread mid-set).
      cache = [ (ctx, voicingsOf ctx) | ctx <- kProgs kin ]
      cacheForced = foldr (\(_, (scs, _)) acc -> forceAll scs `seq` acc) () cache
      lookupCache ctx = case lookup ctx cache of
        Just hit -> hit
        Nothing  -> voicingsOf ctx
  in cacheForced `seq` (|* pF "amp" (kDynamic kin)) $
     mask (fmap (\x -> x >= lo && x <= hi) (kSignal kin)) $
       innerJoin $ fmap (\ctx ->
         arrangeLookup (lookupCache ctx) chordPat ranged
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
                    scLen   = length sc
                    octv    = noteVal `div` max 1 scLen
                    idx     = noteVal `mod` max 1 scLen
                -- A bar with no pitch content (e.g. a typo'd empty chord
                -- in a hand-written prog) emits nothing rather than
                -- indexing [] on the audio thread.
                in [ nEv { value = (sc !! idx) + fromIntegral (octv * 12) }
                   | scLen > 0 ]
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
      -- Context-keyed cache; see 'arrange' — projection, modifier and
      -- voicing all run at build, never per query.
      progPat = kProg kin
      effectiveVF (chroma, p) =
        if chroma then A.strataModeFlow p else voiceFunc p
      voicingsOf ctx =
        let vs = effectiveVF (fmap modifier (layerForVoicing lyr ctx))
            sc = map (map fromIntegral) vs :: [[Note]]
        in forceAll sc `seq` (sc, length vs)
      cache = [ (ctx, voicingsOf ctx) | ctx <- kProgs kin ]
      cacheForced = foldr (\(_, (scs, _)) acc -> forceAll scs `seq` acc) () cache
      lookupCache ctx = case lookup ctx cache of
        Just hit -> hit
        Nothing  -> voicingsOf ctx
  in cacheForced `seq` (|* pF "amp" (kDynamic kin)) $
     mask (fmap (\x -> x >= lo && x <= hi) (kSignal kin)) $
       innerJoin $ fmap (\ctx ->
         arrangeLookup' (lookupCache ctx) chordPat ranged
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
  in if len == 0
       then error "lookupChord: empty progression"
       else chords !! wrappedIdx

-- |Lookup progression (triad layer) as a pattern of voicings via 'A.flow'.
lookupProgression :: PC.ProgressionContext -> Pattern Int -> Pattern [Int]
lookupProgression pc idxPat =
  let prog = PC.triadLayer pc
      len = P.progLength prog
      voicings = A.flow prog
  in if len == 0
       then silence
       else fmap (\idx -> voicings !! (idx `mod` len)) idxPat

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
