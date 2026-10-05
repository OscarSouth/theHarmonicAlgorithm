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

    -- * Natural harmonics (the composite overtone series)
  , Tuning
  , ecbc
  , composite
  , harmonics
  , overtoneMap
  , overtoneTable
  , compositeSeries
  , stringPitches
  , partialOffset

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
import qualified Harmonic.Rules.Types.Pitch as Pitch
import qualified Harmonic.Interface.Tidal.Arranger as A
import Harmonic.Interface.Tidal.Form (Kinetics(..), IK)
import Harmonic.Interface.Tidal.Utils (mono')

import Data.Foldable (toList)
import Data.List (sort, nub, intercalate)
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
-- always voiced by 'A.strataModeFlow', lattice semantics: pattern index
-- @i@ addresses the i-th slot of a fixed pitch lattice grounded on bar 0
-- and inflected per bar — a held index pedals its pitch across key areas
-- rather than transposing with each bar's root. The T
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
-- 'VoiceFunction' rather than half-claiming lattice semantics.
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
-- Natural harmonics: harmonics (one string)
-------------------------------------------------------------------------------

-- | Open strings of an overtone instrument as (name, scientific octave):
-- @(A,1)@ is A1 = MIDI 33.
type Tuning = [(Pitch.NoteName, Int)]

-- | The instrument behind @hmnx@: the Electric Contrabass Cittern as the MPC
-- harmonics keymap holds it — open strings A2 E3 G3 (thesis §2.1: EAeGB is
-- E2 A2 E3 G3 B3) and the top string in both Hipshot lever positions, B3 and
-- C4. B and C are one physical string,
-- but the keygroup carries both sets of samples, so both are always available
-- to a pattern; the harmonic context is what limits the space. Fixed on
-- purpose: the keymap never changes, so no launcher states a tuning.
ecbc :: Tuning
ecbc = [(Pitch.A, 2), (Pitch.E, 3), (Pitch.G, 3), (Pitch.B, 3), (Pitch.C, 4)]

-- | Semitones from a string's fundamental to its @n@-th partial:
-- @round (12 * logBase 2 n)@, so partials 2..5 sit at +12 +19 +24 +28.
partialOffset :: Int -> Int
partialOffset partial = round (12 * logBase 2 (fromIntegral partial :: Double))

-- | The four playable partials of the 2016 thesis — octave, fifth, double
-- octave, third (partials 2 3 4 5) — of one open string, as MIDI pitches.
stringPitches :: (Pitch.NoteName, Int) -> [Int]
stringPitches (nm, octv) =
  let fund = 12 * (octv + 1) + Pitch.unPitchClass (Pitch.pitchClass nm)
  in [ fund + partialOffset partial | partial <- [2, 3, 4, 5] ]

-- | The composite overtone series (thesis §2.2): every pitch any string of a
-- tuning can sound as a natural harmonic, ascending, once each.
-- @compositeSeries ecbc@ is the keymap:
-- @57 64 67 69 71 72 73 74 76 78 79 80 83 84 87 88@.
compositeSeries :: Tuning -> [Int]
compositeSeries = sort . nub . concatMap stringPitches

-- The instrument's series filtered to the harmony: per bar, the pitches whose
-- class the layer voices. The pitch classes are read through the same route
-- 'arrange' uses to see a bar's tones; no voice function is exposed because
-- flow and grid voice the same classes.
viableSeries :: Layer -> PC.ProgressionContext -> ([[Int]], Int)
viableSeries lyr ctx =
  let (chroma, p) = layerForVoicing lyr ctx
      vs  = if chroma then A.strataModeFlow p else A.flow p
      ser = compositeSeries ecbc
      per = [ [ m | m <- ser, (m `mod` 12) `elem` map (`mod` 12) bar ] | bar <- vs ]
  in (per, length vs)

-- | The whole overtone instrument. Pattern ints index the bar's viable
-- composite series — the harmonics of 'ecbc' whose pitch class the layer
-- voices — compressed to @0, 1, 2 …@ ascending and looped by floor-mod, so
-- @"[0,1,2]"@ is the lowest three available tones from the bottom up and
-- @"[-1,-2,-3]"@ the highest three from the top down, whatever the size of the
-- structure in play. A pitch class present in two octaves is two ints. Never an
-- octave wrap: these are fixed sample pitches. A bar whose voicing has no
-- instrument tone rests. Polyphonic; 'harmonics' is the per-string filter over
-- the same mapping. The harmonic context is the only limit on the space:
-- generating inside the instrument's palette (@hcOvertones@) keeps every tone
-- reachable, and 'overtoneMap' shows what each int hits.
--
-- @, composite (0,1) k T ["~", "[0,1,2]/4"] # ch 11@
composite :: (Double, Double)                     -- ^ Kinetics gate, as 'arrange'
          -> IK                                   -- ^ Performance context
          -> Layer                                -- ^ Harmonic state: T (chord tones) or S (pentatonic)
          -> [Pattern Int]                        -- ^ Contours: ints into the bar's viable series
          -> Pattern ValueMap
composite = seriesPlay (const True) False

-- | One string of 'ecbc', named by its open note (A E G B C; enharmonics
-- collapse): the SAME int → pitch mapping as 'composite', but only the pitches
-- that string can sound pass — the rest are silent, not remapped — so a string
-- block isolates or mutes a string and owns its sustain. Within the string the
-- result is 'mono'' (latest-note priority: a new harmonic damps the previous,
-- as on the instrument).
--
-- @, harmonics A (0,1) k T ["~", pat] # ch 11@
harmonics :: Pitch.NoteName -> (Double, Double) -> IK -> Layer -> [Pattern Int] -> Pattern ValueMap
harmonics str = seriesPlay (`elem` owned) True
  where
    owned = concat [ stringPitches st | st@(nm, _) <- ecbc
                   , Pitch.pitchClass nm == Pitch.pitchClass str ]

-- Shared engine of 'composite' and 'harmonics': the 'arrange' cache shape over
-- 'kProgs', an onset-join of the contour against the bar's viable series, a
-- keep-predicate on the resulting pitch, and optional monophony after it.
seriesPlay :: (Int -> Bool) -> Bool -> (Double, Double) -> IK -> Layer -> [Pattern Int] -> Pattern ValueMap
seriesPlay keep monoString (lo, hi) (kin, chordPat) lyr pats =
  let contour = stack pats
      progPat = kProg kin
      cache = [ (ctx, viableSeries lyr ctx) | ctx <- kProgs kin ]
      cacheForced = foldr (\(_, (vs, cnt)) acc -> sum (concat vs) `seq` cnt `seq` acc) () cache
      lookupCache ctx = case lookup ctx cache of
        Just hit -> hit
        Nothing  -> viableSeries lyr ctx
      voicing = if monoString then mono' else id
  in cacheForced `seq` (|* pF "amp" (kDynamic kin)) $
     mask (fmap (\x -> x >= lo && x <= hi) (kSignal kin)) $
       innerJoin $ fmap (\ctx ->
         midinote (fmap fromIntegral (voicing (seriesLookup (lookupCache ctx) keep chordPat contour)))
       ) progPat

-- | Onset-join of a contour against per-bar viable series: floor-mod index
-- (negative = from the top), pitch sampled once at the onset, kept only if
-- the predicate admits it.
seriesLookup :: ([[Int]], Int) -> (Int -> Bool) -> Pattern Int -> Pattern Int -> Pattern Int
seriesLookup (viable, nChords) keep chordPat contour
  | nChords == 0 = silence
  | otherwise =
      let chordIdx = fmap (\i -> (i - 1) `mod` nChords) chordPat
      in Pattern (\st ->
           concatMap (\ev -> case whole ev of
             Nothing -> []
             Just w  ->
               let ci = lookupChordAt (start w) chordIdx
                   vs = viable !! (ci `mod` nChords)
               in [ ev { value = m } | not (null vs)
                                     , let m = vs !! (value ev `mod` length vs)
                                     , keep m ]
             ) (query contour st)
           ) Nothing Nothing

-- | The basic series over a set of voiced pitch classes, on 'ecbc': @(int,
-- MIDI pitch, sources)@, a source being @(string, overtone number)@ in the
-- thesis's numbering — OT1 the fundamental's class (partials 2 and 4), OT2 the
-- fifth, OT3 the third. Pure core of 'overtoneMap'.
overtoneTable :: [Int] -> [(Int, Int, [(Pitch.NoteName, Int)])]
overtoneTable pcs =
  let viable = [ m | m <- compositeSeries ecbc, (m `mod` 12) `elem` map (`mod` 12) pcs ]
  in [ (i, m, sourcesOf m) | (i, m) <- zip [0 ..] viable ]
  where
    sourcesOf m = [ (nm, otNumber partial)
                  | st@(nm, _) <- ecbc
                  , (partial, pitch) <- zip [2, 3, 4, 5 :: Int] (stringPitches st)
                  , pitch == m ]
    otNumber partial = case partial of { 2 -> 1; 3 -> 2; 4 -> 1; 5 -> 3; other -> other }

-- | Print, per chord of a progression, what the ints of the basic series map
-- to on the instrument — pitch, MIDI number and sources in thesis notation
-- (@E2@ = string E, overtone 2; @\/@ = alternative sources) — with the
-- top-down alias beside each int; then the distinct ints of the contour over
-- its first bar, resolved per chord. Takes the progression context and the
-- layer the block plays, so what it prints is what 'composite' sounds.
--
-- @overtoneMap s T ["[0,1,2]/4"]@
overtoneMap :: PC.ProgressionContext -> Layer -> [Pattern Int] -> IO ()
overtoneMap ctx lyr pats = do
  putStrLn "pitch names are scientific (60 = C4); the MPC and MIDI Monitor show one octave lower — compare by number"
  let ints = nub [ value e | e <- queryArc (stack pats) (Arc 0 4), eventHasOnset e ]
      pcName pc = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"] !! (pc `mod` 12)
      pitchName m = pcName m ++ show (m `div` 12 - 1)
      srcText srcs = intercalate " / " [ show nm ++ show ot | (nm, ot) <- srcs ]
      (chroma, p) = layerForVoicing lyr ctx
      bars = if chroma then A.strataModeFlow p else A.flow p
  mapM_ (\(bi, bar) -> do
      let pcs  = nub (map (`mod` 12) bar)
          rows = overtoneTable pcs
          cnt  = length rows
      putStrLn ("chord " ++ show (bi :: Int) ++ "  [" ++ unwords (map pcName pcs) ++ "]"
                ++ (if cnt == 0 then "  — no instrument tone" else ""))
      mapM_ (\(i, m, srcs) ->
          putStrLn ("  " ++ show i ++ " / " ++ show (i - cnt) ++ "\t" ++ pitchName m
                    ++ "\t" ++ show m ++ "\t" ++ srcText srcs)) rows
      if null ints || cnt == 0 then return () else
        putStrLn ("  contour " ++ unwords (map show ints) ++ "  ->  "
                  ++ unwords [ pitchName (vs !! (i `mod` cnt)) | let vs = [ m | (_, m, _) <- rows ], i <- ints ])
    ) (zip [1 ..] bars)

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
