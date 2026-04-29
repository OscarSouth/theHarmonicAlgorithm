{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Harmonic.Interface.Tidal.BridgeSpec
-- Description : Tests for TidalCycles interface patterns
--
-- Validates the Bridge module:
--   * Modulo wrap behavior for pattern indices
--   * Voice extraction functions (flow)
--   * Pattern-based lookup functions
--   * Patterned chord selection (arrange, arrange')
--   * Chord selection helpers (warp, rep, lookupChordAt)

module Harmonic.Interface.Tidal.BridgeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Harmonic.Rules.Types.Progression as P
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as Pitch
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import Harmonic.Rules.Types.ProgressionContext (Layer(..))
import Harmonic.Interface.Tidal.Bridge
import Harmonic.Interface.Tidal.Form (Kinetics(..), IK)
import qualified Harmonic.Interface.Tidal.Arranger as A
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.List (nub, sort)
import Sound.Tidal.Context

-------------------------------------------------------------------------------
-- Test Fixtures (Using Phase B types)
-------------------------------------------------------------------------------

-- |Helper: create a CadenceState for testing
-- Intervals should be in zero-form (first pitch = 0)
-- e.g., major = [0,4,7], minor = [0,3,7]
mkCadenceState :: Pitch.NoteName -> String -> [Integer] -> H.CadenceState
mkCadenceState rootNote func intervals =
  let pcs = map (Pitch.mkPitchClass . fromIntegral) (take 3 intervals)
      cadence = H.Cadence func H.Unison pcs
  in H.CadenceState cadence rootNote H.FlatSpelling

-- |Create a simple test progression with known chords
-- C major -> G major -> F major -> C major (4 chords)
-- Using zero-form intervals: major = [0,4,7]
testProgression :: P.Progression
testProgression =
  P.Progression $ Seq.fromList
    [ mkCadenceState Pitch.C "maj" [0, 4, 7]    -- C major (C=0, E=4, G=7)
    , mkCadenceState Pitch.G "maj" [0, 4, 7]    -- G major (G=0, B=4, D=7 -> 7,11,2 after transpose)
    , mkCadenceState Pitch.F "maj" [0, 4, 7]    -- F major (F=0, A=4, C=7 -> 5,9,0 after transpose)
    , mkCadenceState Pitch.C "maj" [0, 4, 7]    -- C major
    ]

-- |Longer test progression (8 chords) for modulo tests
longerProgression :: P.Progression
longerProgression =
  P.Progression $ Seq.fromList
    [ mkCadenceState Pitch.C "maj" [0, 4, 7]
    , mkCadenceState Pitch.D "min" [0, 3, 7]
    , mkCadenceState Pitch.E "min" [0, 3, 7]
    , mkCadenceState Pitch.F "maj" [0, 4, 7]
    , mkCadenceState Pitch.G "maj" [0, 4, 7]
    , mkCadenceState Pitch.A "min" [0, 3, 7]
    , mkCadenceState Pitch.B "dim" [0, 3, 6]
    , mkCadenceState Pitch.C "maj" [0, 4, 7]
    ]

-- |Create a pass-through Kinetics for testing: kSignal=1, kDynamic=1, constant progression
testKinetics :: P.Progression -> Kinetics
testKinetics prog = Kinetics (pure 1.0) (pure 1.0) (pure (PC.fromProgression prog))

-------------------------------------------------------------------------------
-- Helpers: extract onset events from ControlPattern
-------------------------------------------------------------------------------

-- |Extract (onset-time, note-value) pairs from a ControlPattern over an Arc.
queryOnsets :: ControlPattern -> Arc -> [(Rational, Double)]
queryOnsets pat arc =
  [ (start (part ev), noteVal)
  | ev <- queryArc pat arc
  , isOnset ev
  , Just noteVal <- [extractNote (value ev)]
  ]
  where
    isOnset ev = case whole ev of
      Just w  -> start w == start (part ev)
      Nothing -> False
    extractNote vm = case Map.lookup "note" vm of
      Just (VN n) -> Just (unNote n)
      Just (VF v) -> Just v
      _           -> Nothing

-- |Count onset events in a time range.
onsetCount :: ControlPattern -> Arc -> Int
onsetCount pat arc = length (queryOnsets pat arc)

-- |Extract just the note values (Doubles) from onsets.
onsetNotes :: ControlPattern -> Arc -> [Double]
onsetNotes pat arc = map snd (queryOnsets pat arc)

-- |Count all events (including continuations) with note values.
eventCount :: ControlPattern -> Arc -> Int
eventCount pat arc = length
  [ ()
  | ev <- queryArc pat arc
  , Just _ <- [extractNoteVal (value ev)]
  ]
  where
    extractNoteVal vm = case Map.lookup "note" vm of
      Just (VN n) -> Just (unNote n)
      Just (VF v) -> Just v
      _           -> Nothing

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Voice Functions" $ do

    describe "flow (full harmony, voice-led)" $ do
      it "returns one list per chord" $ do
        let voicings = A.flow testProgression
        length voicings `shouldBe` 4

      it "each voicing contains all pitch classes" $ do
        let voicings = A.flow testProgression
            firstVoicing = head voicings
        -- C major has 3 notes
        length firstVoicing `shouldBe` 3

  describe "progLength" $ do

    it "returns correct length for 4-chord progression" $ do
      P.progLength testProgression `shouldBe` 4

    it "returns correct length for 8-chord progression" $ do
      P.progLength longerProgression `shouldBe` 8

  describe "lookupChord" $ do

    it "returns correct chord at index 0" $ do
      let chord = lookupChord (PC.fromProgression testProgression) 0
          pitches = H.chordIntervals chord
      pitches `shouldBe` [0, 4, 7]

    it "returns correct chord at index 2" $ do
      let chord = lookupChord (PC.fromProgression testProgression) 2
          pitches = H.chordIntervals chord
      pitches `shouldBe` [5, 9, 0]

    describe "Modulo wrap behavior" $ do
      it "index 4 wraps to index 0 (4 mod 4 = 0)" $ do
        let chord0 = lookupChord (PC.fromProgression testProgression) 0
            chord4 = lookupChord (PC.fromProgression testProgression) 4
        chord0 `shouldBe` chord4

      it "index 6 wraps to index 2 (6 mod 4 = 2)" $ do
        let chord2 = lookupChord (PC.fromProgression testProgression) 2
            chord6 = lookupChord (PC.fromProgression testProgression) 6
        chord2 `shouldBe` chord6

      it "negative indices wrap correctly" $ do
        let chord3 = lookupChord (PC.fromProgression testProgression) 3
            chordNeg1 = lookupChord (PC.fromProgression testProgression) (-1)
        chord3 `shouldBe` chordNeg1

      it "large indices wrap correctly" $ do
        let chord0 = lookupChord (PC.fromProgression testProgression) 0
            chord100 = lookupChord (PC.fromProgression testProgression) 100
        chord0 `shouldBe` chord100

  describe "extract" $ do

    it "returns CadenceState directly (no Maybe wrapper)" $ do
      A.extract 1 (PC.fromProgression testProgression) `seq` True `shouldBe` True

    it "index 1 returns first chord" $ do
      let cs1 = A.extract 1 (PC.fromProgression testProgression)
          cs5 = A.extract 5 (PC.fromProgression testProgression)
      cs1 `shouldBe` cs5

    it "wraps around modulo progression length" $ do
      let cs2 = A.extract 2 (PC.fromProgression testProgression)
          cs6 = A.extract 6 (PC.fromProgression testProgression)
      cs2 `shouldBe` cs6

  describe "arrange (onset-join)" $ do

    it "produces events from a simple chord pattern" $ do
      let chordSel = parseBP_E "[1 2 3 4]/4" :: Pattern Int
          result = arrange (0,1) (testKinetics testProgression, chordSel) (-24,24) T A.flow id
                     [parseBP_E "[0 1 2 3]"]
      onsetCount result (Arc 0 1) `shouldSatisfy` (> 0)

    it "returns silence for empty progression" $ do
      let emptyProg = P.Progression Seq.empty
          chordSel = parseBP_E "1" :: Pattern Int
          result = arrange (0,1) (testKinetics emptyProg, chordSel) (-24,24) T A.flow id
                     [parseBP_E "[0 1 2]"]
      onsetCount result (Arc 0 1) `shouldBe` 0

    it "full pattern plays for every chord (no alternation)" $ do
      let chordSel = parseBP_E "[1 2]/2" :: Pattern Int
          pat = parseBP_E "[0 1 2 3]" :: Pattern Int
          result = arrange (0,1) (testKinetics testProgression, chordSel) (-24,24) T A.flow id [pat]
          cycle0 = onsetNotes result (Arc 0 1)
          cycle1 = onsetNotes result (Arc 1 2)
      length cycle0 `shouldBe` length cycle1

    it "sustained note does not re-trigger at chord boundary" $ do
      let chordSel = parseBP_E "[1 2]" :: Pattern Int
          pat = parseBP_E "0" :: Pattern Int
          result = arrange (0,1) (testKinetics testProgression, chordSel) (-24,24) T A.flow id [pat]
      onsetCount result (Arc 0 1) `shouldBe` 1

    it "events fall within expected cycle range" $ do
      let chordSel = parseBP_E "[1 2 3 4]/4" :: Pattern Int
          result = arrange (0,1) (testKinetics testProgression, chordSel) (-24,24) T A.flow id
                     [parseBP_E "[0 1 2 3]"]
          onsets = queryOnsets result (Arc 0 1)
      all (\(t, _) -> t >= 0 && t < 1) onsets `shouldBe` True

    it "AABA form [1 1 2 1]/4 produces correct chord repetition" $ do
      let chordSel = parseBP_E "[1 1 2 1]/4" :: Pattern Int
          result = arrange (0,1) (testKinetics testProgression, chordSel) (-24,24) T A.flow id
                     [parseBP_E "0"]
          onsets = onsetNotes result (Arc 0 4)
      length onsets `shouldBe` 4
      (onsets !! 0) `shouldBe` (onsets !! 1)
      (onsets !! 0) `shouldBe` (onsets !! 3)

    it "extends into higher octaves like toScale (index >= scale length)" $ do
      let chordSel = parseBP_E "1" :: Pattern Int
          result = arrange (0,1) (testKinetics testProgression, chordSel) (-24,24) T A.flow id
                     [parseBP_E "[0 1 2 3]"]
          notes = onsetNotes result (Arc 0 1)
      length notes `shouldBe` 4
      (notes !! 3) `shouldSatisfy` (> (notes !! 2))

    it "chord indices wrap modulo progression length" $ do
      let chordSel = parseBP_E "[5 6 7 8]/4" :: Pattern Int
          result1 = arrange (0,1) (testKinetics testProgression, chordSel) (-24,24) T A.flow id
                      [parseBP_E "0"]
          chordSel2 = parseBP_E "[1 2 3 4]/4" :: Pattern Int
          result2 = arrange (0,1) (testKinetics testProgression, chordSel2) (-24,24) T A.flow id
                      [parseBP_E "0"]
      onsetNotes result1 (Arc 0 4) `shouldBe` onsetNotes result2 (Arc 0 4)

  describe "arrange' (squeeze)" $ do

    it "produces events from a simple chord pattern" $ do
      let chordSel = parseBP_E "[1 2 3 4]/4" :: Pattern Int
          result = arrange' (0,1) (testKinetics testProgression, chordSel) (-24,24) T A.flow id
                     [parseBP_E "[0 1 2 3]"]
      onsetCount result (Arc 0 1) `shouldSatisfy` (> 0)

    it "each chord slot gets the full pattern" $ do
      let chordSel = parseBP_E "[1 2]/2" :: Pattern Int
          pat = parseBP_E "[0 1 2 3]" :: Pattern Int
          result = arrange' (0,1) (testKinetics testProgression, chordSel) (-24,24) T A.flow id [pat]
          cycle0 = onsetNotes result (Arc 0 1)
          cycle1 = onsetNotes result (Arc 1 2)
      length cycle0 `shouldBe` length cycle1

    it "returns silence for empty progression" $ do
      let emptyProg = P.Progression Seq.empty
          chordSel = parseBP_E "1" :: Pattern Int
          result = arrange' (0,1) (testKinetics emptyProg, chordSel) (-24,24) T A.flow id
                     [parseBP_E "[0 1 2]"]
      onsetCount result (Arc 0 1) `shouldBe` 0

  describe "strataModeFlow (key-signature voicing for non-triad layers)" $ do

    -- Synthetic 5-PC strata fixture: each bar has 5 cadence intervals.
    -- Bar roots / chroma chosen to mimic genP output (strata II → strata X
    -- → strata VI walking through D-major-related strata).
    let mk5 rootNote ints =
          let pcs = map (Pitch.mkPitchClass . fromIntegral) (ints :: [Int])
          in H.CadenceState (H.Cadence "" H.Unison pcs) rootNote H.FlatSpelling
        strataFixture = P.Progression $ Seq.fromList
          [ mk5 Pitch.D  [0, 2, 4, 7, 11]   -- strata II rooted on D
          , mk5 Pitch.D  [0, 4, 5, 8, 11]   -- strata X rooted on D
          , mk5 Pitch.D  [0, 1, 2, 5, 9]    -- strata VI rooted on D
          , mk5 Pitch.D  [0, 2, 4, 7, 11]   -- back to strata II
          ]
        modeFixture = P.Progression $ Seq.fromList
          [ mk5 Pitch.D  [0, 2, 4, 5, 7, 9, 11]   -- D Major mode (7 PCs)
          , mk5 Pitch.D  [0, 1, 4, 5, 7, 9, 10]   -- D Harm Major
          , mk5 Pitch.D  [0, 2, 4, 5, 7, 9, 11]
          , mk5 Pitch.D  [0, 1, 4, 5, 7, 9, 10]
          ]

    it "produces 5 voices per bar for a 5-PC strata progression" $ do
      let voicings = A.strataModeFlow strataFixture
      length voicings `shouldBe` 4
      mapM_ (\v -> length v `shouldBe` 5) voicings

    it "produces 7 voices per bar for a 7-PC mode progression" $ do
      let voicings = A.strataModeFlow modeFixture
      length voicings `shouldBe` 4
      mapM_ (\v -> length v `shouldBe` 7) voicings

    it "every bar has all-distinct MIDIs (no duplicate voices, bijective assignment)" $ do
      let strataVs = A.strataModeFlow strataFixture
          modeVs   = A.strataModeFlow modeFixture
          allDistinct v = length v == length (nub v)
      mapM_ (\v -> allDistinct v `shouldBe` True) strataVs
      mapM_ (\v -> allDistinct v `shouldBe` True) modeVs

    it "every PC of the strata is reachable via pattern indices [0..4]" $ do
      let strataVs = A.strataModeFlow strataFixture
          -- For each bar, the 5 voicing slots cover 5 distinct PCs (mod 12)
          -- which equals the cardinality of the bar's chroma.
          pcsPerBar = map (nub . map (`mod` 12)) strataVs
      mapM_ (\pcs -> length pcs `shouldBe` 5) pcsPerBar

    it "every PC of the mode is reachable via pattern indices [0..6]" $ do
      let modeVs = A.strataModeFlow modeFixture
          pcsPerBar = map (nub . map (`mod` 12)) modeVs
      mapM_ (\pcs -> length pcs `shouldBe` 7) pcsPerBar

    it "every bar's voicing is sorted ascending (pattern increment ⇔ pitch ascend)" $ do
      let strataVs = A.strataModeFlow strataFixture
          modeVs   = A.strataModeFlow modeFixture
          ascending v = v == sort v
      mapM_ (\v -> ascending v `shouldBe` True) strataVs
      mapM_ (\v -> ascending v `shouldBe` True) modeVs

    it "voice 0 max step is bounded by 6 semitones across bar transitions" $ do
      let voicings = A.strataModeFlow strataFixture
          v0s = map head voicings
          steps = zipWith (\a b -> abs (b - a)) v0s (tail v0s)
      all (<= 6) steps `shouldBe` True

    it "every voice's max step is bounded by 6 semitones (key-signature smoothness)" $ do
      let voicings = A.strataModeFlow strataFixture
          n = length (head voicings)
          stepsPerVoice = [ [ abs (voicings!!(i+1)!!v - voicings!!i!!v)
                            | i <- [0 .. length voicings - 2] ]
                          | v <- [0 .. n - 1] ]
      all (all (<= 6)) stepsPerVoice `shouldBe` True

    it "bar 0 matches legacy flow on the same non-triad input (initialCompact reuse)" $ do
      let smf = A.strataModeFlow strataFixture
          fl  = A.flow strataFixture
      head smf `shouldBe` head fl

    it "arrange ... S A.flow ... and arrange ... S A.lite ... yield identical events over genP-style 5-PC layer" $ do
      let chordSel = parseBP_E "[1 2 3 4]/4" :: Pattern Int
          kin = Kinetics (pure 1.0) (pure 1.0)
                  (pure (PC.ProgressionContext strataFixture strataFixture strataFixture
                          (Just Seq.empty)))
          aFlow = arrange (0,1) (kin, chordSel) (-24,24) S A.flow id [parseBP_E "[0 1 2 3 4]"]
          aLite = arrange (0,1) (kin, chordSel) (-24,24) S A.lite id [parseBP_E "[0 1 2 3 4]"]
      onsetNotes aFlow (Arc 0 1) `shouldBe` onsetNotes aLite (Arc 0 1)

    it "arrange ... T ... over a genP-style ProgressionContext is unaffected by the override" $ do
      let chordSel = parseBP_E "[1 2 3 4]/4" :: Pattern Int
          tProg = testProgression  -- 3-PC triads
          kin = Kinetics (pure 1.0) (pure 1.0)
                  (pure (PC.ProgressionContext tProg strataFixture modeFixture
                          (Just Seq.empty)))
          aT  = arrange (0,1) (kin, chordSel) (-24,24) T A.flow id [parseBP_E "[0 1 2]"]
          aT' = arrange (0,1) (testKinetics tProg, chordSel) (-24,24) T A.flow id [parseBP_E "[0 1 2]"]
      onsetNotes aT (Arc 0 1) `shouldBe` onsetNotes aT' (Arc 0 1)

    it "S layer with 3-PC duplicated triad (gen origin) falls through to user's voiceFunc" $ do
      -- gen-origin: PC.fromProgression duplicates triad layer across S/M.
      -- So arrange ... S flow ... should produce same events as arrange ... T flow ...
      -- (both use user's flow on the same 3-PC Progression).
      let chordSel = parseBP_E "[1 2 3 4]/4" :: Pattern Int
          kin = testKinetics testProgression  -- gen-style: all layers identical
          aS  = arrange (0,1) (kin, chordSel) (-24,24) S A.flow id [parseBP_E "[0 1 2]"]
          aT  = arrange (0,1) (kin, chordSel) (-24,24) T A.flow id [parseBP_E "[0 1 2]"]
      onsetNotes aS (Arc 0 1) `shouldBe` onsetNotes aT (Arc 0 1)

  describe "lookupChordAt" $ do

    it "returns chord index at a given time" $ do
      let chordPat = parseBP_E "[0 1 2 3]" :: Pattern Int
      lookupChordAt 0 chordPat `shouldBe` 0
      lookupChordAt (3/8) chordPat `shouldBe` 1
      lookupChordAt (5/8) chordPat `shouldBe` 2
      lookupChordAt (7/8) chordPat `shouldBe` 3

    it "returns 0 as default for empty pattern" $ do
      lookupChordAt 0 silence `shouldBe` 0

  describe "warp" $ do

    it "parses a mininotation string into a Pattern Int" $ do
      let r = warp "[1 2 3 4]/4"
      lookupChordAt 2 r `shouldBe` 1
      lookupChordAt 6 r `shouldBe` 2

  describe "rep" $ do

    it "generates sequential chord selection from progression length" $ do
      let r = rep (PC.fromProgression testProgression) 1
      lookupChordAt 2 r `shouldBe` 1
      lookupChordAt 6 r `shouldBe` 2
      lookupChordAt 10 r `shouldBe` 3
      lookupChordAt 14 r `shouldBe` 4
