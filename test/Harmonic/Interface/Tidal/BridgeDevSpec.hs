{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Interface.Tidal.BridgeDevSpec
-- Description : Tests for patterned chord selection (BridgeDev)
--
-- Validates the three arrangement variants against the three limitations
-- of the legacy arrange/applyProg approach:
--   1. Pattern alternation (rep < 1)
--   2. Spurious onsets at chord boundaries
--   3. Temporal distortion

module Harmonic.Interface.Tidal.BridgeDevSpec (spec) where

import Test.Hspec

import qualified Harmonic.Rules.Types.Progression as P
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as Pitch
import Harmonic.Interface.Tidal.BridgeDev
import qualified Harmonic.Interface.Tidal.Arranger as A
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Sound.Tidal.Context

-------------------------------------------------------------------------------
-- Test Fixtures (reused from BridgeSpec)
-------------------------------------------------------------------------------

-- |Helper: create a CadenceState for testing
mkCadenceState :: Pitch.NoteName -> String -> [Integer] -> H.CadenceState
mkCadenceState rootNote func intervals =
  let pcs = map (Pitch.mkPitchClass . fromIntegral) (take 3 intervals)
      cadence = H.Cadence func H.Unison pcs
  in H.CadenceState cadence rootNote H.FlatSpelling

-- |C major -> G major -> F major -> C major (4 chords)
testProgression :: P.Progression
testProgression =
  P.Progression $ Seq.fromList
    [ mkCadenceState Pitch.C "maj" [0, 4, 7]
    , mkCadenceState Pitch.G "maj" [0, 4, 7]
    , mkCadenceState Pitch.F "maj" [0, 4, 7]
    , mkCadenceState Pitch.C "maj" [0, 4, 7]
    ]

-------------------------------------------------------------------------------
-- Helpers: extract onset events from ControlPattern
-------------------------------------------------------------------------------

-- |Extract (onset-time, note-value) pairs from a ControlPattern over an Arc.
-- Only includes events where the event onset (start of whole) coincides with
-- the start of the part (i.e., true onsets, not continuations).
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

  describe "arrangeDev (onset-join)" $ do

    it "produces events from a simple chord pattern" $ do
      -- "[1 2 3 4]/4" = 1 chord per cycle over 4 cycles
      let chordSel = parseBP_E "[1 2 3 4]/4" :: Pattern Int
          result = arrangeDev A.flow testProgression chordSel (-24,24)
                     [parseBP_E "[0 1 2 3]"]
      onsetCount result (Arc 0 1) `shouldSatisfy` (> 0)

    it "returns silence for empty progression" $ do
      let emptyProg = P.Progression Seq.empty
          chordSel = parseBP_E "1" :: Pattern Int
          result = arrangeDev A.flow emptyProg chordSel (-24,24)
                     [parseBP_E "[0 1 2]"]
      onsetCount result (Arc 0 1) `shouldBe` 0

    -- Limitation 1: pattern consistency across cycles
    it "full pattern plays for every chord (no alternation)" $ do
      let chordSel = parseBP_E "[1 2]/2" :: Pattern Int
          pat = parseBP_E "[0 1 2 3]" :: Pattern Int
          result = arrangeDev A.flow testProgression chordSel (-24,24) [pat]
          cycle0 = onsetNotes result (Arc 0 1)
          cycle1 = onsetNotes result (Arc 1 2)
      -- Both cycles should produce the same number of events
      length cycle0 `shouldBe` length cycle1

    -- Limitation 2: no spurious onsets at chord boundaries
    it "sustained note does not re-trigger at chord boundary" $ do
      -- "[1 2]" = 2 chords within 1 cycle (boundary at 0.5)
      let chordSel = parseBP_E "[1 2]" :: Pattern Int
          -- A single note spanning the whole cycle
          pat = parseBP_E "0" :: Pattern Int
          result = arrangeDev A.flow testProgression chordSel (-24,24) [pat]
      -- Should produce exactly 1 onset, not 2
      onsetCount result (Arc 0 1) `shouldBe` 1

    -- Limitation 3: no temporal distortion
    it "events fall within expected cycle range" $ do
      let chordSel = parseBP_E "[1 2 3 4]/4" :: Pattern Int
          result = arrangeDev A.flow testProgression chordSel (-24,24)
                     [parseBP_E "[0 1 2 3]"]
          onsets = queryOnsets result (Arc 0 1)
      all (\(t, _) -> t >= 0 && t < 1) onsets `shouldBe` True

    -- Patterned selection: AABA form
    it "AABA form [1 1 2 1]/4 produces correct chord repetition" $ do
      let chordSel = parseBP_E "[1 1 2 1]/4" :: Pattern Int
          result = arrangeDev A.flow testProgression chordSel (-24,24)
                     [parseBP_E "0"]
          onsets = onsetNotes result (Arc 0 4)
      -- Chords 1, 1, 2, 1 → same chord for positions 0, 1, 3
      length onsets `shouldBe` 4
      (onsets !! 0) `shouldBe` (onsets !! 1)
      (onsets !! 0) `shouldBe` (onsets !! 3)

    -- Octave extension: indices beyond scale length add +12 per octave
    it "extends into higher octaves like toScale (index >= scale length)" $ do
      -- Index 3 on a 3-note chord [0,4,7] should give 12 (0+12), not 0
      let chordSel = parseBP_E "1" :: Pattern Int
          result = arrangeDev A.flow testProgression chordSel (-24,24)
                     [parseBP_E "[0 1 2 3]"]
          notes = onsetNotes result (Arc 0 1)
      length notes `shouldBe` 4
      -- Notes should ascend: each higher than the last
      (notes !! 3) `shouldSatisfy` (> (notes !! 2))

    -- Chord indices wrap modulo progression length
    it "chord indices wrap modulo progression length" $ do
      let chordSel = parseBP_E "[5 6 7 8]/4" :: Pattern Int
          result1 = arrangeDev A.flow testProgression chordSel (-24,24)
                      [parseBP_E "0"]
          chordSel2 = parseBP_E "[1 2 3 4]/4" :: Pattern Int
          result2 = arrangeDev A.flow testProgression chordSel2 (-24,24)
                      [parseBP_E "0"]
      -- 5 mod 4 = 1, 6 mod 4 = 2, etc. → same as [1 2 3 4]
      onsetNotes result1 (Arc 0 4) `shouldBe` onsetNotes result2 (Arc 0 4)

  describe "squeezeDev (squeeze)" $ do

    it "produces events from a simple chord pattern" $ do
      let chordSel = parseBP_E "[1 2 3 4]/4" :: Pattern Int
          result = squeezeDev A.flow testProgression chordSel (-24,24)
                     [parseBP_E "[0 1 2 3]"]
      onsetCount result (Arc 0 1) `shouldSatisfy` (> 0)

    -- Limitation 1: each chord gets full pattern
    it "each chord slot gets the full pattern" $ do
      let chordSel = parseBP_E "[1 2]/2" :: Pattern Int
          pat = parseBP_E "[0 1 2 3]" :: Pattern Int
          result = squeezeDev A.flow testProgression chordSel (-24,24) [pat]
          cycle0 = onsetNotes result (Arc 0 1)
          cycle1 = onsetNotes result (Arc 1 2)
      length cycle0 `shouldBe` length cycle1

    it "returns silence for empty progression" $ do
      let emptyProg = P.Progression Seq.empty
          chordSel = parseBP_E "1" :: Pattern Int
          result = squeezeDev A.flow emptyProg chordSel (-24,24)
                     [parseBP_E "[0 1 2]"]
      onsetCount result (Arc 0 1) `shouldBe` 0

  describe "arrangeStrict (innerJoin)" $ do

    it "produces events from a simple chord pattern" $ do
      let chordSel = parseBP_E "[1 2 3 4]/4" :: Pattern Int
          result = arrangeStrict A.flow testProgression chordSel (-24,24)
                     [parseBP_E "[0 1 2 3]"]
      onsetCount result (Arc 0 1) `shouldSatisfy` (> 0)

    -- innerJoin DOES re-trigger at boundaries (this is expected)
    it "re-triggers notes at chord boundaries" $ do
      -- "[1 2]" = 2 chords within 1 cycle (boundary at 0.5)
      let chordSel = parseBP_E "[1 2]" :: Pattern Int
          -- A single note spanning the whole cycle
          pat = parseBP_E "0" :: Pattern Int
          result = arrangeStrict A.flow testProgression chordSel (-24,24) [pat]
      -- With innerJoin, a note through 2 chords produces 2 events
      -- (1 onset + 1 continuation with different pitch at boundary)
      eventCount result (Arc 0 1) `shouldBe` 2

    it "returns silence for empty progression" $ do
      let emptyProg = P.Progression Seq.empty
          chordSel = parseBP_E "1" :: Pattern Int
          result = arrangeStrict A.flow emptyProg chordSel (-24,24)
                     [parseBP_E "[0 1 2]"]
      onsetCount result (Arc 0 1) `shouldBe` 0

  describe "lookupChordAt" $ do

    it "returns chord index at a given time" $ do
      -- "[0 1 2 3]" = 4 values within 1 cycle (each spans 1/4 cycle)
      let chordPat = parseBP_E "[0 1 2 3]" :: Pattern Int
      lookupChordAt 0 chordPat `shouldBe` 0
      -- Use mid-slot times to avoid boundary ambiguity
      lookupChordAt (3/8) chordPat `shouldBe` 1
      lookupChordAt (5/8) chordPat `shouldBe` 2
      lookupChordAt (7/8) chordPat `shouldBe` 3

    it "returns 0 as default for empty pattern" $ do
      lookupChordAt 0 silence `shouldBe` 0

  describe "repToChordPat" $ do

    it "converts rep=1 with 4 chords to sequential pattern" $ do
      let cpat = repToChordPat 4 1
      -- Should produce [1, 2, 3, 4] over 4 cycles
      -- Use mid-cycle probes to avoid boundary issues
      lookupChordAt (1/2) cpat `shouldBe` 1
      lookupChordAt (3/2) cpat `shouldBe` 2
      lookupChordAt (5/2) cpat `shouldBe` 3
      lookupChordAt (7/2) cpat `shouldBe` 4
