{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Interface.Tidal.LineHarmonySpec
-- Description : Smoke tests for the walking-bass Tidal interface
module Harmonic.Interface.Tidal.LineHarmonySpec (spec) where

import Test.Hspec

import qualified Harmonic.Rules.Types.Progression as P
import qualified Harmonic.Rules.Types.Harmony as H
import Harmonic.Interface.Tidal.Form (Kinetics(..))
import Harmonic.Interface.Tidal.Groove (fund)
import Harmonic.Interface.Tidal.LineHarmony
import Harmonic.Traversal.WalkingBass (closestLowMidi)

import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Sound.Tidal.Context

-------------------------------------------------------------------------------
-- Fixtures
-------------------------------------------------------------------------------

testProgression :: P.Progression
testProgression = P.Progression $ Seq.fromList
  [ H.initCadenceState 0 "C" [0,4,7]
  , H.initCadenceState 0 "G" [0,4,7]
  , H.initCadenceState 0 "A" [0,3,7]
  , H.initCadenceState 0 "F" [0,4,7]
  ]

fullKin :: P.Progression -> Kinetics
fullKin pr = Kinetics (pure 1.0) (pure 1.0) (pure pr)

mutedKin :: P.Progression -> Kinetics
mutedKin pr = Kinetics (pure 0.0) (pure 1.0) (pure pr)

-------------------------------------------------------------------------------
-- Onset helpers (mirrors BridgeSpec)
-------------------------------------------------------------------------------

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

onsetCount :: ControlPattern -> Arc -> Int
onsetCount pat arc = length (queryOnsets pat arc)

onsetNotes :: ControlPattern -> Arc -> [Double]
onsetNotes pat arc = map snd (queryOnsets pat arc)

-------------------------------------------------------------------------------
-- Spec
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "lineHarmony" $ do

    it "produces events from a simple beat pattern" $ do
      let chordSel = parseBP_E "1" :: Pattern Int
          result  = lineHarmony (pure 1.0) (fullKin testProgression, chordSel) fund
                      [parseBP_E "[1 2 3 4]"]
      onsetCount result (Arc 0 1) `shouldSatisfy` (> 0)

    it "returns silence for empty progression" $ do
      let emptyProg = P.Progression Seq.empty
          chordSel  = parseBP_E "1" :: Pattern Int
          result    = lineHarmony (pure 1.0) (fullKin emptyProg, chordSel) fund
                        [parseBP_E "[1 2 3 4]"]
      onsetCount result (Arc 0 1) `shouldBe` 0

    it "beat 1 event at bar 0 equals walking-bass first note (Tidal-shifted)" $ do
      let chordSel = parseBP_E "1" :: Pattern Int
          result   = lineHarmony (pure 1.0) (fullKin testProgression, chordSel) fund
                       [parseBP_E "1"]
          notes    = onsetNotes result (Arc 0 1)
          -- MIDI 36 (C2) emitted as note -12 after tidalNoteOffset=48 shift.
          expected = fromIntegral (closestLowMidi 0 - 48) :: Double
      take 1 notes `shouldBe` [expected]

    it "polyphonic stacking adds events from extra layers" $ do
      let chordSel = parseBP_E "1" :: Pattern Int
          single   = lineHarmony (pure 1.0) (fullKin testProgression, chordSel) fund
                       [parseBP_E "1"]
          doubled  = lineHarmony (pure 1.0) (fullKin testProgression, chordSel) fund
                       [parseBP_E "1", parseBP_E "3"]
      onsetCount doubled (Arc 0 1) `shouldSatisfy` (> onsetCount single (Arc 0 1))

    it "kinetics gate (kSignal < 0.1) mutes events" $ do
      let chordSel = parseBP_E "1" :: Pattern Int
          result   = lineHarmony (pure 1.0) (mutedKin testProgression, chordSel) fund
                       [parseBP_E "[1 2 3 4]"]
      onsetCount result (Arc 0 1) `shouldBe` 0

    it "events stay within the cycle bounds" $ do
      let chordSel = parseBP_E "1" :: Pattern Int
          result   = lineHarmony (pure 1.0) (fullKin testProgression, chordSel) fund
                       [parseBP_E "[1 2 3 4]"]
          onsets   = queryOnsets result (Arc 0 1)
      all (\(t, _) -> t >= 0 && t < 1) onsets `shouldBe` True
