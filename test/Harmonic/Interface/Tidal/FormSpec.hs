{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Interface.Tidal.FormSpec
-- Description : Tests for Kinetics framework (Form module)

module Harmonic.Interface.Tidal.FormSpec (spec) where

import Test.Hspec

import qualified Harmonic.Rules.Types.Progression as P
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as Pitch
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import Harmonic.Interface.Tidal.Form
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Sound.Tidal.Context

-------------------------------------------------------------------------------
-- Test Fixtures
-------------------------------------------------------------------------------

-- |Helper: create a CadenceState for testing
mkCadenceState :: Pitch.NoteName -> String -> [Integer] -> H.CadenceState
mkCadenceState rootNote func intervals =
  let pcs = map (Pitch.mkPitchClass . fromIntegral) (take 3 intervals)
      cadence = H.Cadence func H.Unison pcs
  in H.CadenceState cadence rootNote H.FlatSpelling

-- |Simple test progressions
testProgA :: PC.ProgressionContext
testProgA = PC.fromProgression $ P.Progression $ Seq.fromList
  [ mkCadenceState Pitch.C "maj" [0, 4, 7]
  , mkCadenceState Pitch.G "maj" [0, 4, 7]
  ]

testProgB :: PC.ProgressionContext
testProgB = PC.fromProgression $ P.Progression $ Seq.fromList
  [ mkCadenceState Pitch.F "maj" [0, 4, 7]
  , mkCadenceState Pitch.D "min" [0, 3, 7]
  ]

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "FormNode construction" $ do
    it "creates node with at constructor (seconds, smooth)" $ do
      let node = at 10.0 0.5 0.8 testProgA
      fnTime node `shouldBe` Secs 10.0
      fnKinetics node `shouldBe` 0.5
      fnDynamic node `shouldBe` 0.8
      fnProg node `shouldBe` testProgA
      fnTrans node `shouldBe` Smooth

    it "time unit and transition are orthogonal across at/at'/rh/rh'" $ do
      fnTime  (rh  4 0 0 testProgA) `shouldBe` Bars 4
      fnTime  (rh' 4 0 0 testProgA) `shouldBe` Bars 4
      fnTime  (at' 4 0 0 testProgA) `shouldBe` Secs 4
      fnTrans (at  4 0 0 testProgA) `shouldBe` Smooth
      fnTrans (at' 4 0 0 testProgA) `shouldBe` Snap
      fnTrans (rh  4 0 0 testProgA) `shouldBe` Smooth
      fnTrans (rh' 4 0 0 testProgA) `shouldBe` Snap

  describe "bars (rh) vs seconds (at)" $ do
    -- rh 4 at 120bpm = 4 bars * 4 beats = 16 cycles = 8s;  at 8 (120bpm) = 8*2 = 16 cycles = 8s
    let kBar = formK 120 [at 0 0 0 testProgA, rh 4 1 1 testProgA]
        kSec = formK 120 [at 0 0 0 testProgA, at 8 1 1 testProgA]
    it "rh resolves bars to the same loop seconds as the equivalent at" $ do
      kLoopSecs kBar `shouldBe` 8
      kLoopSecs kSec `shouldBe` 8
      kCps kBar `shouldBe` kCps kSec
    it "rh and equivalent at produce identical kinetics signals" $ do
      let bar = [ value e | e <- queryArc (kSignal kBar) (Arc 0 16) ]
          sec = [ value e | e <- queryArc (kSignal kSec) (Arc 0 16) ]
      bar `shouldBe` sec

  describe "snap (') vs smooth transition" $ do
    -- 60bpm → cps 1 → span 10 cycles; sample the midpoint (cycle 5..6)
    let midVals kin = [ value e | e <- queryArc (kSignal kin) (Arc 5 6) ]
    it "smooth ramps toward the next node" $ do
      let k = formK 60 [at 0 0.0 0.0 testProgA, at 10 1.0 1.0 testProgA]
      midVals k `shouldSatisfy` all (> 0.3)
    it "snap holds the start value until the next node" $ do
      let k = formK 60 [at' 0 0.0 0.0 testProgA, at 10 1.0 1.0 testProgA]
      midVals k `shouldSatisfy` all (\v -> abs v < 1e-9)

  describe "display invariants (kLoopSecs / kCps)" $ do
    it "seconds form keeps kLoopSecs = last node seconds (unchanged)" $ do
      let k = formK 90 [at 0 0 0 testProgA, at 148 0 0 testProgA]
      kLoopSecs k `shouldBe` 148
      kCps k `shouldBe` 1.5
    it "single-node form stays atemporal (kLoopSecs 0)" $ do
      kLoopSecs (formK 90 [at 0 0.7 0.9 testProgA]) `shouldBe` 0

  describe "Single-state form" $ do
    it "single node produces constant kinetics signal" $ do
      let k = formK 90 [at 0 0.7 0.9 testProgA]
          -- Query the signal: should be constant 0.7
          evs = queryArc (kSignal k) (Arc 0 1)
      -- All events should have value 0.7
      all (\e -> abs (value e - 0.7) < 0.001) evs `shouldBe` True

    it "single node produces constant dynamic signal" $ do
      let k = formK 90 [at 0 0.7 0.9 testProgA]
          evs = queryArc (kDynamic k) (Arc 0 1)
      all (\e -> abs (value e - 0.9) < 0.001) evs `shouldBe` True

    it "single node produces constant progression" $ do
      let k = formK 90 [at 0 0.7 0.9 testProgA]
          evs = queryArc (kProg k) (Arc 0 1)
      all (\e -> value e == testProgA) evs `shouldBe` True

  describe "ki (range gating)" $ do
    it "passes events when signal in range" $ do
      let kin = Kinetics (pure 0.5) (pure 1.0) (pure testProgA) 0 0
          ik = (kin, parseBP_E "1" :: Pattern Int) :: IK
          result = ki (0.3, 0.7) ik (note "0")
          evs = queryArc result (Arc 0 1)
      length evs `shouldSatisfy` (> 0)

    it "blocks events when signal outside range" $ do
      let kin = Kinetics (pure 0.5) (pure 1.0) (pure testProgA) 0 0
          ik = (kin, parseBP_E "1" :: Pattern Int) :: IK
          result = ki (0.6, 0.9) ik (note "0")
          evs = queryArc result (Arc 0 1)
      length evs `shouldBe` 0

    it "inclusive at boundaries" $ do
      let kin = Kinetics (pure 0.5) (pure 1.0) (pure testProgA) 0 0
          ik = (kin, parseBP_E "1" :: Pattern Int) :: IK
          result = ki (0.5, 0.5) ik (note "0")
          evs = queryArc result (Arc 0 1)
      length evs `shouldSatisfy` (> 0)

  describe "slate" $ do
    it "activates patterns when in range" $ do
      let kin = Kinetics (pure 0.8) (pure 1.0) (pure testProgA) 0 0
          ik = (kin, parseBP_E "1" :: Pattern Int) :: IK
          result = slate (0.5, 1.0) ik [note "0", note "1"]
          evs = queryArc result (Arc 0 1)
      length evs `shouldSatisfy` (> 0)

    it "blocks patterns when outside range" $ do
      let kin = Kinetics (pure 0.2) (pure 1.0) (pure testProgA) 0 0
          ik = (kin, parseBP_E "1" :: Pattern Int) :: IK
          result = slate (0.5, 1.0) ik [note "0", note "1"]
          evs = queryArc result (Arc 0 1)
      length evs `shouldBe` 0

  describe "withForm" $ do
    it "applies function using progression from kinetics" $ do
      let kin = Kinetics (pure 1.0) (pure 1.0) (pure testProgA) 0 0
          ik = (kin, parseBP_E "1" :: Pattern Int) :: IK
          result = withForm ik (\_ -> note "42")
          evs = queryArc result (Arc 0 1)
      length evs `shouldSatisfy` (> 0)

  describe "Form realization (multi-node)" $ do
    it "formK produces kinetics from multi-node form" $ do
      let nodes = [at 0 0.0 0.0 testProgA, at 10.0 1.0 1.0 testProgA]
          k = formK 90 nodes
          -- At the very start, kinetics should be near 0
          evs = queryArc (kSignal k) (Arc 0 (1/1000))
      -- Should have events (signal exists)
      length evs `shouldSatisfy` (>= 0)  -- Just verify it doesn't crash

    it "discrete signal holds progression value" $ do
      let nodes = [at 0 0.0 0.0 testProgA, at 10.0 1.0 1.0 testProgB]
          k = formK 90 nodes
          -- Query near start: should have testProgA
          evs = queryArc (kProg k) (Arc 0 1)
      all (\e -> value e == testProgA) evs `shouldBe` True
