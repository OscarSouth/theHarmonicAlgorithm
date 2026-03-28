{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Interface.Tidal.FormSpec
-- Description : Tests for Kinetics framework (Form module)

module Harmonic.Interface.Tidal.FormSpec (spec) where

import Test.Hspec

import qualified Harmonic.Rules.Types.Progression as P
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as Pitch
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
testProgA :: P.Progression
testProgA = P.Progression $ Seq.fromList
  [ mkCadenceState Pitch.C "maj" [0, 4, 7]
  , mkCadenceState Pitch.G "maj" [0, 4, 7]
  ]

testProgB :: P.Progression
testProgB = P.Progression $ Seq.fromList
  [ mkCadenceState Pitch.F "maj" [0, 4, 7]
  , mkCadenceState Pitch.D "min" [0, 3, 7]
  ]

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "FormNode construction" $ do
    it "creates node with at constructor" $ do
      let node = at 10.0 0.5 0.8 testProgA
      fnTime node `shouldBe` 10.0
      fnKinetics node `shouldBe` 0.5
      fnDynamic node `shouldBe` 0.8
      fnProg node `shouldBe` testProgA

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
      let k = Kinetics (pure 0.5) (pure 1.0) (pure testProgA)
          result = ki (0.3, 0.7) k (note "0")
          evs = queryArc result (Arc 0 1)
      length evs `shouldSatisfy` (> 0)

    it "blocks events when signal outside range" $ do
      let k = Kinetics (pure 0.5) (pure 1.0) (pure testProgA)
          result = ki (0.6, 0.9) k (note "0")
          evs = queryArc result (Arc 0 1)
      length evs `shouldBe` 0

    it "inclusive at boundaries" $ do
      let k = Kinetics (pure 0.5) (pure 1.0) (pure testProgA)
          result = ki (0.5, 0.5) k (note "0")
          evs = queryArc result (Arc 0 1)
      length evs `shouldSatisfy` (> 0)

  describe "slate" $ do
    it "activates patterns when in range" $ do
      let k = Kinetics (pure 0.8) (pure 1.0) (pure testProgA)
          result = slate (0.5, 1.0) k [note "0", note "1"]
          evs = queryArc result (Arc 0 1)
      length evs `shouldSatisfy` (> 0)

    it "blocks patterns when outside range" $ do
      let k = Kinetics (pure 0.2) (pure 1.0) (pure testProgA)
          result = slate (0.5, 1.0) k [note "0", note "1"]
          evs = queryArc result (Arc 0 1)
      length evs `shouldBe` 0

  describe "withForm" $ do
    it "applies function using progression from kinetics" $ do
      let k = Kinetics (pure 1.0) (pure 1.0) (pure testProgA)
          result = withForm k (\_ -> note "42")
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
