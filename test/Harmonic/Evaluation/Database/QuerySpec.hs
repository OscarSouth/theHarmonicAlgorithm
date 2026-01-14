{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Harmonic.Database.QuerySpec
-- Description : Tests for composer weight parsing and resolution
--
-- Validates the Phase C Query module:
--   * parseComposerWeights: String parsing formats
--   * normalizeWeights: Normalization to sum 1.0
--   * resolveWeights: Composer blend scoring

module Harmonic.Evaluation.Database.QuerySpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T

import Harmonic.Evaluation.Database.Query
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as P

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- |Check if weights sum to approximately 1.0 (within floating point tolerance)
sumsToOne :: ComposerWeights -> Bool
sumsToOne weights = 
  let total = sum (Map.elems weights)
   in abs (total - 1.0) < 0.0001 || Map.null weights

-- |Check if all weights are non-negative
allPositive :: ComposerWeights -> Bool
allPositive = all (>= 0) . Map.elems

-- |Create a test Cadence using Phase B types
testCadence :: Int -> H.Cadence
testCadence n = H.Cadence ("test" ++ show n) H.Unison [P.P 0, P.P 4, P.P 7]

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "parseComposerWeights" $ do
    
    describe "Space-separated equal weights" $ do
      it "\"bach debussy\" -> equal 0.5/0.5" $ do
        let result = parseComposerWeights "bach debussy"
        Map.lookup "bach" result `shouldBe` Just 0.5
        Map.lookup "debussy" result `shouldBe` Just 0.5
        sumsToOne result `shouldBe` True
      
      it "\"bach\" alone -> 1.0" $ do
        let result = parseComposerWeights "bach"
        Map.lookup "bach" result `shouldBe` Just 1.0
        sumsToOne result `shouldBe` True
      
      it "\"bach debussy stravinsky\" -> equal thirds" $ do
        let result = parseComposerWeights "bach debussy stravinsky"
            expected = 1.0 / 3.0
        Map.lookup "bach" result `shouldSatisfy` maybe False (\v -> abs (v - expected) < 0.0001)
        Map.lookup "debussy" result `shouldSatisfy` maybe False (\v -> abs (v - expected) < 0.0001)
        Map.lookup "stravinsky" result `shouldSatisfy` maybe False (\v -> abs (v - expected) < 0.0001)
        sumsToOne result `shouldBe` True
    
    describe "Weighted format (colon syntax)" $ do
      it "\"bach:30 debussy:70\" -> 0.3/0.7" $ do
        let result = parseComposerWeights "bach:30 debussy:70"
        Map.lookup "bach" result `shouldBe` Just 0.3
        Map.lookup "debussy" result `shouldBe` Just 0.7
        sumsToOne result `shouldBe` True
      
      it "\"bach:1 debussy:1\" -> equal 0.5/0.5" $ do
        let result = parseComposerWeights "bach:1 debussy:1"
        Map.lookup "bach" result `shouldBe` Just 0.5
        Map.lookup "debussy" result `shouldBe` Just 0.5
        sumsToOne result `shouldBe` True
      
      it "\"bach:100\" -> 1.0" $ do
        let result = parseComposerWeights "bach:100"
        Map.lookup "bach" result `shouldBe` Just 1.0
        sumsToOne result `shouldBe` True
      
      it "\"bach:0.3 debussy:0.7\" -> already normalized" $ do
        let result = parseComposerWeights "bach:0.3 debussy:0.7"
        Map.lookup "bach" result `shouldBe` Just 0.3
        Map.lookup "debussy" result `shouldBe` Just 0.7
        sumsToOne result `shouldBe` True
    
    describe "Comma-separated format" $ do
      it "\"bach:30, debussy:70\" -> 0.3/0.7" $ do
        let result = parseComposerWeights "bach:30, debussy:70"
        Map.lookup "bach" result `shouldBe` Just 0.3
        Map.lookup "debussy" result `shouldBe` Just 0.7
        sumsToOne result `shouldBe` True
      
      it "\"bach, debussy, stravinsky\" -> equal thirds" $ do
        let result = parseComposerWeights "bach, debussy, stravinsky"
        Map.size result `shouldBe` 3
        sumsToOne result `shouldBe` True
    
    describe "Edge cases" $ do
      it "empty string -> empty map" $ do
        let result = parseComposerWeights ""
        Map.null result `shouldBe` True
      
      it "whitespace only -> empty map" $ do
        let result = parseComposerWeights "   "
        Map.null result `shouldBe` True
      
      it "extra spaces handled gracefully" $ do
        let result = parseComposerWeights "  bach   debussy  "
        Map.size result `shouldBe` 2
        sumsToOne result `shouldBe` True
      
      it "invalid weight defaults to 1.0" $ do
        let result = parseComposerWeights "bach:abc debussy:70"
        -- "abc" should parse as 1.0, then normalized with 70
        Map.size result `shouldBe` 2
        sumsToOne result `shouldBe` True

  describe "normalizeWeights" $ do
    
    it "normalizes weights to sum 1.0" $ do
      let input = Map.fromList [("a", 25.0), ("b", 75.0)]
          result = normalizeWeights input
      Map.lookup "a" result `shouldBe` Just 0.25
      Map.lookup "b" result `shouldBe` Just 0.75
      sumsToOne result `shouldBe` True
    
    it "handles single entry" $ do
      let input = Map.fromList [("a", 50.0)]
          result = normalizeWeights input
      Map.lookup "a" result `shouldBe` Just 1.0
    
    it "preserves empty map" $ do
      let result = normalizeWeights Map.empty
      Map.null result `shouldBe` True
    
    it "handles all-zero weights (edge case)" $ do
      let input = Map.fromList [("a", 0.0), ("b", 0.0)]
          result = normalizeWeights input
      -- Should not crash, weights stay 0
      allPositive result `shouldBe` True

  describe "resolveWeights" $ do
    
    it "multiplies user blend with edge weights" $ do
      let userBlend = Map.fromList [("bach", 1.0)]
          candidates = [(testCadence 1, Map.fromList [("bach", 10.0), ("debussy", 5.0)])]
          result = resolveWeights userBlend candidates
      -- Score should be 1.0 * 10.0 = 10.0 (bach only)
      length result `shouldBe` 1
      snd (head result) `shouldBe` 10.0
    
    it "blends multiple composers" $ do
      let userBlend = Map.fromList [("bach", 0.5), ("debussy", 0.5)]
          candidates = [(testCadence 1, Map.fromList [("bach", 10.0), ("debussy", 20.0)])]
          result = resolveWeights userBlend candidates
      -- Score should be 0.5 * 10.0 + 0.5 * 20.0 = 15.0
      snd (head result) `shouldBe` 15.0
    
    it "returns 0 score for missing composer" $ do
      let userBlend = Map.fromList [("stravinsky", 1.0)]
          candidates = [(testCadence 1, Map.fromList [("bach", 10.0)])]
          result = resolveWeights userBlend candidates
      -- Stravinsky not in edge weights, so score = 0
      snd (head result) `shouldBe` 0.0
    
    it "sorts by score descending" $ do
      let userBlend = Map.fromList [("bach", 1.0)]
          candidates = [ (testCadence 1, Map.fromList [("bach", 5.0)])
                       , (testCadence 2, Map.fromList [("bach", 15.0)])
                       , (testCadence 3, Map.fromList [("bach", 10.0)])
                       ]
          result = resolveWeights userBlend candidates
      map snd result `shouldBe` [15.0, 10.0, 5.0]

  describe "applyComposerBlend" $ do
    
    it "filters out zero-score candidates" $ do
      let userBlend = Map.fromList [("bach", 1.0)]
          candidates = [ (testCadence 1, Map.fromList [("bach", 10.0)])
                       , (testCadence 2, Map.fromList [("debussy", 20.0)])  -- No bach
                       ]
          result = applyComposerBlend userBlend candidates
      length result `shouldBe` 1
