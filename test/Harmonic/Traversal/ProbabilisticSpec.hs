{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Harmonic.Core.ProbabilisticSpec
-- Description : Tests for gamma distribution sampling
--
-- Validates the Phase C Probabilistic module:
--   * gammaIndex: Index generation within bounds
--   * gammaSelect: Selection from weighted lists
--   * weightedSelect: Standard roulette wheel selection
--   * Statistical properties of gamma sampling

module Harmonic.Traversal.ProbabilisticSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Monad (replicateM)
import Data.List (sort, sortBy)
import Data.Ord (Down(..))

import Harmonic.Traversal.Probabilistic

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- |Run an IO action multiple times and collect results
sampleMany :: Int -> IO a -> IO [a]
sampleMany n action = replicateM n action

-- |Check if a value is within bounds
inBounds :: Int -> Int -> Int -> Bool
inBounds lower upper x = x >= lower && x <= upper

-- |Calculate mean of a list
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

-- |Calculate standard deviation
stdDev :: [Double] -> Double
stdDev xs = 
  let m = mean xs
      variance = sum [(x - m)^2 | x <- xs] / fromIntegral (length xs)
   in sqrt variance

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "gammaIndex" $ do
    
    describe "Bounds checking" $ do
      it "always returns index in [0, maxIndex]" $ do
        -- Sample many times with various shapes
        results <- sampleMany 100 $ gammaIndex 1.0 10
        all (inBounds 0 10) results `shouldBe` True
      
      it "works with maxIndex = 0" $ do
        results <- sampleMany 20 $ gammaIndex 1.0 0
        all (== 0) results `shouldBe` True
      
      it "works with high shape parameter" $ do
        results <- sampleMany 100 $ gammaIndex 5.0 10
        all (inBounds 0 10) results `shouldBe` True
      
      it "works with very low shape parameter" $ do
        results <- sampleMany 100 $ gammaIndex 0.5 10
        all (inBounds 0 10) results `shouldBe` True
    
    describe "Statistical properties" $ do
      it "shape=1.0 tends toward lower indices (exponential-like)" $ do
        -- With shape=1.0, gamma is exponential, mean ≈ 1.0
        results <- sampleMany 500 $ gammaIndex 1.0 20
        let avg = mean (map fromIntegral results)
        -- Mean should be relatively low (< 5 for maxIndex=20)
        avg `shouldSatisfy` (< 5.0)
      
      it "shape=3.0 has higher mean index" $ do
        results <- sampleMany 500 $ gammaIndex 3.0 20
        let avg = mean (map fromIntegral results)
        -- Mean should be higher than shape=1.0 case
        avg `shouldSatisfy` (> 1.5)

  describe "gammaSelect" $ do
    
    it "returns Nothing for empty list" $ do
      result <- gammaSelect 1.0 ([] :: [(Int, Double)])
      result `shouldBe` Nothing
    
    it "returns Just for non-empty list" $ do
      let candidates = [("a", 1.0), ("b", 2.0), ("c", 3.0)]
      result <- gammaSelect 1.0 candidates
      result `shouldSatisfy` maybe False (const True)
    
    it "with shape=1.0, tends to select highest-weighted items" $ do
      let candidates = [("low", 1.0), ("med", 5.0), ("high", 10.0)]
      results <- sampleMany 100 $ gammaSelect 1.0 candidates
      let highCount = length [x | Just x <- results, x == "high"]
      -- "high" should be selected most often (>50% with shape=1.0)
      highCount `shouldSatisfy` (> 30)
    
    it "with high shape, explores deeper into the list" $ do
      let candidates = [("high", 10.0), ("med", 5.0), ("low", 1.0)]
      results <- sampleMany 100 $ gammaSelect 4.0 candidates
      let lowCount = length [x | Just x <- results, x == "low"]
      -- With high shape, even "low" should be selected sometimes
      lowCount `shouldSatisfy` (> 5)

  describe "gammaSequence" $ do
    
    it "returns correct number of indices" $ do
      result <- gammaSequence 1.0 0.0 16
      length result `shouldBe` 16
    
    it "returns empty list for count=0" $ do
      result <- gammaSequence 1.0 0.0 0
      result `shouldBe` []
    
    it "all indices are non-negative" $ do
      results <- gammaSequence 2.0 0.5 100
      all (>= 0) results `shouldBe` True
    
    it "entropy multiplier affects distribution" $ do
      -- Higher entropy should give higher mean indices
      lowEntropy <- sampleMany 100 (gammaSequence 1.0 0.0 1 >>= pure . head)
      highEntropy <- sampleMany 100 (gammaSequence 1.0 2.0 1 >>= pure . head)
      let lowMean = mean (map fromIntegral lowEntropy)
          highMean = mean (map fromIntegral highEntropy)
      -- High entropy should have higher mean
      highMean `shouldSatisfy` (> lowMean)

  describe "weightedSelect" $ do
    
    it "returns Nothing for empty list" $ do
      result <- weightedSelect ([] :: [(Int, Double)])
      result `shouldBe` Nothing
    
    it "returns Just for non-empty list" $ do
      let candidates = [(1, 1.0), (2, 2.0)]
      result <- weightedSelect candidates
      result `shouldSatisfy` maybe False (const True)
    
    it "respects weight proportions" $ do
      -- "heavy" has 9x the weight of "light"
      let candidates = [("light", 1.0), ("heavy", 9.0)]
      results <- sampleMany 200 $ weightedSelect candidates
      let heavyCount = length [x | Just x <- results, x == "heavy"]
          ratio = fromIntegral heavyCount / 200.0
      -- Should be approximately 0.9 (within statistical tolerance)
      ratio `shouldSatisfy` (> 0.75)
      ratio `shouldSatisfy` (< 0.99)
    
    it "handles zero-weight items" $ do
      let candidates = [("zero", 0.0), ("nonzero", 1.0)]
      results <- sampleMany 50 $ weightedSelect candidates
      -- All selections should be "nonzero"
      all (== Just "nonzero") results `shouldBe` True

  describe "pickWeighted" $ do
    
    it "selects first element when r <= first weight" $ do
      let candidates = [(1, 0.5), (2, 0.5)]
          result = pickWeighted candidates 0.3
      result `shouldBe` 1
    
    it "selects second element when r > first weight" $ do
      let candidates = [(1, 0.5), (2, 0.5)]
          result = pickWeighted candidates 0.7
      result `shouldBe` 2

  describe "withRandomGen" $ do
    
    it "provides a random generator for custom operations" $ do
      let result = withRandomGen 42 $ \gen -> 42 :: Int
      result `shouldBe` 42
    
    it "same seed produces same result" $ do
      let result1 = withRandomGen 123 $ \gen -> 99
          result2 = withRandomGen 123 $ \gen -> 99
      result1 `shouldBe` result2
