-- |
-- Module      : Harmonic.Traversal.ProbabilisticSpec
-- Description : Tests for gamma distribution sampling
--
-- Validates 'gammaIndexScaledWith', the production sampler behind the
-- entropy dial: bounds safety, the rank-target contract (median drawn
-- index tracks entropy * 10), and small-pool truncation (no probability
-- mass collapse onto the worst-ranked element).

module Harmonic.Traversal.ProbabilisticSpec (spec) where

import Test.Hspec
import Control.Monad (replicateM)
import Data.List (sort)
import System.Random.MWC (createSystemRandom)

import Harmonic.Traversal.Probabilistic

-- |Run an IO action multiple times and collect results
sampleMany :: Int -> IO a -> IO [a]
sampleMany n action = replicateM n action

median :: [Int] -> Int
median xs = sort xs !! (length xs `div` 2)

spec :: Spec
spec = do
  describe "gammaIndexScaledWith" $ do
    it "stays within [0, poolSize-1] across the entropy range" $ do
      gen <- createSystemRandom
      results <- sequence [ gammaIndexScaledWith gen e p
                          | e <- [0.0, 0.5, 1.0, 2.0]
                          , p <- [1, 3, 30]
                          , _ <- [1 .. 40 :: Int] ]
      results `shouldSatisfy` all (>= 0)
      -- pool of 1 can only ever yield 0
      r1 <- sampleMany 40 (gammaIndexScaledWith gen 1.0 1)
      r1 `shouldSatisfy` all (== 0)

    it "clamps negative entropy instead of crashing" $ do
      gen <- createSystemRandom
      lo <- sampleMany 40 (gammaIndexScaledWith gen (-5.0) 10)
      lo `shouldSatisfy` all (\i -> i >= 0 && i <= 9)

    it "usually picks the top candidate at entropy 0" $ do
      gen <- createSystemRandom
      xs <- sampleMany 2000 (gammaIndexScaledWith gen 0.0 30)
      let pTop = fromIntegral (length (filter (== 0) xs)) / 2000 :: Double
      pTop `shouldSatisfy` (> 0.7)
      pTop `shouldSatisfy` (< 0.99)

    it "median drawn index tracks entropy * 10 on large pools" $ do
      gen <- createSystemRandom
      half <- median <$> sampleMany 2000 (gammaIndexScaledWith gen 0.5 100)
      full <- median <$> sampleMany 2000 (gammaIndexScaledWith gen 1.0 100)
      deep <- median <$> sampleMany 2000 (gammaIndexScaledWith gen 2.0 100)
      half `shouldSatisfy` (\m -> m >= 3 && m <= 7)
      full `shouldSatisfy` (\m -> m >= 7 && m <= 13)
      deep `shouldSatisfy` (\m -> m >= 15 && m <= 25)

    it "does not collapse onto the worst element of a small pool" $ do
      gen <- createSystemRandom
      xs <- sampleMany 2000 (gammaIndexScaledWith gen 1.0 9)
      let pLast = fromIntegral (length (filter (== 8) xs)) / 2000 :: Double
      pLast `shouldSatisfy` (< 0.35)
