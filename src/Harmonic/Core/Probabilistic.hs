{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Harmonic.Core.Probabilistic
-- Description : Gamma distribution sampling for weighted selection
-- 
-- This module implements probabilistic selection for the generative engine.
-- The key concept is the "entropy" knob: higher gamma shape parameter pushes
-- selection probability deeper into the sorted candidate list, favoring
-- more "unusual" but still valid harmonic choices.
--
-- Replaces the legacy R-based gammaGen with pure Haskell implementation.

module Harmonic.Core.Probabilistic
  ( -- * Gamma Sampling
    gammaIndex
  , gammaIndexScaled
  , gammaSelect
  , gammaSelectFromPool
  , gammaSequence
  
    -- * Weighted Selection
  , weightedSelect
  , pickWeighted
  
    -- * Random Utilities
  , withRandomGen
  ) where

import System.Random (randomRIO, randomIO, StdGen, mkStdGen, Random(..))
import System.Random.MWC (GenIO, createSystemRandom, uniformRM)
import qualified System.Random.MWC.Distributions as Dist
import Statistics.Distribution (quantile)
import Statistics.Distribution.Gamma (gammaDistr)
import Data.List (sortBy)
import Data.Ord (Down(..))
import Control.Monad (replicateM)

-------------------------------------------------------------------------------
-- Gamma Sampling
-------------------------------------------------------------------------------

-- |Draw an index from a gamma distribution.
-- 
-- The shape parameter controls "unusualness":
--   shape ≈ 1.0: High probability of index 0 or 1 (common paths)
--   shape > 2.0: Distribution peak moves away from 0 (unusual paths)
--   shape > 4.0: Very likely to pick 3rd, 4th, or deeper indices
--
-- The index is clamped to [0, maxIndex] to ensure valid selection.
-- Alpha is floored at 0.01 to prevent crash when entropy=0.
--
-- Implementation uses Statistics.Distribution.Gamma for the distribution
-- and mwc-random for high-quality random number generation.
gammaIndex :: Double -> Int -> IO Int
gammaIndex shape maxIndex = do
  gen <- createSystemRandom
  let safeShape = max 0.01 shape  -- Prevent gamma crash at alpha=0
  x <- Dist.gamma safeShape 1.0 gen  -- Gamma(shape, scale=1.0)
  let idx = floor x
  pure $ max 0 (min idx maxIndex)

-- |Draw an index scaled for entropy in [0, 1] range.
-- 
-- Maps entropy to gamma shape parameter:
--   entropy = 0.0 -> shape = 1.0 (strongly peaked at index 0)
--   entropy = 0.5 -> shape = 3.0 (moderate spread)
--   entropy = 1.0 -> shape = 5.0 (spread toward higher indices)
--
-- The index is clamped to [0, poolSize-1].
-- This is the primary selection function for the redesigned algorithm.
gammaIndexScaled :: Double  -- ^ Entropy in [0, 1] range
                 -> Int     -- ^ Pool size (e.g., 30)
                 -> IO Int
gammaIndexScaled entropy poolSize = do
  gen <- createSystemRandom
  let clampedEntropy = max 0.0 (min 1.0 entropy)
      shape = 1.0 + clampedEntropy * 9.0  -- Maps [0,1] -> [1,5]
      safeShape = max 0.01 shape
  x <- Dist.gamma safeShape 1.0 gen
  let idx = floor x
      maxIdx = poolSize - 1
  pure $ max 0 (min idx maxIdx)

-- |Select an element from a candidate pool using scaled gamma sampling.
-- 
-- The pool is expected to be pre-sorted (highest score first).
-- Entropy controls how "adventurous" the selection is:
--   entropy = 0.0 -> almost always picks top candidate
--   entropy = 1.0 -> frequently picks candidates deep in the list
--
-- If the pool is empty, returns Nothing.
gammaSelectFromPool :: Double -> [(a, Double)] -> IO (Maybe a)
gammaSelectFromPool _ [] = pure Nothing
gammaSelectFromPool entropy pool = do
  idx <- gammaIndexScaled entropy (length pool)
  pure $ Just $ fst (pool !! idx)

-- |Select an element from a weighted list using gamma sampling.
-- 
-- The list is first sorted by weight (highest first), then gamma sampling
-- determines which index to pick. Higher shape = deeper index = more unusual.
--
-- If the list is empty, returns Nothing.
gammaSelect :: Double -> [(a, Double)] -> IO (Maybe a)
gammaSelect _ [] = pure Nothing
gammaSelect shape candidates = do
  let sorted = sortBy (compare `on` (Down . snd)) candidates
      maxIdx = length sorted - 1
  idx <- gammaIndex shape maxIdx
  pure $ Just $ fst (sorted !! idx)

-- |Generate a sequence of gamma-distributed indices.
-- 
-- Matches legacy gammaGen behavior: generate n indices for n transitions.
-- Each index can be used to select from a sorted candidate list.
gammaSequence :: Double    -- ^ Gamma shape parameter
              -> Double    -- ^ Entropy multiplier (scales shape)
              -> Int       -- ^ Number of indices to generate
              -> IO [Int]
gammaSequence baseShape entropy count = do
  gen <- createSystemRandom
  let effectiveShape = baseShape * (1 + entropy)
  replicateM count $ do
    x <- Dist.gamma effectiveShape 1.0 gen
    pure $ floor x

-------------------------------------------------------------------------------
-- Weighted Selection (Non-Gamma)
-------------------------------------------------------------------------------

-- |Select from a weighted list using standard roulette wheel selection.
-- 
-- Each element's selection probability is proportional to its weight.
-- This is used when gamma "unusualness" is not desired.
weightedSelect :: [(a, Double)] -> IO (Maybe a)
weightedSelect [] = pure Nothing
weightedSelect candidates = do
  let total = sum (map snd candidates)
  if total <= 0
    then pure $ Just $ fst (head candidates)
    else do
      r <- randomRIO (0, total)
      pure $ Just $ pick r candidates
  where
    pick _ [(x, _)] = x
    pick r ((x, w):rest)
      | r <= w    = x
      | otherwise = pick (r - w) rest
    pick _ [] = error "weightedSelect: empty list"

-- |Pure version of weighted selection given a random value in [0, total].
-- Useful for deterministic testing.
pickWeighted :: [(a, Double)] -> Double -> a
pickWeighted [] _ = error "pickWeighted: empty list"
pickWeighted candidates r = go r candidates
  where
    go _ [(x, _)] = x
    go remaining ((x, w):rest)
      | remaining <= w = x
      | otherwise      = go (remaining - w) rest
    go _ [] = error "pickWeighted: exhausted list"

-------------------------------------------------------------------------------
-- Random Utilities
-------------------------------------------------------------------------------

-- |Execute an action with a fresh random generator.
-- Provides a seeded StdGen for reproducible randomness.
withRandomGen :: Int -> (StdGen -> a) -> a
withRandomGen seed f = f (mkStdGen seed)

-- Utility for comparison in sorting
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on cmp f x y = cmp (f x) (f y)
