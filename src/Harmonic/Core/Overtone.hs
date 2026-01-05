{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Harmonic.Core.Overtone
-- Description : Constructive generation of valid triads from overtone sets
-- 
-- This module implements the Rules (R) component of the Creative Systems
-- Framework for constraining the "search space" of possible harmonies.
--
-- KEY DESIGN DECISION: Constructive Generation
-- 
-- The legacy implementation used "generate all combinations, then filter"
-- which has O(n³) complexity. This module uses CONSTRUCTIVE generation:
-- only valid combinations are produced in the first place.
--
-- From the theoretical basis:
--   "Constructive generation means the generator only produces valid outputs
--    in the first place, rather than filtering invalid ones post-hoc."
--
-- The overtone series provides the "palette" of available tones, and
-- the combinatorial generator produces all valid 3-note subsets rooted
-- on a specified fundamental.

module Harmonic.Core.Overtone
  ( -- * Triad Generation
    possibleTriads
  , possibleTriads''   -- ^ Legacy alias for MusicData compatibility
  , possibleTriadsFrom
  , overtoneSets
  
    -- * Combination Utilities
  , nCr
  , combinations
  
    -- * Triad Selection
  , rankedTriads
  , topTriads
  ) where

import GHC.Generics (Generic)
import Data.List (sort, nub, sortBy)
import Data.Function (on)

import Harmonic.Core.Pitch (PitchClass(..), mkPitchClass, unPitchClass)
import Harmonic.Core.Dissonance (dissonanceLevel, mostConsonant, rankByConsonance)

-------------------------------------------------------------------------------
-- Combination Generator (nCr)
-------------------------------------------------------------------------------

-- |Generate all combinations of size n from a list.
-- This is the mathematical "n choose r" operation.
--
-- Implementation uses direct recursion for clarity:
--   * nCr 0 xs = [[]]           -- One way to choose nothing
--   * nCr n [] = []              -- Can't choose from empty
--   * nCr n (x:xs) = with x ++ without x
--
-- Ported from legacy MusicData.hs nCr function.
nCr :: Int -> [a] -> [[a]]
nCr 0 _      = [[]]
nCr _ []     = []
nCr n (x:xs) = map (x:) (nCr (n-1) xs) ++ nCr n xs

-- |Alias for nCr with more descriptive name
combinations :: Int -> [a] -> [[a]]
combinations = nCr

-------------------------------------------------------------------------------
-- Overtone Set Generation
-------------------------------------------------------------------------------

-- |Generate all valid subsets of size n from a fundamental and overtone palette.
-- 
-- CONSTRUCTIVE: This function directly builds valid sets rather than
-- generating all and filtering. Each set contains:
--   * Exactly one element from the fundamental list
--   * Exactly (n-1) elements from the overtone list
--   * No duplication of the fundamental in the overtone selection
--
-- Ported from legacy MusicData.hs (lines 382-385):
-- @
-- overtoneSets n rs ps = [ i:j | i <- rs,
--                          j <- sort <$> (nCr $ n-1) ps,
--                          not $ i `elem` j]
-- @
overtoneSets :: (Eq a, Ord a) => Int -> [a] -> [a] -> [[a]]
overtoneSets n roots overtones = 
  [ root : overtoneSet 
  | root <- roots
  , overtoneSet <- sort <$> nCr (n - 1) overtones
  , root `notElem` overtoneSet  -- Constructive constraint: no doubling
  ]

-------------------------------------------------------------------------------
-- Triad Generation (n=3 specialization)
-------------------------------------------------------------------------------

-- |Generate all possible triads rooted on a given fundamental.
-- 
-- Input: (fundamental pitch class, available overtone pitch classes)
-- Output: List of triads, each as [root, tone1, tone2] where:
--   * Root is the specified fundamental
--   * tone1 < tone2 (sorted)
--   * Neither tone equals root
--
-- This is the workhorse function called during ingestion to derive
-- harmonic interpretations from YCACL slices.
--
-- Ported from legacy MusicData.hs (lines 388-391):
-- @
-- possibleTriads'' (r, ps) =
--   let fund = (\x -> [x]) . fromIntegral $ r
--    in overtoneSets 3 fund ps
-- @
possibleTriads :: (Int, [Int]) -> [[Int]]
possibleTriads (root, overtones) =
  let fundList = [root `mod` 12]
      -- Remove fundamental from overtones to avoid doubling
      availableOvertones = filter (\x -> x `mod` 12 /= root `mod` 12) overtones
      -- Generate 2-element combinations from available overtones
  in overtoneSets 3 fundList (nub $ map (`mod` 12) availableOvertones)

-- |Alternative signature taking PitchClasses
possibleTriadsFrom :: PitchClass -> [PitchClass] -> [[PitchClass]]
possibleTriadsFrom root overtones =
  let rootInt = unPitchClass root
      overtoneInts = map unPitchClass overtones
      triads = possibleTriads (rootInt, overtoneInts)
  in map (map mkPitchClass) triads

-------------------------------------------------------------------------------
-- Ranked Triad Selection
-------------------------------------------------------------------------------

-- |Generate triads ranked by consonance (most consonant first).
-- Uses Hindemith dissonance scores from the Dissonance module.
rankedTriads :: (Int, [Int]) -> [[Int]]
rankedTriads input = rankByConsonance $ possibleTriads input

-- |Get the top N most consonant triads from a fundamental/overtone pair.
-- Used by the multi-triad branching logic (3/2/1 weighting).
--
-- Returns at most n triads, or fewer if not enough valid triads exist.
topTriads :: Int -> (Int, [Int]) -> [[Int]]
topTriads n input = take n $ rankedTriads input

-------------------------------------------------------------------------------
-- Utility: Count Valid Triads
-------------------------------------------------------------------------------

-- |Count how many valid triads can be formed from a fundamental/overtone pair.
-- Useful for diagnostic logging.
countPossibleTriads :: (Int, [Int]) -> Int
countPossibleTriads = length . possibleTriads

-------------------------------------------------------------------------------
-- Legacy Compatibility Aliases
-------------------------------------------------------------------------------

-- |Legacy alias for 'possibleTriads'.
-- Matches the signature from MusicData.hs for smooth migration:
-- @
-- possibleTriads'' :: (Integral a, Num a) => (a, [a]) -> [[a]]
-- @
--
-- This version converts to/from Int internally to maintain type safety
-- while preserving the polymorphic signature for backward compatibility.
possibleTriads'' :: (Integral a, Num a) => (a, [a]) -> [[a]]
possibleTriads'' (r, ps) =
  let intResult = possibleTriads (fromIntegral r, map fromIntegral ps)
  in map (map fromIntegral) intResult
