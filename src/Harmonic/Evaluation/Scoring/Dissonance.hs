{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Harmonic.Core.Dissonance
-- Description : Hindemith-based dissonance evaluation
-- 
-- This module implements the Evaluation (E) component of the Creative Systems
-- Framework for consonance/dissonance assessment.
--
-- The dissonance model is ported VERBATIM from legacy MusicData.hs (lines 394-401):
--   dissVect = [16,8,4,2,1,24]
--
-- This weighting vector is based on Paul Hindemith's "Craft of Musical Composition"
-- ranking of interval classes by consonance:
--   * Perfect unison/octave (ic 0): most consonant
--   * Perfect fifth (ic 7→1): 16
--   * Perfect fourth (ic 5→2): 8
--   * Major third/minor sixth (ic 4/8→3): 4
--   * Minor third/major sixth (ic 3/9→4): 2
--   * Major second/minor seventh (ic 2/10→5): 1
--   * Tritone (ic 6→6): 24 (most dissonant)

module Harmonic.Evaluation.Scoring.Dissonance
  ( -- * Core Dissonance Calculation
    dissonanceLevel
  , dissonanceScore
  
    -- * Hindemith Vector
  , hindemithVector
  
    -- * Interval Analysis
  , intervalVector
  , intervalClass
  
    -- * Root Motion Scoring
  , rootMotionVector
  , rootMotionScore
  
    -- * Selection
  , mostConsonant
  , rankByConsonance
  ) where

import GHC.Generics (Generic)
import Data.Function (on)
import Data.List (sortBy, sort, nub)

import Harmonic.Rules.Types.Pitch (PitchClass(..), mkPitchClass, unPitchClass)

-------------------------------------------------------------------------------
-- Hindemith Dissonance Model (VERBATIM from legacy MusicData.hs)
-------------------------------------------------------------------------------

-- |The Hindemith dissonance weighting vector.
-- Based on Paul Hindemith's ranking of interval classes.
-- 
-- Interval class mapping:
--   Index 0 → ic 1 (minor 2nd / major 7th) → weight 16
--   Index 1 → ic 2 (major 2nd / minor 7th) → weight 8
--   Index 2 → ic 3 (minor 3rd / major 6th) → weight 4
--   Index 3 → ic 4 (major 3rd / minor 6th) → weight 2
--   Index 4 → ic 5 (perfect 4th / perfect 5th) → weight 1
--   Index 5 → ic 6 (tritone) → weight 24
hindemithVector :: [Integer]
hindemithVector = [16, 8, 4, 2, 1, 24]
{-# INLINE hindemithVector #-}

-- |Root motion scoring vector for fallback generation.
-- Ranks interval classes by smoothness (lower = smoother movement).
--
-- Interval class mapping:
--   Index 0 → ic 1 (m2/M7) → weight 3 (stepwise)
--   Index 1 → ic 2 (M2/m7) → weight 3 (stepwise)
--   Index 2 → ic 3 (m3/M6) → weight 4 (moderate leap)
--   Index 3 → ic 4 (M3/m6) → weight 4 (moderate leap)
--   Index 4 → ic 5 (P4/P5) → weight 1 (strong harmonic motion)
--   Index 5 → ic 6 (TT)    → weight 6 (avoid)
--
-- Special cases:
--   ic 0 (Unison/Pedal) → weight 2 (encourages harmonic rhythm)
rootMotionVector :: [Integer]
rootMotionVector = [3, 3, 4, 4, 1, 6]
{-# INLINE rootMotionVector #-}

-- |Score root motion interval by smoothness.
-- Returns smoothness penalty (lower = better movement).
--
-- Examples:
--   rootMotionScore 0  == 2 (pedal - slight penalty)
--   rootMotionScore 7  == 1 (P5 - strongest movement)
--   rootMotionScore 5  == 1 (P4 - strong movement)
--   rootMotionScore 1  == 3 (m2 - stepwise)
--   rootMotionScore 2  == 3 (M2 - stepwise)
--   rootMotionScore 6  == 6 (TT - avoid)
rootMotionScore :: Int -> Integer
rootMotionScore n
  | ic == 0   = 2  -- Pedal: slight penalty to encourage harmonic rhythm
  | otherwise = rootMotionVector !! (ic - 1)
  where ic = intervalClass n
{-# INLINE rootMotionScore #-}

-------------------------------------------------------------------------------
-- Interval Vector (Set Theory)
-------------------------------------------------------------------------------

-- |Calculate the interval class (0-6) for an interval in semitones.
-- Interval classes fold intervals larger than a tritone to their complement.
intervalClass :: Int -> Int
intervalClass n
  | m <= 6    = m
  | otherwise = 12 - m
  where m = n `mod` 12
{-# INLINE intervalClass #-}

-- |Calculate the interval vector for a pitch class set.
-- Returns counts for interval classes [1..6].
-- 
-- The interval vector is the "fingerprint" of a pitch set, counting how many
-- times each interval class appears between all pairs of pitches.
--
-- Ported from legacy MusicData.hs (lines 349-358)
intervalVector :: [Int] -> [Integer]
intervalVector xs = [toInteger (vectCounts ic) | ic <- [1..6]]
  where
    pitches = sort $ nub $ map (`mod` 12) xs
    -- All pairs of pitches
    pairs = [(a, b) | a <- pitches, b <- pitches, a < b]
    -- Interval between each pair, folded to interval class
    intervals = [intervalClass (b - a) | (a, b) <- pairs]
    -- Count occurrences of each interval class
    vectCounts ic = length $ filter (== ic) intervals

-- |Alternative interval vector from pitch classes directly
intervalVectorPC :: [PitchClass] -> [Integer]
intervalVectorPC pcs = intervalVector $ map unPitchClass pcs

-------------------------------------------------------------------------------
-- Dissonance Calculation (VERBATIM PRINCIPLE from legacy MusicData.hs)
-------------------------------------------------------------------------------

-- |Calculate dissonance level for a pitch set.
-- Returns (dissonance score, original pitches) for sorting/selection.
--
-- The calculation:
--   1. Compute interval vector [ic1, ic2, ic3, ic4, ic5, ic6]
--   2. Dot product with Hindemith weights [16, 8, 4, 2, 1, 24]
--   3. Special case: if only one interval class is present and it's the fifth,
--      subtract 1 (bonus for "pure" fifths)
--   4. Special case: if interval vector is all zeros except one slot,
--      return 27 (penalty for degenerate sets)
--
-- VERBATIM from legacy MusicData.hs (lines 394-401):
-- @
-- dissonanceLevel xs
--   | countElem iVect 0 == 5 = (27, xs)
--   | elem (7+head xs) xs    = (subtract 1 $ sum $ zipWith (*) dissVect iVect, xs)
--   | otherwise              = (sum $ zipWith (*) dissVect iVect, xs)
-- @
dissonanceLevel :: [Int] -> (Integer, [Int])
dissonanceLevel xs
  | countElem iVect 0 == 5 = (27, xs)  -- Degenerate: only one interval class
  | hasPerfectFifth xs     = (baseDiss - 1, xs)  -- Bonus for containing P5
  | otherwise              = (baseDiss, xs)
  where
    iVect    = intervalVector xs
    baseDiss = sum $ zipWith (*) hindemithVector iVect

-- |Simplified dissonance score (just the number, not paired with input)
dissonanceScore :: [Int] -> Integer
dissonanceScore = fst . dissonanceLevel

-- |Check if a pitch set contains a perfect fifth above the root
hasPerfectFifth :: [Int] -> Bool
hasPerfectFifth [] = False
hasPerfectFifth (root:rest) = 
  let p5 = (root + 7) `mod` 12
  in any (\x -> x `mod` 12 == p5) rest

-- |Count occurrences of an element in a list
countElem :: Eq a => [a] -> a -> Int
countElem xs x = length $ filter (== x) xs

-------------------------------------------------------------------------------
-- Consonance Selection
-------------------------------------------------------------------------------

-- |Select the most consonant option from a list of pitch sets.
-- Used by triad generation to pick the "best" interpretation.
--
-- Ported from legacy MusicData.hs (lines 403-406):
-- @
-- mostConsonant xs = triadChoice . sortFst $ dissonanceLevel <$> xs
--   where triadChoice xs = (snd . head . sortFst) xs
--         sortFst xs     = List.sortBy (compare `on` fst) xs
-- @
mostConsonant :: [[Int]] -> [Int]
mostConsonant [] = [0, 4, 7]  -- Default to major triad if no options
mostConsonant xs = snd . head . sortByFst $ map dissonanceLevel xs
  where
    sortByFst = sortBy (compare `on` fst)

-- |Rank a list of pitch sets by consonance (most consonant first)
rankByConsonance :: [[Int]] -> [[Int]]
rankByConsonance xs = map snd $ sortBy (compare `on` fst) $ map dissonanceLevel xs

-- |Rank and return with scores for debugging/inspection
rankByConsonanceWithScores :: [[Int]] -> [(Integer, [Int])]
rankByConsonanceWithScores xs = sortBy (compare `on` fst) $ map dissonanceLevel xs
