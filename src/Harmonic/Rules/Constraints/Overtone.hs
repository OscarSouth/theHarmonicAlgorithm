{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Harmonic.Rules.Constraints.Overtone
-- Description : Constructive generation of valid triads from overtone sets
--
-- This module implements the Rules (R) component of the Creative Systems
-- Framework for constraining the "search space" of possible harmonies.
--
-- == Academic Lineage
--
-- /The Harmonic Algorithm/ (South, 2016), Section Two: the exhaustive
-- combinatorial charts of 3-note overtone combinations across 12 chromatic
-- bass notes for EAeGB, EAeGC, and EADG tunings. This module is the
-- computational realisation of those charts.
--
-- /Data Science In The Creative Process/ (South, 2018): the @overtoneSets@
-- function is ported from the MusicData module (lines 382-385).
--
-- == Design
--
-- Constructive generation: only valid combinations are produced in the
-- first place, avoiding O(n³) generate-then-filter.
--
-- The overtone series provides the "palette" of available tones, and
-- the combinatorial generator produces all valid 3-note subsets rooted
-- on a specified fundamental. The 'annotateOvertones' function provides
-- reverse-mapping from pitch classes back to string/overtone sources,
-- using the thesis notation (@E3\/e1@, @G1+3@).

module Harmonic.Rules.Constraints.Overtone
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

    -- * Overtone Annotation
  , annotateOvertones
  , formatOvertoneAnnotation
  ) where

import GHC.Generics (Generic)
import Data.List (sort, nub, sortBy, intercalate)
import Data.Function (on)

import Harmonic.Rules.Types.Pitch (PitchClass(..), mkPitchClass, unPitchClass)
import Harmonic.Evaluation.Scoring.Dissonance (dissonanceLevel, mostConsonant, rankByConsonance)

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

-------------------------------------------------------------------------------
-- Overtone Annotation
-------------------------------------------------------------------------------

-- |Annotate pitch classes with their possible overtone sources from a tuning.
--
-- For each pitch in the chord, finds all (stringName, overtoneNumber) pairs
-- where that string's overtone series contains the pitch class.
--
-- Overtone numbering follows the thesis convention:
--   OT1 = fundamental (offset 0), OT2 = P5 (offset 7),
--   OT3 = M3 (offset 4), OT4 = m7 (offset 10), OT5 = M2 (offset 2)
--
-- Example:
-- @
-- annotateOvertones [("E",4),("A",9),("D",2),("G",7)] [11,7,2]
-- -- → [(11,[("E",2),("D",4)]), (7,[("G",1)]), (2,[("E",5),("D",1)])]
-- @
annotateOvertones :: [(String, Int)] -> [Int] -> [(Int, [(String, Int)])]
annotateOvertones tuning pitches = map annotate pitches
  where
    otOffsets = zip [1..5] [0, 7, 4, 10, 2 :: Int]
    annotate p = (p, concatMap (sourcesFor p) tuning)
    sourcesFor p (name, fund) =
      [ (name, otNum)
      | (otNum, offset) <- otOffsets
      , (p - fund) `mod` 12 == offset
      ]

-- |Format overtone annotation for a chord as a display string.
--
-- Uses thesis notation:
--   @"/"@ separates alternative sources from different strings
--   @"+"@ connects multiple overtone numbers from the same string
--
-- Example output: @"{B: E2/D4, G: G1, D: E5/D1}"@
formatOvertoneAnnotation :: [(String, Int)] -> [Int] -> (Int -> String) -> String
formatOvertoneAnnotation tuning pitches pcToName =
  let annotated = annotateOvertones tuning pitches
      entries = [ pcToName p ++ ": " ++ formatSources sources
                | (p, sources) <- annotated
                , not (null sources)
                ]
  in if null entries then "" else "{" ++ intercalate ", " entries ++ "}"
  where
    formatSources sources =
      let grouped = groupByString sources
      in intercalate "/" [ name ++ formatNums nums | (name, nums) <- grouped ]
    formatNums [n] = show n
    formatNums ns  = intercalate "+" (map show ns)
    groupByString sources =
      let names = nub [name | (name, _) <- sources]
      in [ (name, [num | (n, num) <- sources, n == name]) | name <- names ]
