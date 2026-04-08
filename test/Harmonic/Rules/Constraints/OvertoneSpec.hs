-- |
-- Module      : Harmonic.Core.OvertoneSpec
-- Description : Invariant tests for constructive generation
--
-- Validates that possibleTriads and overtoneSets produce ONLY valid
-- outputs, enforcing the "constructive generation" principle.

module Harmonic.Rules.Constraints.OvertoneSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.List (nub, sort)

import Harmonic.Rules.Types.Pitch (PitchClass(..), mkPitchClass)
import Harmonic.Rules.Constraints.Overtone
import Harmonic.Evaluation.Scoring.Dissonance (dissonanceScore)

-------------------------------------------------------------------------------
-- Constructive Generation Invariants
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "possibleTriads Invariants" $ do
    
    it "every triad contains the specified root" $ do
      let triads = possibleTriads (0, [1,2,3,4,5,6,7,8,9,10,11])
      all (\t -> head t == 0) triads `shouldBe` True
    
    it "every triad has exactly 3 elements" $ do
      let triads = possibleTriads (0, [4,7,10,2])
      all (\t -> length t == 3) triads `shouldBe` True
    
    it "no duplicate pitch classes within any triad" $ do
      let triads = possibleTriads (0, [1,2,3,4,5,6,7,8,9,10,11])
      all (\t -> nub t == t) triads `shouldBe` True
    
    it "produces non-empty result from valid overtones" $ do
      let triads = possibleTriads (0, [4,7])  -- C, E, G available
      triads `shouldNotBe` []
    
    it "produces empty result when insufficient overtones" $ do
      let triads = possibleTriads (0, [4])  -- Only one overtone, need 2
      triads `shouldBe` []
    
    it "does not include root in overtone selections" $ do
      -- Even if root appears in overtones list, it shouldn't be doubled
      let triads = possibleTriads (0, [0,4,7,10])
      all (\t -> length (filter (== 0) t) == 1) triads `shouldBe` True
    
  describe "overtoneSets Generalization" $ do
    
    it "overtoneSets 3 produces triads" $ do
      let sets = overtoneSets 3 [0] [4,7,10,2]
      all (\s -> length s == 3) sets `shouldBe` True
    
    it "overtoneSets 4 produces tetrads" $ do
      let sets = overtoneSets 4 [0] [4,7,10,2,5]
      all (\s -> length s == 4) sets `shouldBe` True
    
    it "root is always first element" $ do
      let sets = overtoneSets 3 [5] [0,4,7,9]  -- Root = F
      all (\s -> head s == 5) sets `shouldBe` True
    
  describe "nCr Combinations" $ do
    
    it "nCr 2 [1,2,3] = [[1,2],[1,3],[2,3]]" $ do
      nCr 2 [1,2,3] `shouldMatchList` [[1,2],[1,3],[2,3]]
    
    it "nCr 0 xs = [[]] (one way to choose nothing)" $ do
      nCr 0 [1,2,3] `shouldBe` [[]]
    
    it "nCr n [] = [] when n > 0" $ do
      nCr 3 ([] :: [Int]) `shouldBe` []
    
    it "length (nCr k xs) = C(n,k)" $ do
      -- C(5,3) = 10
      length (nCr 3 [1,2,3,4,5]) `shouldBe` 10
    
  describe "rankedTriads Ordering" $ do
    
    it "rankedTriads returns most consonant first" $ do
      -- Major triad [0,4,7] should be more consonant than [0,1,2]
      let ranked = rankedTriads (0, [1,2,3,4,5,6,7,8,9,10,11])
      let scores = map dissonanceScore ranked
      -- First should have lower (better) score than later entries
      scores `shouldSatisfy` isSortedAscending
    
    it "topTriads 3 returns at most 3 triads" $ do
      let top = topTriads 3 (0, [1,2,3,4,5,6,7,8,9,10,11])
      length top `shouldSatisfy` (<= 3)
    
    it "topTriads n on limited input returns what's available" $ do
      let top = topTriads 10 (0, [4,7])  -- Only 1 triad possible
      length top `shouldBe` 1
    
  describe "possibleTriadsFrom (PitchClass version)" $ do
    
    it "produces same structure as possibleTriads" $ do
      let triadsInt = possibleTriads (0, [4,7,10])
      let triadsPc = possibleTriadsFrom (P 0) [P 4, P 7, P 10]
      length triadsInt `shouldBe` length triadsPc

  describe "annotateOvertones" $ do

    it "E2 produces B (pitch class 11)" $ do
      let result = annotateOvertones [("E", 4)] [11]
      result `shouldBe` [(11, [("E", 2)])]

    it "finds multiple sources for shared pitch class" $ do
      let result = annotateOvertones [("E", 4), ("G", 7)] [11]
      -- B(11): E + 7 = 11 → OT2, G + 4 = 11 → OT3
      result `shouldBe` [(11, [("E", 2), ("G", 3)])]

    it "returns empty sources for unproducible pitch" $ do
      let result = annotateOvertones [("E", 4)] [3]
      -- Eb(3): (3-4) mod 12 = 11, not in offsets [0,7,4,10,2]
      result `shouldBe` [(3, [])]

    it "annotates full EADG chord correctly" $ do
      let tuning = [("E",4),("A",9),("D",2),("G",7)]
          result = annotateOvertones tuning [7]
      -- G(7): A + 10 = 7 → OT4, G + 0 = 7 → OT1
      result `shouldBe` [(7, [("A", 4), ("G", 1)])]

  describe "formatOvertoneAnnotation" $ do

    it "formats single source" $ do
      let tuning = [("G", 7)]
          pcName pc = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"] !! pc
      formatOvertoneAnnotation tuning [7] pcName `shouldBe` "{G: G1}"

    it "formats multiple sources with / separator" $ do
      let tuning = [("E", 4), ("G", 7)]
          pcName pc = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"] !! pc
      formatOvertoneAnnotation tuning [11] pcName `shouldBe` "{B: E2/G3}"

    it "returns empty string for unproducible pitches" $ do
      let tuning = [("E", 4)]
          pcName pc = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"] !! pc
      formatOvertoneAnnotation tuning [3] pcName `shouldBe` ""

    it "returns empty string for empty tuning" $ do
      formatOvertoneAnnotation [] [0, 4, 7] (\_ -> "?") `shouldBe` ""

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

isSortedAscending :: Ord a => [a] -> Bool
isSortedAscending xs = and $ zipWith (<=) xs (tail xs)
