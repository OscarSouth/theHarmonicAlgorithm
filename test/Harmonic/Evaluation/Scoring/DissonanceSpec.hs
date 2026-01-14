module Harmonic.Evaluation.Scoring.DissonanceSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Harmonic.Evaluation.Scoring.Dissonance as D
import qualified Harmonic.Rules.Types.Pitch as P

spec :: Spec
spec = do
  describe "intervalClass" $ do
    it "maps 0 to 0 (unison)" $
      D.intervalClass 0 `shouldBe` 0
    
    it "maps 1 to 1 (minor second)" $
      D.intervalClass 1 `shouldBe` 1
    
    it "maps 6 to 6 (tritone)" $
      D.intervalClass 6 `shouldBe` 6
    
    it "maps 7 to 5 (perfect fifth → perfect fourth)" $
      D.intervalClass 7 `shouldBe` 5
    
    it "maps 11 to 1 (major seventh → minor second)" $
      D.intervalClass 11 `shouldBe` 1
    
    it "wraps mod 12" $
      D.intervalClass 13 `shouldBe` 1
    
    prop "always returns 0-6" $ \n ->
      let ic = D.intervalClass n
      in ic >= 0 && ic <= 6

  describe "hindemithVector" $ do
    it "has correct length (6 interval classes)" $
      length D.hindemithVector `shouldBe` 6
    
    it "has correct values [16,8,4,2,1,24]" $
      D.hindemithVector `shouldBe` [16, 8, 4, 2, 1, 24]
    
    it "tritone (index 5) is most dissonant (24)" $
      D.hindemithVector !! 5 `shouldBe` 24
    
    it "perfect fifth (index 4) is most consonant (1)" $
      D.hindemithVector !! 4 `shouldBe` 1

  describe "rootMotionVector" $ do
    it "has correct length (6 interval classes)" $
      length D.rootMotionVector `shouldBe` 6
    
    it "has correct values [3,3,4,4,1,6]" $
      D.rootMotionVector `shouldBe` [3, 3, 4, 4, 1, 6]
    
    it "perfect fifth/fourth (index 4) is smoothest (1)" $
      D.rootMotionVector !! 4 `shouldBe` 1
    
    it "tritone (index 5) is least smooth (6)" $
      D.rootMotionVector !! 5 `shouldBe` 6
    
    it "stepwise motion (indices 0-1) has moderate penalty (3)" $ do
      D.rootMotionVector !! 0 `shouldBe` 3
      D.rootMotionVector !! 1 `shouldBe` 3

  describe "rootMotionScore" $ do
    it "pedal (0 semitones) scores 2" $
      D.rootMotionScore 0 `shouldBe` 2
    
    it "minor second (1 semitone) scores 3" $
      D.rootMotionScore 1 `shouldBe` 3
    
    it "major second (2 semitones) scores 3" $
      D.rootMotionScore 2 `shouldBe` 3
    
    it "minor third (3 semitones) scores 4" $
      D.rootMotionScore 3 `shouldBe` 4
    
    it "major third (4 semitones) scores 4" $
      D.rootMotionScore 4 `shouldBe` 4
    
    it "perfect fourth (5 semitones) scores 1" $
      D.rootMotionScore 5 `shouldBe` 1
    
    it "tritone (6 semitones) scores 6" $
      D.rootMotionScore 6 `shouldBe` 6
    
    it "perfect fifth (7 semitones) scores 1" $
      D.rootMotionScore 7 `shouldBe` 1
    
    it "major sixth (9 semitones) scores 4 (ic=3)" $
      D.rootMotionScore 9 `shouldBe` 4
    
    it "minor seventh (10 semitones) scores 3 (ic=2)" $
      D.rootMotionScore 10 `shouldBe` 3
    
    it "major seventh (11 semitones) scores 3 (ic=1)" $
      D.rootMotionScore 11 `shouldBe` 3
    
    prop "always returns 1-6" $ \n ->
      let score = D.rootMotionScore n
      in score >= 1 && score <= 6

  describe "dissonanceScore - Basic Triads" $ do
    it "major triad [0,4,7] is consonant" $
      D.dissonanceScore [0, 4, 7] `shouldBe` 6
    
    it "minor triad [0,3,7] is consonant" $
      D.dissonanceScore [0, 3, 7] `shouldBe` 6
    
    it "diminished triad [0,3,6] is more dissonant" $
      D.dissonanceScore [0, 3, 6] `shouldBe` 32
    
    it "augmented triad [0,4,8] is more dissonant" $
      D.dissonanceScore [0, 4, 8] `shouldBe` 27
    
    it "sus2 triad [0,2,7] is consonant" $
      D.dissonanceScore [0, 2, 7] `shouldBe` 9
    
    it "sus4 triad [0,5,7] is consonant" $
      D.dissonanceScore [0, 5, 7] `shouldBe` 9

  describe "dissonanceScore - Ordering" $ do
    it "major triad is more consonant than diminished" $
      D.dissonanceScore [0, 4, 7] `shouldSatisfy` (< D.dissonanceScore [0, 3, 6])
    
    it "minor triad is more consonant than augmented" $
      D.dissonanceScore [0, 3, 7] `shouldSatisfy` (< D.dissonanceScore [0, 4, 8])
    
    it "perfect fifth [0,7] is most consonant interval" $
      D.dissonanceScore [0, 7] `shouldBe` 27  -- Degenerate case (only one ic)
    
    it "tritone [0,6] is most dissonant interval" $
      D.dissonanceScore [0, 6] `shouldBe` 27  -- Degenerate case (only one ic)
    
    it "cluster [0,1,2] is highly dissonant" $
      D.dissonanceScore [0, 1, 2] `shouldSatisfy` (> 20)

  describe "dissonanceScore - Perfect Fifth Bonus" $ do
    it "triads containing P5 get -1 bonus" $ do
      let withP5 = D.dissonanceScore [0, 5, 7]  -- contains P5
          withoutP5 = D.dissonanceScore [0, 3, 6]  -- no P5
      withP5 `shouldSatisfy` (< withoutP5)  -- Should be more consonant

  describe "intervalVector" $ do
    it "major triad [0,4,7] has vector [0,0,1,1,1,0]" $
      D.intervalVector [0, 4, 7] `shouldBe` [0, 0, 1, 1, 1, 0]
    
    it "minor triad [0,3,7] has vector [0,0,1,1,1,0]" $
      D.intervalVector [0, 3, 7] `shouldBe` [0, 0, 1, 1, 1, 0]
    
    it "diminished triad [0,3,6] has vector [0,0,2,0,0,1]" $
      D.intervalVector [0, 3, 6] `shouldBe` [0, 0, 2, 0, 0, 1]
    
    it "whole tone scale subset [0,2,4] has vector [0,2,0,1,0,0]" $
      D.intervalVector [0, 2, 4] `shouldBe` [0, 2, 0, 1, 0, 0]
    
    prop "interval vector always has 6 elements" $ \pcs ->
      let normalized = map (`mod` 12) pcs
      in length (D.intervalVector normalized) == 6

  describe "mostConsonant" $ do
    it "selects major over diminished" $
      D.mostConsonant [[0, 4, 7], [0, 3, 6]] `shouldBe` [0, 4, 7]
    
    it "selects minor over augmented" $
      D.mostConsonant [[0, 3, 7], [0, 4, 8]] `shouldBe` [0, 3, 7]
    
    it "returns major triad as default for empty list" $
      D.mostConsonant ([] :: [[Int]]) `shouldBe` [0, 4, 7]
    
    it "handles single element" $
      D.mostConsonant [[0, 4, 7]] `shouldBe` [0, 4, 7]

  describe "rankByConsonance" $ do
    it "sorts triads by consonance (ascending)" $ do
      let triads = [[0, 3, 6], [0, 4, 7], [0, 1, 2], [0, 3, 7]]
          ranked = D.rankByConsonance triads
          scores = map D.dissonanceScore ranked
      scores `shouldSatisfy` isSortedAscending
    
    it "major and minor triads rank equally" $ do
      let triads = [[0, 4, 7], [0, 3, 7]]
          ranked = D.rankByConsonance triads
      length ranked `shouldBe` 2

  describe "Root Motion Scoring - Musical Examples" $ do
    it "fifth motion is strongest (1)" $
      D.rootMotionScore 7 `shouldBe` 1
    
    it "fourth motion is strong (1)" $
      D.rootMotionScore 5 `shouldBe` 1
    
    it "stepwise motion is moderate (3)" $ do
      D.rootMotionScore 1 `shouldBe` 3  -- m2
      D.rootMotionScore 2 `shouldBe` 3  -- M2
    
    it "third motion is moderate (4)" $ do
      D.rootMotionScore 3 `shouldBe` 4  -- m3
      D.rootMotionScore 4 `shouldBe` 4  -- M3
    
    it "tritone is avoided (6)" $
      D.rootMotionScore 6 `shouldBe` 6
    
    it "pedal has slight penalty (2)" $
      D.rootMotionScore 0 `shouldBe` 2

  describe "Normalization Ranges" $ do
    it "chord dissonance minimum is 6 (major/minor triad)" $
      D.dissonanceScore [0, 4, 7] `shouldBe` 6
    
    it "chord dissonance for cluster is high" $
      D.dissonanceScore [0, 1, 2] `shouldSatisfy` (> 20)
    
    it "root motion minimum is 1 (P5/P4)" $ do
      D.rootMotionScore 5 `shouldBe` 1
      D.rootMotionScore 7 `shouldBe` 1
    
    it "root motion maximum is 6 (tritone)" $
      D.rootMotionScore 6 `shouldBe` 6

-- Helper: Check if list is sorted in ascending order
isSortedAscending :: Ord a => [a] -> Bool
isSortedAscending xs = and $ zipWith (<=) xs (tail xs)
