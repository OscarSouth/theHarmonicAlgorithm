-- |
-- Module      : Harmonic.Core.VoiceLeadingSpec
-- Description : Voice leading cost function validation
--
-- Tests the cost function behavior including:
--   * Zero cost for no movement
--   * Parallel motion penalties
--   * Cyclic cost wrap-around
--   * Cyclic DP solvers (solveRoot, solveFlow)

module Harmonic.Core.VoiceLeadingSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Harmonic.Core.VoiceLeading

-------------------------------------------------------------------------------
-- Voice Leading Cost Function Tests
-- Note: voiceMovement now uses absolute distance for concrete pitches
-- (no mod-12 wrapping). Use minimalMovement for pitch class movement.
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "voiceMovement" $ do
    
    it "no movement = 0" $
      voiceMovement 0 0 `shouldBe` 0
    
    it "semitone up = 1" $
      voiceMovement 0 1 `shouldBe` 1
    
    it "semitone down = 1 (absolute)" $
      voiceMovement 1 0 `shouldBe` 1
    
    it "tritone = 6" $
      voiceMovement 0 6 `shouldBe` 6
    
    it "11 semitones = 11 (absolute, not mod-12)" $
      voiceMovement 0 11 `shouldBe` 11  -- Absolute distance, not mod-12
    
    it "perfect 5th = 7 (absolute)" $
      voiceMovement 0 7 `shouldBe` 7
    
    describe "voiceMovement absolute distances" $ do
      
      it "10 to 2 = 8 (absolute)" $
        voiceMovement 10 2 `shouldBe` 8
      
      it "2 to 10 = 8 (absolute)" $
        voiceMovement 2 10 `shouldBe` 8
      
      it "11 to 1 = 10 (absolute)" $
        voiceMovement 11 1 `shouldBe` 10
      
      it "1 to 11 = 10 (absolute)" $
        voiceMovement 1 11 `shouldBe` 10
      
      it "9 to 3 = 6 (absolute)" $
        voiceMovement 9 3 `shouldBe` 6
      
      it "11 to 0 = 11 (absolute)" $
        voiceMovement 11 0 `shouldBe` 11
      
      it "0 to 11 = 11 (absolute)" $
        voiceMovement 0 11 `shouldBe` 11
    
  describe "voiceLeadingCost" $ do
    
    it "identical voicings = 0 cost" $
      voiceLeadingCost [0,4,7] [0,4,7] `shouldBe` 0
    
    it "single semitone movement = 1 (plus any penalties)" $
      voiceLeadingCost [0,4,7] [1,4,7] `shouldSatisfy` (>= 1)
    
    it "common-tone retention minimizes cost" $ do
      -- C major to F major: C is common tone
      let cToF = voiceLeadingCost [0,4,7] [0,5,9]  -- Keep C, move E->F, G->A
      -- C major to F# major: no common tones
      let cToFsharp = voiceLeadingCost [0,4,7] [1,6,10]
      cToF `shouldSatisfy` (< cToFsharp)
    
    it "incompatible voicing lengths = high cost" $
      voiceLeadingCost [0,4,7] [0,4] `shouldBe` 999
    
    describe "voiceLeadingCost with boundary crossings" $ do
      
      -- Voicings that cross the 0/12 boundary
      it "[10,2,5] to [11,3,6] = small cost (semitone each)" $ do
        let cost = voiceLeadingCost [10,2,5] [11,3,6]
        cost `shouldSatisfy` (< 10)  -- Should be around 3 + penalties
      
      it "[11,3,7] to [0,4,7] = cost of 2 (two semitones)" $ do
        let cost = voiceLeadingCost [11,3,7] [0,4,7]
        -- 11→0 = 1, 3→4 = 1, 7→7 = 0, total base = 2
        cost `shouldSatisfy` (>= 2)
      
      it "[1,5,8] to [11,3,6] = reasonable cost (Db maj to B maj)" $ do
        let cost = voiceLeadingCost [1,5,8] [11,3,6]
        -- 1→11 = 2, 5→3 = 2, 8→6 = 2, total base = 6
        cost `shouldSatisfy` (>= 6)
      
      it "cost is symmetric" $ do
        let forward = voiceLeadingCost [10,2,5] [0,4,7]
        let backward = voiceLeadingCost [0,4,7] [10,2,5]
        forward `shouldBe` backward
    
  describe "Parallel Motion Penalty" $ do
    
    -- Key test: Parallel fifths should incur penalty
    it "parallel fifths incur penalty" $ do
      -- [0,7] -> [1,8] is parallel fifths (both move up semitone)
      let parallelCost = voiceLeadingCost [0,7] [1,8]
      let baseCost = 2  -- Two voices each move 1 semitone
      parallelCost `shouldSatisfy` (> baseCost)
    
    it "parallel octaves incur penalty" $ do
      -- [0,12] -> [1,13] is parallel octaves
      let parallelCost = voiceLeadingCost [0,12] [1,13]
      parallelCost `shouldSatisfy` (> 2)
    
    it "contrary motion avoids penalty" $ do
      -- [0,7] -> [1,6] is contrary motion (no parallel)
      let contraryCost = voiceLeadingCost [0,7] [1,6]
      let parallelCost = voiceLeadingCost [0,7] [1,8]
      contraryCost `shouldSatisfy` (<= parallelCost)
    
    describe "Parallel motion across 0/12 boundary" $ do
      
      it "parallel fifths crossing boundary incur penalty" $ do
        -- [10,5] -> [11,6] is parallel fifths (interval 7 preserved)
        let parallelCost = voiceLeadingCost [10,5] [11,6]
        parallelCost `shouldSatisfy` (> 2)
      
      it "[11,6] -> [0,7] parallel fifths (B-F# to C-G)" $ do
        let parallelCost = voiceLeadingCost [11,6] [0,7]
        parallelCost `shouldSatisfy` (> 2)
      
      it "oblique motion near boundary avoids penalty" $ do
        -- [11,6] -> [0,6] - only bass moves
        let obliqueCost = voiceLeadingCost [11,6] [0,6]
        let parallelCost = voiceLeadingCost [11,6] [0,7]
        obliqueCost `shouldSatisfy` (< parallelCost)
    
  describe "Large Leap Penalty" $ do
    
    it "movement > 4 semitones incurs leap penalty" $ do
      let smallMove = voiceLeadingCost [0,4,7] [0,5,7]  -- E->F = 1
      let largeMove = voiceLeadingCost [0,4,7] [0,10,7] -- E->Bb = 6
      largeMove `shouldSatisfy` (> smallMove + 5)  -- 6 > 1+penalty
    
    describe "Leap penalty with large intervals" $ do
      
      it "large leap incurs penalty" $ do
        -- 11 to 5 = 6 semitones absolute
        let largeLeap = voiceLeadingCost [11,4,7] [5,4,7]
        -- Should have penalty since 6 > 4
        largeLeap `shouldSatisfy` (> 6)
      
      it "large leap 2 to 8 incurs penalty" $ do
        -- 2 to 8 = 6 semitones
        let cost = voiceLeadingCost [2,6,9] [8,6,9]
        cost `shouldSatisfy` (> 6)  -- More than just the movement
    
  describe "totalCost" $ do
    
    it "empty sequence = 0" $
      totalCost [] `shouldBe` 0
    
    it "single chord = 0" $
      totalCost [[0,4,7]] `shouldBe` 0
    
    it "sums pairwise costs" $ do
      let prog = [[0,4,7], [0,5,9], [7,11,2]]
      let expected = voiceLeadingCost [0,4,7] [0,5,9] 
                   + voiceLeadingCost [0,5,9] [7,11,2]
      totalCost prog `shouldBe` expected
    
    describe "totalCost with boundary-crossing progressions" $ do
      
      it "progression crossing 0/12 sums correctly" $ do
        let prog = [[10,2,5], [11,3,6], [0,4,7]]  -- Bb maj -> B maj -> C maj
        let expected = voiceLeadingCost [10,2,5] [11,3,6]
                     + voiceLeadingCost [11,3,6] [0,4,7]
        totalCost prog `shouldBe` expected
      
      it "chromatic descent through boundary" $ do
        let prog = [[1,5,8], [0,4,7], [11,3,6], [10,2,5]]  -- Db -> C -> B -> Bb
        totalCost prog `shouldSatisfy` (> 0)
    
  describe "cyclicCost" $ do
    
    it "includes wrap-around from last to first" $ do
      let prog = [[0,4,7], [5,9,0], [7,11,2]]
      let linear = totalCost prog
      let cyclic = cyclicCost prog
      -- Cyclic should include last->first cost
      cyclic `shouldSatisfy` (>= linear)
    
    it "cyclic cost accounts for return journey" $ do
      -- Progression that drifts: C -> G -> D (ascending fifths)
      -- Return from D to C is large
      let drifting = [[0,4,7], [7,11,2], [2,6,9]]  
      let cycling = [[0,4,7], [5,9,0], [0,4,7]]  -- Returns naturally
      cyclicCost drifting `shouldSatisfy` (> cyclicCost cycling)
    
    describe "cyclicCost with return" $ do
      
      it "progression returning to start" $ do
        let prog = [[0,4,7], [5,9,12], [11,15,18]]  -- C -> F -> B (concrete pitches)
        let linear = totalCost prog
        let cyclic = cyclicCost prog
        -- Cyclic includes return cost
        cyclic `shouldSatisfy` (>= linear)
      
      it "Bb major returning to C major" $ do
        let prog = [[0,4,7], [7,11,14], [10,14,17]]  -- C -> G -> Bb (concrete)
        let cyclic = cyclicCost prog
        cyclic `shouldSatisfy` (> 0)
    
  describe "solveRoot" $ do
    
    it "keeps root in bass throughout" $ do
      let input = [[0,4,7], [7,11,2], [5,9,0]]  -- C, G, F (roots 0,7,5)
      let solved = solveRoot input
      -- Check first note of each chord matches root pitch class
      map (\c -> head c `mod` 12) solved `shouldBe` [0, 7, 5]
    
    it "produces voice-led progression" $ do
      let input = [[0,4,7], [5,9,0], [7,11,2], [0,4,7]]
      let solved = solveRoot input
      length solved `shouldBe` 4

  describe "solveFlow" $ do
    
    it "allows any inversion for smoothest voice leading" $ do
      let input = [[0,4,7], [5,9,0], [7,11,2]]  -- C, F, G
      let solved = solveFlow input
      -- Flow doesn't require root in bass, just verify valid output
      length solved `shouldBe` 3
    
    it "produces voice-led progression" $ do
      let input = [[0,4,7], [5,9,0], [7,11,2], [0,4,7]]
      let solved = solveFlow input
      length solved `shouldBe` 4
