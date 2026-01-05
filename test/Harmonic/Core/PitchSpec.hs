{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Harmonic.Core.PitchSpec
-- Description : QuickCheck properties for ℤ₁₂ algebraic laws
--
-- Validates that PitchClass behaves as a proper cyclic group:
--   * Closure under modulo 12
--   * Transpose as group action
--   * Invert as involution
--   * Interval as inverse operation

module Harmonic.Core.PitchSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Harmonic.Core.Pitch

-------------------------------------------------------------------------------
-- Arbitrary Instance for PitchClass
-------------------------------------------------------------------------------

instance Arbitrary PitchClass where
  arbitrary = mkPitchClass <$> choose (0, 11)

-------------------------------------------------------------------------------
-- ℤ₁₂ Group Properties
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "ℤ₁₂ Group Laws" $ do
    
    prop "mkPitchClass wraps modulo 12" $ \(n :: Int) ->
      mkPitchClass n == mkPitchClass (n + 12)
    
    prop "mkPitchClass always produces value in [0,11]" $ \(n :: Int) ->
      let (P v) = mkPitchClass n
      in v >= 0 && v <= 11
    
    prop "invert is an involution (invert . invert = id)" $ \(p :: PitchClass) ->
      invert (invert p) == p
    
    prop "invert produces octave complement" $ \(p :: PitchClass) ->
      let (P n) = p
          (P inv) = invert p
      in (n + inv) `mod` 12 == 0
    
  describe "Transpose (Group Action)" $ do
    
    prop "transpose 0 is identity" $ \(p :: PitchClass) ->
      transpose 0 p == p
    
    prop "transpose 12 is identity (octave equivalence)" $ \(p :: PitchClass) ->
      transpose 12 p == p
    
    prop "transpose composes additively" $ \(a :: Int) (b :: Int) (p :: PitchClass) ->
      transpose a (transpose b p) == transpose (a + b) p
    
    prop "transpose by negative is inverse" $ \(n :: Int) (p :: PitchClass) ->
      transpose n (transpose (-n) p) == p
    
  describe "Interval Measurement" $ do
    
    prop "interval p p = 0 (reflexive)" $ \(p :: PitchClass) ->
      interval p p == P 0
    
    prop "interval is antisymmetric mod 12" $ \(p :: PitchClass) (q :: PitchClass) ->
      let (P i1) = interval p q
          (P i2) = interval q p
      in (i1 + i2) `mod` 12 == 0
    
    prop "interval relates to transpose" $ \(p :: PitchClass) (q :: PitchClass) ->
      transpose (unPitchClass (interval p q)) p == q
    
  describe "Num Instance" $ do
    
    prop "addition wraps at 12" $ \(p :: PitchClass) (q :: PitchClass) ->
      let (P result) = p + q
      in result >= 0 && result <= 11
    
    prop "subtraction wraps at 12" $ \(p :: PitchClass) (q :: PitchClass) ->
      let (P result) = p - q
      in result >= 0 && result <= 11
    
    prop "P 0 is additive identity" $ \(p :: PitchClass) ->
      p + P 0 == p && P 0 + p == p
    
  describe "Enum Instance" $ do
    
    it "succ P 11 = P 0 (cyclic)" $
      succ (P 11) `shouldBe` P 0
    
    it "pred P 0 = P 11 (cyclic)" $
      pred (P 0) `shouldBe` P 11
    
    it "toEnum wraps correctly" $
      toEnum 15 `shouldBe` (P 3 :: PitchClass)
    
  describe "NoteName Mappings" $ do
    
    it "pitchClass C = P 0" $
      pitchClass C `shouldBe` P 0
    
    it "pitchClass G = P 7" $
      pitchClass G `shouldBe` P 7
    
    it "sharp and flat are consistent for natural notes" $
      sharp (P 0) `shouldBe` C
    
    it "sharp P 1 = C'" $
      sharp (P 1) `shouldBe` C'
    
    it "flat P 1 = Db" $
      flat (P 1) `shouldBe` Db
    
    prop "pitchClass . sharp = id for PitchClass" $ \(p :: PitchClass) ->
      pitchClass (sharp p) == p
    
    prop "pitchClass . flat = id for PitchClass" $ \(p :: PitchClass) ->
      pitchClass (flat p) == p
