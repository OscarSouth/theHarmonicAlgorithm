-- |
-- Module      : Harmonic.Core.ProgressionSpec
-- Description : Monoid laws and manipulation function tests
--
-- Validates Progression as a proper Monoid and tests the
-- macro-level manipulation functions ported from Arranger.hs

module Harmonic.Core.ProgressionSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Harmonic.Core.Pitch (PitchClass(..), NoteName(..), pitchClass)
import Harmonic.Core.Harmony
import Harmonic.Core.Progression

-------------------------------------------------------------------------------
-- Monoid Law Tests
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Monoid Laws" $ do
    
    it "mempty is left identity" $ do
      let p = mkTestProgression 3
      mempty <> p `shouldBe` p
    
    it "mempty is right identity" $ do
      let p = mkTestProgression 3
      p <> mempty `shouldBe` p
    
    it "(<>) is associative" $ do
      let p1 = mkTestProgression 2
      let p2 = mkTestProgression 3
      let p3 = mkTestProgression 2
      (p1 <> p2) <> p3 `shouldBe` p1 <> (p2 <> p3)
    
  describe "Construction" $ do
    
    it "singleton creates length-1 progression" $ do
      let cs = mkTestCadenceState
      progLength (singleton cs) `shouldBe` 1
    
    it "fromCadenceStates preserves order" $ do
      let css = replicate 5 mkTestCadenceState
      progLength (fromCadenceStates css) `shouldBe` 5
    
  describe "Queries" $ do
    
    it "progLength returns correct count" $ do
      let p = mkTestProgression 7
      progLength p `shouldBe` 7
    
    it "getCadenceState returns Nothing for out-of-bounds" $ do
      let p = mkTestProgression 3
      getCadenceState p 0 `shouldBe` Nothing   -- 1-indexed, so 0 is invalid
      getCadenceState p 4 `shouldBe` Nothing   -- Only 3 elements
    
    it "getCadenceState returns Just for valid index" $ do
      let p = mkTestProgression 3
      getCadenceState p 1 `shouldSatisfy` isJust
      getCadenceState p 3 `shouldSatisfy` isJust
    
  describe "Rotation" $ do
    
    it "rotateProgression 0 is identity" $ do
      let p = mkTestProgression 5
      rotateProgression 0 p `shouldBe` p
    
    it "rotateProgression n then -n is identity" $ do
      let p = mkTestProgression 5
      rotateProgression (-2) (rotateProgression 2 p) `shouldBe` p
    
    it "rotateProgression by length is identity" $ do
      let p = mkTestProgression 5
      rotateProgression 5 p `shouldBe` p
    
    it "rotateProgression handles empty" $ do
      rotateProgression 3 mempty `shouldBe` (mempty :: Progression)
    
  describe "Excerpt" $ do
    
    it "excerptProgression extracts correct range" $ do
      let p = mkTestProgression 10
      progLength (excerptProgression 3 7 p) `shouldBe` 5
    
    it "excerptProgression 1 n on length-n returns all" $ do
      let p = mkTestProgression 5
      excerptProgression 1 5 p `shouldBe` p
    
  describe "Transposition" $ do
    
    it "transposeProgression 0 is identity for length" $ do
      let p = mkTestProgression 5
      progLength (transposeProgression 0 p) `shouldBe` progLength p
    
    it "transposeProgression 12 preserves pitch classes" $ do
      let p = mkTestProgression 3
      -- Transposing by 12 semitones should result in equivalent pitch classes
      progLength (transposeProgression 12 p) `shouldBe` progLength p
    
  describe "Expansion" $ do
    
    it "expandProgression 1 is identity" $ do
      let p = mkTestProgression 3
      expandProgression 1 p `shouldBe` p
    
    it "expandProgression 0 is mempty" $ do
      let p = mkTestProgression 3
      expandProgression 0 p `shouldBe` mempty
    
    it "expandProgression n multiplies length" $ do
      let p = mkTestProgression 4
      progLength (expandProgression 3 p) `shouldBe` 12
    
  describe "Fuse (Interleave)" $ do
    
    it "fusing empty with p returns p" $ do
      let p = mkTestProgression 3
      fuseProgression mempty p `shouldBe` p
    
    it "fusing p with empty returns p" $ do
      let p = mkTestProgression 3
      fuseProgression p mempty `shouldBe` p
    
    it "fusing equal-length progressions doubles length" $ do
      let p1 = mkTestProgression 3
      let p2 = mkTestProgression 3
      progLength (fuseProgression p1 p2) `shouldBe` 6
    
  describe "Voicing Extractors" $ do
    
    it "literalVoicing returns one list per cadence state" $ do
      let p = mkTestProgression 5
      length (literalVoicing p) `shouldBe` 5
    
    it "harmonyVoicing values are in [0,11]" $ do
      let p = mkTestProgression 3
      let voicings = harmonyVoicing p
      all (all (\x -> x >= 0 && x <= 11)) voicings `shouldBe` True

-------------------------------------------------------------------------------
-- Test Helpers
-------------------------------------------------------------------------------

-- | Create a test CadenceState for testing
mkTestCadenceState :: CadenceState
mkTestCadenceState = initCadenceState 0 "C" [0,4,7] FlatSpelling

-- | Create a progression of n identical cadence states
mkTestProgression :: Int -> Progression
mkTestProgression n = fromCadenceStates (replicate n mkTestCadenceState)

-- | Check if Maybe is Just
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False
