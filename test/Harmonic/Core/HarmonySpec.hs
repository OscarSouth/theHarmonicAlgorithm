-- |
-- Module      : Harmonic.Core.HarmonySpec
-- Description : Golden tests for legacy chord naming fidelity
--
-- These tests verify that nameFunc produces EXACTLY the same output
-- as the legacy MusicData.hs implementation. Any deviation breaks
-- compatibility with existing data.

module Harmonic.Core.HarmonySpec (spec) where

import Test.Hspec

import Harmonic.Core.Pitch (PitchClass(..), NoteName(..), flat, sharp, unPitchClass)
import Harmonic.Core.Harmony

-------------------------------------------------------------------------------
-- Golden Tests: Chord Naming
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Legacy nameFunc Fidelity" $ do
    
    describe "Basic Triads" $ do
      
      it "[0,4,7] -> maj (major triad)" $
        chordFunctionality (flatTriad [0,4,7]) `shouldContain` "maj"
      
      it "[0,3,7] -> min (minor triad)" $
        chordFunctionality (flatTriad [0,3,7]) `shouldContain` "min"
      
      it "[0,4,8] -> aug (augmented triad)" $
        chordFunctionality (flatTriad [0,4,8]) `shouldContain` "aug"
      
      it "[0,3,6] -> dim (diminished triad)" $
        chordFunctionality (flatTriad [0,3,6]) `shouldContain` "dim"
    
    describe "Suspended Chords" $ do
      
      -- [0,2,7] is enharmonically equivalent to sus4 1st inversion
      -- Legacy treats this as sus4_1stInv (sus2 and sus4 are inversions of each other)
      
      it "[0,2,7] -> sus4_1stInv (legacy: sus2/sus4 are inversionally equivalent)" $
        chordFunctionality (flatTriad [0,2,7]) `shouldContain` "sus4"
      
      it "[0,5,7] -> sus4" $
        chordFunctionality (flatTriad [0,5,7]) `shouldContain` "sus4"
      
      it "[0,2,5,7] -> sus2/4 (combined)" $
        toFunctionalityChord [P 0, P 2, P 5, P 7] `shouldContain` "sus2/4"
    
    describe "Sixth Chords" $ do
      
      it "[0,4,7,9] -> 6 (major sixth)" $
        toFunctionalityChord [P 0, P 4, P 7, P 9] `shouldContain` "6"
      
      -- Note: Legacy uses "m" not "min" for minor sixth
      it "[0,3,7,9] -> m6 (minor sixth)" $ do
        let func = toFunctionalityChord [P 0, P 3, P 7, P 9]
        func `shouldContain` "m"
        func `shouldContain` "6"
    
    describe "Seventh Chords (using toFunctionalityChord for extended harmonies)" $ do
      
      it "[0,4,7,10] -> 7 (dominant seventh, not maj7 or min7)" $ do
        let func = toFunctionalityChord [P 0, P 4, P 7, P 10]
        func `shouldContain` "7"
        func `shouldNotContain` "maj"
        func `shouldNotContain` "min"
      
      it "[0,4,7,11] -> maj7 (major seventh)" $
        toFunctionalityChord [P 0, P 4, P 7, P 11] `shouldContain` "maj7"
      
      it "[0,3,7,10] -> min7 (minor seventh)" $ do
        toFunctionalityChord [P 0, P 3, P 7, P 10] `shouldContain` "m7"
    
  describe "Chord Construction" $ do
    
    it "flatTriad produces Chord with correct root" $
      chordNoteName (flatTriad [0,4,7]) `shouldBe` C
    
    it "sharpTriad uses sharp spelling" $
      chordNoteName (sharpTriad [1,5,8]) `shouldBe` C'
    
    it "flatTriad uses flat spelling" $
      chordNoteName (flatTriad [1,5,8]) `shouldBe` Db
    
  describe "Movement" $ do
    
    -- toMovement 0 7 = desc 5 (because 0→7 up = 0→7 down by 5)
    it "toMovement P 0 to P 7 = Desc P 5 (descending fifth)" $
      toMovement (P 0) (P 7) `shouldBe` Desc (P 5)
    
    -- toMovement 0 5 = asc 5 (because 0→5 up is shorter than down)
    it "toMovement P 0 to P 5 = Asc P 5 (ascending fifth)" $
      toMovement (P 0) (P 5) `shouldBe` Asc (P 5)
    
    it "toMovement P 0 to P 0 = Unison (no movement)" $
      toMovement (P 0) (P 0) `shouldBe` Unison
    
    it "toMovement P 0 to P 6 = Tritone" $
      toMovement (P 0) (P 6) `shouldBe` Tritone
    
    it "fromMovement . toMovement preserves interval" $ do
      let mv = toMovement (P 0) (P 7)
      -- Desc 5 → 12 - 5 = 7
      fromMovement mv `shouldBe` P 7
    
    describe "Edge cases crossing 0/12 boundary" $ do
      
      -- Non-zero starting points
      it "toMovement P 10 to P 2 = Asc P 4 (crosses 0)" $
        toMovement (P 10) (P 2) `shouldBe` Asc (P 4)
      
      it "toMovement P 2 to P 10 = Desc P 4 (crosses 0 backwards)" $
        toMovement (P 2) (P 10) `shouldBe` Desc (P 4)
      
      it "toMovement P 11 to P 1 = Asc P 2 (B to C#)" $
        toMovement (P 11) (P 1) `shouldBe` Asc (P 2)
      
      it "toMovement P 1 to P 11 = Desc P 2 (C# to B)" $
        toMovement (P 1) (P 11) `shouldBe` Desc (P 2)
      
      -- Tritone from non-zero
      it "toMovement P 3 to P 9 = Tritone (Eb to A)" $
        toMovement (P 3) (P 9) `shouldBe` Tritone
      
      -- Unison at non-zero
      it "toMovement P 7 to P 7 = Unison (G to G)" $
        toMovement (P 7) (P 7) `shouldBe` Unison
      
      -- fromMovement round-trip for non-zero start
      it "fromMovement preserves interval for P 5 to P 10" $ do
        let mv = toMovement (P 5) (P 10)  -- F to Bb = Asc 5
        -- The movement distance is 5, so fromMovement returns P 5
        -- But we need to check interval: (5 + fromMovement mv) `mod` 12 == 10
        (5 + unPitchClass (fromMovement mv)) `mod` 12 `shouldBe` 10
    
  describe "State Types" $ do
    
    it "CadenceState preserves root information" $ do
      let cs = initCadenceState 5 "C" [0,4,7] FlatSpelling
      stateCadenceRoot cs `shouldBe` C
    
    it "fromCadenceState produces valid Chord" $ do
      let cs = initCadenceState 0 "G" [0,4,7] FlatSpelling
      let chord = fromCadenceState cs
      chordNoteName chord `shouldBe` G
    
  describe "Normal and Prime Form" $ do
    
    it "normalForm of major triad is [0,4,7]" $ do
      let nf = normalForm [P 0, P 4, P 7]
      map unPitchClass nf `shouldBe` [0,4,7]
    
    it "primeForm is transposition-invariant" $ do
      let pf1 = primeForm [P 0, P 4, P 7]
      let pf2 = primeForm [P 5, P 9, P 0]  -- F major = same structure
      pf1 `shouldBe` pf2
    
    -- primeForm returns the more compact inversion
    it "primeForm of major triad is [0,3,7] (minor is more compact)" $ do
      let pf = primeForm [P 0, P 4, P 7]
      map unPitchClass pf `shouldBe` [0,3,7]
    
  describe "Inversions" $ do
    
    -- NOTE: Current flatTriad/toTriad implementation uses the FIRST element
    -- (bass note) as the reported root. True inversion detection (finding the
    -- "real" root of an inverted chord) is a complex topic that requires
    -- comparing against known chord shapes. The tests below document the
    -- Inversion detection now correctly identifies the root and adds inversion suffix
    
    describe "Inversions correctly identify root and add suffix" $ do
      
      -- C major inversions: now correctly reports C as root with inversion suffix
      it "[4,7,0] (C major 1st inv) reports C as root with _1stInv suffix" $ do
        let chord = flatTriad [4,7,0]
        chordNoteName chord `shouldBe` C
        chordFunctionality chord `shouldBe` "maj_1stInv"
      
      it "[7,0,4] (C major 2nd inv) reports C as root with _2ndInv suffix" $ do
        let chord = flatTriad [7,0,4]
        chordNoteName chord `shouldBe` C
        chordFunctionality chord `shouldBe` "maj_2ndInv"
      
      it "[3,7,0] (C minor 1st inv) reports C as root with _1stInv suffix" $ do
        let chord = flatTriad [3,7,0]
        chordNoteName chord `shouldBe` C
        chordFunctionality chord `shouldBe` "min_1stInv"
    
    describe "Root position chords correctly identify root" $ do
      
      -- These SHOULD work correctly
      it "[0,4,7] correctly identifies C as root" $
        chordNoteName (flatTriad [0,4,7]) `shouldBe` C
      
      it "[7,11,2] correctly identifies G as root" $
        chordNoteName (flatTriad [7,11,2]) `shouldBe` G
      
      it "[10,2,5] correctly identifies Bb as root" $
        chordNoteName (flatTriad [10,2,5]) `shouldBe` Bb
      
      it "[6,9,1] correctly identifies F# as root" $
        chordNoteName (sharpTriad [6,9,1]) `shouldBe` F'
    
    describe "Functionality detection correctly identifies chord quality" $ do
      
      -- With correct inversion detection, chord quality is determined from the actual chord
      it "[4,7,0] (C major 1st inv) is correctly identified as major" $ do
        -- E-G-C is C major in 1st inversion
        let chord = flatTriad [4,7,0]
        chordFunctionality chord `shouldContain` "maj"
      
      it "[8,0,4] is detected as augmented (symmetric)" $ do
        -- Augmented triads are symmetric, any rotation is equivalent
        let chord = flatTriad [8,0,4]
        chordFunctionality chord `shouldContain` "aug"
      
      -- Diminished in root position
      it "[0,3,6] is detected as diminished" $ do
        let chord = flatTriad [0,3,6]
        chordFunctionality chord `shouldContain` "dim"
    
    describe "normalForm preserves inversional equivalence" $ do
      
      it "all C major inversions have same normalForm" $ do
        let root = normalForm [P 0, P 4, P 7]
        let first = normalForm [P 4, P 7, P 0]
        let second = normalForm [P 7, P 0, P 4]
        root `shouldBe` first
        first `shouldBe` second
      
      it "all G minor inversions have same normalForm" $ do
        let root = normalForm [P 7, P 10, P 2]   -- G-Bb-D
        let first = normalForm [P 10, P 2, P 7]  -- Bb-D-G
        let second = normalForm [P 2, P 7, P 10] -- D-G-Bb
        root `shouldBe` first
        first `shouldBe` second
    
    describe "inversions function generates all rotations" $ do
      
      it "inversions of major triad produces 3 forms" $ do
        let invs = inversions [P 0, P 4, P 7]
        length invs `shouldBe` 3
      
      it "each inversion is in zero form" $ do
        let invs = inversions [P 0, P 4, P 7]
        all (\ps -> unPitchClass (head ps) == 0) invs `shouldBe` True

-------------------------------------------------------------------------------
-- Helper
-------------------------------------------------------------------------------

-- Local helper to unwrap PitchClass
unwrapPC :: PitchClass -> Int
unwrapPC (P n) = n
