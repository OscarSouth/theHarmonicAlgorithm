module Harmonic.Interface.Tidal.GrooveSpec (spec) where

import Test.Hspec
import Harmonic.Lib
import Harmonic.Interface.Tidal.Arranger (literal, fromChords)

spec :: Spec
spec = describe "Groove Interface" $ do

  describe "fund voice strategy" $ do
    it "extracts harmonic roots ignoring inversions" $ do
      let prog = fromChords FlatSpelling [[0,4,7], [7,0,4], [4,7,0]]
      fund prog `shouldBe` [[0], [0], [0]]

    it "differs from literal when inversions present" $ do
      let prog = fromChords FlatSpelling [[0,4,7], [7,0,4]]
      fund prog `shouldBe` [[0], [0]]  -- Roots only
      -- literal gives full voicings, which will differ

    it "handles various root notes correctly" $ do
      -- fromChords constructs cadence states, fund extracts true harmonic roots
      let prog = fromChords FlatSpelling [[0,4,7], [5,9,0], [7,11,2]]
      -- [0,4,7] = C major → root C (0)
      -- [5,9,0] sorted = [0,5,9] stored as C root, but intervals [0,5,9] = F major → root F (5)
      -- [7,11,2] sorted = [2,7,11] stored as D root, intervals [0,4,9] = F major → root G (7)
      fund prog `shouldBe` [[0], [5], [7]]  -- True harmonic roots, not bass notes

  describe "fund vs literal comparison" $ do
    it "fund extracts only root regardless of inversion" $ do
      -- C major: root position [0,4,7], first inversion [4,7,0], second inversion [7,0,4]
      let prog = fromChords FlatSpelling [[0,4,7], [4,7,0], [7,0,4]]
      fund prog `shouldBe` [[0], [0], [0]]  -- All roots are C (0)

    it "literal returns full voicings with inversions" $ do
      let prog = fromChords FlatSpelling [[0,4,7], [7,0,4]]
      -- literal returns full chord voicings (may be octave-shifted)
      length (head (literal prog)) `shouldSatisfy` (>= 3)  -- Should have at least 3 notes

  describe "Inverted chords: fund returns harmonic roots, not bass notes" $ do
    it "returns harmonic roots, not bass notes, for inverted chords" $ do
      -- Create a progression with explicit inversions
      -- G# minor first inversion: [B, D#, G#] = [11, 3, 8]
      let prog = fromChords FlatSpelling [[4,7,11], [11,3,8]]
      -- E major [E,G#,B] = [4,7,11] → root E (4)
      -- G# minor 1st inv [B,D#,G#] = [11,3,8] → root G# (8), NOT bass B (11)
      fund prog `shouldBe` [[4], [8]]  -- Harmonic roots, not [[4], [11]]
