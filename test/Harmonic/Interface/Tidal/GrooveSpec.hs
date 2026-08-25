module Harmonic.Interface.Tidal.GrooveSpec (spec) where

import Test.Hspec
import Harmonic.Lib
import Harmonic.Interface.Tidal.Arranger (literal, fromChords)
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import Sound.Tidal.Context
  (Pattern, Time, ArcF(..), Arc, queryArc, wholeOrPart, eventHasOnset, value, silence,
   ValueMap, Value(VF, VN))
import qualified Data.Map.Strict as Map

spec :: Spec
spec = describe "Groove Interface" $ do

  describe "fund voice strategy" $ do
    it "extracts harmonic roots ignoring inversions" $ do
      let prog = fromChords [[0,4,7], [7,0,4], [4,7,0]]
      fund (PC.triadLayer prog) `shouldBe` [[0], [0], [0]]

    it "differs from literal when inversions present" $ do
      let prog = fromChords [[0,4,7], [7,0,4]]
      fund (PC.triadLayer prog) `shouldBe` [[0], [0]]  -- Roots only
      -- literal gives full voicings, which will differ

    it "handles various root notes correctly" $ do
      -- fromChords constructs cadence states, fund extracts true harmonic roots
      let prog = fromChords [[0,4,7], [5,9,0], [7,11,2]]
      -- [0,4,7] = C major → root C (0)
      -- [5,9,0] sorted = [0,5,9] stored as C root, but intervals [0,5,9] = F major → root F (5)
      -- [7,11,2] sorted = [2,7,11] stored as D root, intervals [0,4,9] = F major → root G (7)
      fund (PC.triadLayer prog) `shouldBe` [[0], [5], [7]]  -- True harmonic roots, not bass notes

  describe "fund vs literal comparison" $ do
    it "fund extracts only root regardless of inversion" $ do
      -- C major: root position [0,4,7], first inversion [4,7,0], second inversion [7,0,4]
      let prog = fromChords [[0,4,7], [4,7,0], [7,0,4]]
      fund (PC.triadLayer prog) `shouldBe` [[0], [0], [0]]  -- All roots are C (0)

    it "literal returns full voicings with inversions" $ do
      let prog = fromChords [[0,4,7], [7,0,4]]
      -- literal returns full chord voicings (may be octave-shifted)
      length (head (literal (PC.triadLayer prog))) `shouldSatisfy` (>= 3)  -- Should have at least 3 notes

  describe "Inverted chords: fund returns harmonic roots, not bass notes" $ do
    it "returns harmonic roots, not bass notes, for inverted chords" $ do
      -- Create a progression with explicit inversions
      -- G# minor first inversion: [B, D#, G#] = [11, 3, 8]
      let prog = fromChords [[4,7,11], [11,3,8]]
      -- E major [E,G#,B] = [4,7,11] → root E (4)
      -- G# minor 1st inv [B,D#,G#] = [11,3,8] → root G# (8), NOT bass B (11)
      fund (PC.triadLayer prog) `shouldBe` [[4], [8]]  -- Harmonic roots, not [[4], [11]]

  -- CHARACTERISATION (standing instruction: the sustain 0.01 + CC64
  -- mechanism is intentional and must never change). These pin the
  -- emitted event set so any drift is a build failure, not a discovery
  -- on stage.
  describe "subKick CC64/sustain characterisation" $ do
    let prog  = fromChords [[0,4,7], [5,9,0]]
        kin   = Kinetics (pure 1.0) (pure 1.0) (pure prog) [prog] 0 0
        k     = (kin, pure 1 :: Pattern Int)
        pat   = subKick (pure 1.0) k fund (1/2, "1 ~ ~ ~", "~", "~ ~ 1 ~")
        evs   = map value (queryArc pat (Arc 0 4))
        num v = case v of { VF x -> Just x; VN x -> Just (realToFrac x); _ -> Nothing }
        field f vm = Map.lookup f vm >>= num
        -- midinote N lands in the ValueMap as note (N - 60): kick 48 -> -12
        notes = [ vm | vm <- evs, Map.member "note" vm ]
        ccs   = [ (n, val) | vm <- evs
                           , Just n   <- [field "ctlNum" vm]
                           , Just val <- [field "control" vm] ]

    it "every sub/kick note carries sustain 0.01 (ring is pedal-held, not note-length)" $ do
      notes `shouldSatisfy` (not . null)
      mapM_ (\vm -> field "sustain" vm `shouldBe` Just 0.01) notes

    it "a continuous CC64=127 background holds the pedal down" $
      ccs `shouldSatisfy` any (\(n, v) -> n == 64 && v == 127)

    it "maxDur < 1 emits CC64=0 auto note-offs" $
      ccs `shouldSatisfy` any (\(n, v) -> n == 64 && v == 0)

    it "the kick is fixed high-C MIDI 48 (note -12)" $ do
      let kicks = [ vm | vm <- notes, field "note" vm == Just (-12) ]
      kicks `shouldSatisfy` (not . null)

    it "sub notes sit in the sub register (below the MIDI-48 kick)" $ do
      let subs = [ p | vm <- notes, Just p <- [field "note" vm], p /= (-12) ]
      subs `shouldSatisfy` (not . null)
      subs `shouldSatisfy` all (< (-12))

  describe "noteoff (note-length truncate)" $ do
    let spans p = [ (start w, stop w - start w)
                  | e <- queryArc p (Arc 0 4), eventHasOnset e, value e, let w = wholeOrPart e ]
        g = "[[1 0 0 0] [0 0 0 0] [1 0 0 0] [1 0 0 0]]/4" :: Pattern Bool

    it "caps each hit at a quarter note (n=4)" $
      spans (noteoff 4 g) `shouldBe` [(0,1),(2,1),(3,1)]

    it "extends to the next onset when the cap is not reached (n=1)" $
      spans (noteoff 1 g) `shouldBe` [(0,2),(2,1),(3,1)]

    it "produces sixteenth-note gates (n=16)" $
      spans (noteoff 16 g) `shouldBe` [(0,1/4),(2,1/4),(3,1/4)]

    it "matches the equivalent literal pattern" $
      spans (noteoff 4 g) `shouldBe` spans ("[1 0 1 1]/4" :: Pattern Bool)

    it "yields nothing for an empty gate" $
      spans (noteoff 4 (silence :: Pattern Bool)) `shouldBe` []

    it "never exceeds the cap nor emits a zero-length note" $
      all (\nn -> all (\(_, d) -> d > 0 && d <= 4 / nn) (spans (noteoff nn g)))
          [1, 2, 4, 8, 16 :: Time] `shouldBe` True
