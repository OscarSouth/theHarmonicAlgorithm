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

    it "recovers the true root of a 4-tone jazz slash structure" $ do
      -- Bb7 over Ab: stored anchored on Ab with tones {0,2,6,9} above it.
      -- fund reports the harmonic fundamental Bb (10); root reports Ab (8).
      let prog = fromChords [[8,10,2,5]]   -- Ab anchor, Bb D F above
      fund (PC.triadLayer prog) `shouldBe` [[10]]

    it "keeps the anchor for a root-position extended chord" $ do
      let prog = fromChords [[0,4,7,10]]   -- C7 in root position
      fund (PC.triadLayer prog) `shouldBe` [[0]]

  -- CHARACTERISATION: the sub is held by note duration to the next kill
  -- boundary (no CC64 — the MPC sub program does not treat it as a damper).
  -- These pin the emitted event set so any drift is a build failure, not a
  -- discovery on stage.
  describe "subKick note-duration characterisation" $ do
    let prog  = fromChords [[0,4,7]]
        kin   = Kinetics (pure 1.0) (pure 1.0) (pure prog) [prog] 0 0
        k     = (kin, pure 1 :: Pattern Int)
        -- maxDur 1/4 -> autoOff at onset+1 cycle; subOn "1*4" (slow 4) -> onsets
        -- at 0,1,2,3; no manual off; kick every cycle. Each sub note spans [n, n+1).
        pat   = subKick (pure 1.0) k fund (1/4, "1*4", "~", "1*4")
        es    = queryArc pat (Arc 0 4)
        num v = case v of { VF x -> Just x; VN x -> Just (realToFrac x); _ -> Nothing }
        field f vm = Map.lookup f vm >>= num
        -- midinote N lands in the ValueMap as note (N - 60): kick 48 -> -12
        isSub e = case field "note" (value e) of
                    Just nn -> let m = round (nn + 60) :: Int in m >= 36 && m <= 47
                    Nothing -> False
        subEvs  = [ e | e <- es, eventHasOnset e, isSub e ]
        kickEvs = [ e | e <- es, eventHasOnset e, field "note" (value e) == Just (-12) ]

    it "emits no CC64 — notes are held by duration, not the pedal" $
      [ () | e <- es, Just c <- [field "ctlNum" (value e)], round c == (64 :: Int) ]
        `shouldBe` []

    it "holds each sub note to the next kill boundary (onset + maxDur*4)" $
      [ (start (wholeOrPart e), stop (wholeOrPart e)) | e <- subEvs ]
        `shouldBe` [(0, 1), (1, 2), (2, 3), (3, 4)]

    it "sub notes carry legato 1 so SuperDirt emits the note-off at the whole end" $
      [ v | e <- subEvs, Just v <- [field "legato" (value e)] ]
        `shouldSatisfy` (\vs -> length vs == 4 && all (== 1.0) vs)

    it "the kick is fixed high-C MIDI 48 (note -12), one-shot with sustain 0.01" $ do
      kickEvs `shouldSatisfy` (not . null)
      mapM_ (\e -> field "sustain" (value e) `shouldBe` Just 0.01) kickEvs

    it "sub notes sit in the sub register (below the MIDI-48 kick)" $ do
      subEvs `shouldSatisfy` (not . null)
      [ p | e <- subEvs, Just p <- [field "note" (value e)] ] `shouldSatisfy` all (< (-12))

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
