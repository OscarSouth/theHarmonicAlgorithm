-- |
-- Module      : Harmonic.Evaluation.Scoring.ProgressionSpec
-- Description : Whole-progression scoring validation
--
-- Tests the composable score record produced by 'scoreProgression':
--   * Per-component normalisations land in [0, 1]
--   * Hand-crafted "perfect" progressions score near 1.0 on relevant axes
--   * Total scores honour the supplied weight blend
--   * Mode-validity reads the modeLayer cardinality correctly for both
--     legacy (3-PC duplicate) and genP (7-PC distinct) contexts

module Harmonic.Evaluation.Scoring.ProgressionSpec (spec) where

import           Test.Hspec
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import qualified Harmonic.Rules.Types.Harmony as H
import Harmonic.Rules.Types.Pitch (mkPitchClass)
import qualified Harmonic.Rules.Types.Progression as Prog
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import           Harmonic.Evaluation.Scoring.Progression

-- |Build a triad-only ProgressionContext from a list of (rootName, intervals).
mkTriadCtx :: [(String, [Int])] -> PC.ProgressionContext
mkTriadCtx cs =
  let cadStates = [ H.initCadenceState 0 r ints | (r, ints) <- cs ]
      prog      = Prog.Progression (Seq.fromList cadStates)
  in PC.fromProgression prog

spec :: Spec
spec = do

  describe "scoreProgression: range invariants" $ do
    it "all components land in [0, 1]" $ do
      let pc = mkTriadCtx [ ("C", [0,4,7]), ("F", [0,4,7])
                          , ("G", [0,4,7]), ("C", [0,4,7]) ]
          s  = scoreProgression pc
      psRootMotion   s `shouldSatisfy` (\x -> x >= 0 && x <= 1)
      psVoiceLeading s `shouldSatisfy` (\x -> x >= 0 && x <= 1)
      psCadenceFav   s `shouldSatisfy` (\x -> x >= 0 && x <= 1)
      psModeValidity s `shouldSatisfy` (\x -> x >= 0 && x <= 1)

    it "psCadenceFav defaults to 0 in offline computation" $ do
      let pc = mkTriadCtx [ ("C", [0,4,7]), ("F", [0,4,7]) ]
      psCadenceFav (scoreProgression pc) `shouldBe` 0.0

  describe "psRootMotion" $ do
    it "cyclic C → G (P5 / P4 wrap) scores 1.0" $ do
      -- C→G is P5 (+7); the cyclic wrap G→C is P4 (-7 mod 12 = 5). Both
      -- have interval class 5 → rootMotionScore 1 each → avg 1 → normalised
      -- (6 - 1) / 5 = 1.0.
      let pc = mkTriadCtx [ ("C", [0,4,7]), ("G", [0,4,7]) ]
      psRootMotion (scoreProgression pc) `shouldBe` 1.0

    it "all tritone motion scores 0.0 (worst)" $ do
      let pc = mkTriadCtx [ ("C", [0,4,7]), ("F#", [0,4,7])
                          , ("C", [0,4,7]), ("F#", [0,4,7]) ]
      psRootMotion (scoreProgression pc) `shouldBe` 0.0

    it "singleton progression scores 1.0 (no movement to penalise)" $ do
      let pc = mkTriadCtx [ ("C", [0,4,7]) ]
      psRootMotion (scoreProgression pc) `shouldBe` 1.0

  describe "psVoiceLeading" $ do
    it "non-negative for any progression" $ do
      let pc = mkTriadCtx [ ("C", [0,4,7]), ("F", [0,4,7]) ]
      psVoiceLeading (scoreProgression pc) `shouldSatisfy` (>= 0)

    it "measures honest sorted voicings (C->A no longer reads as 9 semitones)" $ do
      -- Under the old wrapped/unsorted extraction an A-rooted [0,4,7] bar
      -- read as [9,1,4]; C->A cyclic pairs then measured ~15/edge. Honest
      -- sorted PCs give [0,4,7]->[1,4,9]: base 3, stepwise -1 => 2/edge
      -- => score clamps to 1.0 under the 3.0/8.0 anchors.
      let pc = mkTriadCtx [ ("C", [0,4,7]), ("A", [0,4,7]) ]
      psVoiceLeading (scoreProgression pc) `shouldBe` 1.0

    it "single-bar progression scores explicit neutral 0.5 (was accidental 1.0)" $ do
      let pc = mkTriadCtx [ ("C", [0,4,7]) ]
      psVoiceLeading (scoreProgression pc) `shouldBe` 0.5

    it "smooth beats jagged" $ do
      -- tritone-apart major triads: sorted absolute PCs [0,4,7]->[1,6,10],
      -- 6+/edge — inside the 3.0/8.0 anchor window, scores below smooth
      let smooth = mkTriadCtx [ ("C", [0,4,7]), ("A", [0,4,7]) ]
          jagged = mkTriadCtx [ ("C", [0,4,7]), ("F#", [0,4,7]) ]
      psVoiceLeading (scoreProgression smooth)
        `shouldSatisfy` (> psVoiceLeading (scoreProgression jagged))

  describe "psModeValidity" $ do
    it "legacy (Nothing provenance) scores 1.0 unconditionally" $ do
      let pc = mkTriadCtx [ ("C", [0,4,7]), ("F", [0,4,7]) ]
      PC.pcProvenance pc `shouldBe` Nothing
      psModeValidity (scoreProgression pc) `shouldBe` 1.0

  describe "totalScore: weight blending" $ do
    it "equal weights produce arithmetic mean of components" $ do
      let s = ProgressionScore 1.0 0.5 0.0 0.5
          w = ProgressionScoreWeights 0.25 0.25 0.25 0.25
      totalScore w s `shouldBe` 0.5

    it "defaultWeights gives cadence-favourability (0.4) the dominant share" $ do
      wCadenceFav defaultWeights `shouldBe` 0.4
      wRootMotion defaultWeights `shouldBe` 0.2

    it "defaultWeightsOffline zeroes the cadence component" $ do
      wCadenceFav defaultWeightsOffline `shouldBe` 0.0

    it "defaultWeightsOffline renormalises the three remaining components" $ do
      let totalWeight =
              wRootMotion   defaultWeightsOffline
            + wVoiceLeading defaultWeightsOffline
            + wCadenceFav   defaultWeightsOffline
            + wModeValidity defaultWeightsOffline
      totalWeight `shouldSatisfy` (\x -> abs (x - 1.0) < 1e-9)

    it "online defaultWeights sum to 1.0" $ do
      let totalWeight =
              wRootMotion   defaultWeights
            + wVoiceLeading defaultWeights
            + wCadenceFav   defaultWeights
            + wModeValidity defaultWeights
      totalWeight `shouldSatisfy` (\x -> abs (x - 1.0) < 1e-9)

  describe "cadenceFavFromMap — per-source-prior aggregation" $ do
    -- A 'Cadence' shows as @(movement -> functionality)@ — root note is
    -- intentionally absent (the DB is pitch-agnostic). Test fixtures vary
    -- movement and/or chord quality so cadences have distinct show keys.
    let mkProg :: [(Int, String, [Int])] -> Prog.Progression
        mkProg cs =
          let states = [ H.initCadenceState m r ints | (m, r, ints) <- cs ]
          in Prog.Progression (Seq.fromList states)

        cadOf :: Int -> String -> [Int] -> H.Cadence
        cadOf m r ints = H.stateCadence (H.initCadenceState m r ints)

        -- Three distinguishable cadences via (movement, quality) variation:
        --   alpha = (Unison, maj)
        --   beta  = (asc 5,  maj)   -- different movement
        --   gamma = (Unison, min)   -- different functionality
        alphaSig = (0, "C", [0,4,7])
        betaSig  = (5, "C", [0,4,7])
        gammaSig = (0, "C", [0,3,7])
        cAlpha = (\(m, r, i) -> cadOf m r i) alphaSig
        cBeta  = (\(m, r, i) -> cadOf m r i) betaSig
        cGamma = (\(m, r, i) -> cadOf m r i) gammaSig

    it "test fixtures have distinct show keys" $ do
      -- Sanity check: if this fails, the other tests in the block can't be
      -- trusted.
      show cAlpha `shouldNotBe` show cBeta
      show cAlpha `shouldNotBe` show cGamma
      show cBeta  `shouldNotBe` show cGamma

    it "empty map → score 0 (no transitions known anywhere)" $ do
      let prog = mkProg [alphaSig, betaSig, gammaSig]
      cadenceFavFromMap Map.empty prog `shouldBe` 0.0

    it "singleton progression → score 0 (no edges)" $ do
      let prog = mkProg [alphaSig]
      cadenceFavFromMap Map.empty prog `shouldBe` 0.0

    it "two-bar progression with sole-edge match → score 1.0 (perfect share at presence+full)" $ do
      -- Hybrid: each edge present + share 1/1 → 0.5 + 0.5 = 1.0. Mean = 1.0.
      let prog = mkProg [alphaSig, betaSig]
          srcMap = Map.fromList
            [ (T.pack (show cAlpha), [(cBeta,  1.0)])
            , (T.pack (show cBeta),  [(cAlpha, 1.0)])
            ]
      cadenceFavFromMap srcMap prog `shouldBe` 1.0

    it "a fused 4-note chain scores identically to its triad walk shadow (gen4)" $ do
      -- Same alpha/beta walk, but each bar carries maj7 (triad + added 11)
      -- as gen4 emits. walkTriadCadence must project the keys back to the
      -- plain maj cadences — otherwise the map misses and the score is 0.
      let fuse4 (m, r, ints) =
            let base = H.initCadenceState m r ints
                cad  = H.stateCadence base
                cad4 = cad { H.cadenceFunctionality = "maj7"
                           , H.cadenceIntervals = map mkPitchClass (ints ++ [11]) }
            in base { H.stateCadence = cad4 }
          fusedProg = Prog.Progression (Seq.fromList (map fuse4 [alphaSig, betaSig]))
          triadProg = mkProg [alphaSig, betaSig]
          srcMap = Map.fromList
            [ (T.pack (show cAlpha), [(cBeta,  1.0)])
            , (T.pack (show cBeta),  [(cAlpha, 1.0)])
            ]
      cadenceFavFromMap srcMap fusedProg
        `shouldBe` cadenceFavFromMap srcMap triadProg
      cadenceFavFromMap srcMap fusedProg `shouldBe` 1.0

    it "edge with no matching destination → contributes 0" $ do
      -- alpha→beta edge, but the map only has alpha→gamma. beta→alpha wraps
      -- back; map empty there.
      let prog = mkProg [alphaSig, betaSig]
          srcMap = Map.fromList
            [ (T.pack (show cAlpha), [(cGamma, 5.0)])    -- alpha→gamma, not alpha→beta
            ]
      -- Edge 1 (α→β): 0/5 = 0. Edge 2 (β→α): β not in map → 0. Mean = 0.
      cadenceFavFromMap srcMap prog `shouldBe` 0.0

    it "partial coverage averages correctly" $ do
      -- α→β (matched, single edge, weight 3 of total 3 → 0.5 + 0.5 = 1.0).
      -- β→α (unknown source → 0).
      -- Mean of [1, 0] = 0.5.
      let prog = mkProg [alphaSig, betaSig]
          srcMap = Map.fromList
            [ (T.pack (show cAlpha), [(cBeta, 3.0)])
            ]
      cadenceFavFromMap srcMap prog `shouldBe` 0.5

    it "multi-destination source gives presence + share-weighted bonus" $ do
      -- α→β: present, share 3/10 = 0.3 → 0.5 + 0.5*0.3 = 0.65.
      -- β→α: present, share 1/1 = 1.0 → 0.5 + 0.5*1.0 = 1.0.
      -- Mean = (0.65 + 1.0) / 2 = 0.825.
      let prog = mkProg [alphaSig, betaSig]
          srcMap = Map.fromList
            [ (T.pack (show cAlpha), [(cBeta, 3.0), (cGamma, 7.0)])
            , (T.pack (show cBeta),  [(cAlpha, 1.0)])
            ]
      let result = cadenceFavFromMap srcMap prog
      result `shouldSatisfy` (\x -> abs (x - 0.825) < 1e-9)

    it "zero total weight under blend → contributes 0 (no division by zero)" $ do
      let prog = mkProg [alphaSig, betaSig]
          srcMap = Map.fromList
            [ (T.pack (show cAlpha), [(cBeta,  0.0)])    -- present but zeroed
            , (T.pack (show cBeta),  [(cAlpha, 0.0)])
            ]
      cadenceFavFromMap srcMap prog `shouldBe` 0.0
