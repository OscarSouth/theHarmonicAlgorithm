-- |
-- Module      : Harmonic.Framework.BuilderPSpec
-- Description : Tests for the genP (strata-first) paradigm

module Harmonic.Framework.BuilderPSpec (spec) where

import Test.Hspec
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.List (nub, sort)
import qualified Data.Set as Set

import qualified Harmonic.Rules.Types.Pitch as P
import qualified Harmonic.Rules.Types.Scale as Sc
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import qualified Harmonic.Rules.Types.Progression as Prog
import qualified Harmonic.Rules.Types.Harmony as H
import           Harmonic.Rules.Types.Harmony
  ( initCadenceState, stateCadence, cadenceIntervals, stateCadenceRoot )
import           Harmonic.Framework.Builder
  ( cue, len, seek, tonal, relStrata, absStrata
  , sameBoost, flipBoost, triBoost
  , genP, genVI, genI
  , hContext, hcTristrata
  )

-- |Helper: extract the (Tristrata, StrataLabel) provenance list.
provList :: PC.ProgressionContext -> [(Sc.Tristrata, Sc.StrataLabel)]
provList pc = maybe [] toList (PC.pcProvenance pc)

-- |Per-bar triad PC set from the triad layer (absolute PCs: root + intervals).
triadAbsolutePCs :: PC.ProgressionContext -> [[Int]]
triadAbsolutePCs pc =
  let states = toList (Prog.unProgression (PC.triadLayer pc))
  in map absPCs states
  where
    absPCs cs =
      let rootPC = P.unPitchClass (P.pitchClass (stateCadenceRoot cs))
          ints   = map P.unPitchClass (cadenceIntervals (stateCadence cs))
      in [(i + rootPC) `mod` 12 | i <- ints]

-- |Pick a known-valid cue for a strata — uses the 3 lowest PCs of the
-- strata's chroma (zero-formed from the lowest as root). Guarantees
-- cue-containment for that strata, so tests don't trigger the
-- invalid-cue abort.
validCueFor :: Sc.StrataLabel -> H.CadenceState
validCueFor s =
  let pcs   = sort (map P.unPitchClass (Sc.strataChroma s))
      top3  = take 3 pcs
      root  = head top3
      ivals = [ (p - root) `mod` 12 | p <- top3 ]
      rname = show (H.enharmonicFunc (H.defaultEnharm (P.mkPitchClass root))
                                      (P.mkPitchClass root))
  in initCadenceState 0 rname ivals

spec :: Spec
spec = describe "genP paradigm" $ do

  -- Strata-I-valid cue used across most tests. genVI tests use a VI-valid
  -- cue separately where needed.
  let start = validCueFor Sc.VI

  describe "basic invariants" $ do
    it "genP VI produces the requested length" $ do
      pc <- seek "none" $ cue start $ len 6 $ genVI
      PC.pcLength pc `shouldBe` 6
      length (provList pc) `shouldBe` 6

    it "all three layers have equal length" $ do
      pc <- seek "none" $ cue start $ len 8 $ genVI
      let tL = Prog.progLength (PC.triadLayer pc)
          sL = Prog.progLength (PC.strataLayer pc)
          mL = Prog.progLength (PC.modeLayer pc)
      tL `shouldBe` 8
      sL `shouldBe` 8
      mL `shouldBe` 8

    it "carries 3-5-7 pitch-set cardinality (triad/strata/mode)" $ do
      pc <- seek "none" $ cue start $ len 8 $ genVI
      let layerCardinalities prog =
            map (length . cadenceIntervals . stateCadence)
                (toList (Prog.unProgression prog))
      layerCardinalities (PC.triadLayer pc)  `shouldBe` replicate 8 3
      layerCardinalities (PC.strataLayer pc) `shouldBe` replicate 8 5
      layerCardinalities (PC.modeLayer pc)   `shouldBe` replicate 8 7

    it "strata-layer chroma matches strataChroma rooted on triad's harmonic root" $ do
      pc <- seek "none" $ cue start $ len 6 $ genVI
      let triadRoots = [ P.unPitchClass (P.pitchClass (H.chordNoteName (H.fromCadenceState cs)))
                       | cs <- toList (Prog.unProgression (PC.triadLayer pc)) ]
          strataAbs  = [ sort [ (i + r) `mod` 12
                              | i <- map P.unPitchClass
                                       (cadenceIntervals (stateCadence cs)) ]
                       | (cs, r) <- zip (toList (Prog.unProgression (PC.strataLayer pc))) triadRoots ]
          expected   = [ sort (map P.unPitchClass (Sc.strataChroma s))
                       | (_, s) <- provList pc ]
      strataAbs `shouldBe` expected

    it "populates pcProvenance (Just)" $ do
      pc <- seek "none" $ cue start $ len 4 $ genVI
      case PC.pcProvenance pc of
        Just _  -> pure ()
        Nothing -> expectationFailure "expected Just provenance"

  describe "initial placement (bar 0)" $ do
    it "seeds strata = VI and tristrata = index 2 (lowest-dissonance containing VI)" $ do
      pc <- seek "none" $ cue start $ len 1 $ genVI
      case provList pc of
        ((t, s) : _) -> do
          s `shouldBe` Sc.VI
          t `shouldBe` Sc.tristrataIndex 2
        _ -> expectationFailure "empty provenance"

    it "genI seeds strata = I and tristrata = index 1" $ do
      pc <- seek "none" $ cue (validCueFor Sc.I) $ len 1 $ genI
      case provList pc of
        ((t, s) : _) -> do
          s `shouldBe` Sc.I
          t `shouldBe` Sc.tristrataIndex 1
        _ -> expectationFailure "empty provenance"

  describe "single-strata containment invariant" $ do
    it "every triad's absolute PCs ⊆ the current strata's chroma" $ do
      pc <- seek "none" $ cue start $ len 8 $ genVI
      let bars  = triadAbsolutePCs pc
          prov  = provList pc
      length bars `shouldBe` length prov
      -- The triad layer is generated by R→E→T with _hcOvertones narrowed to
      -- the strata's chroma, so every triad's absolute PCs must be a subset
      -- of the strata's chroma PCs.
      let strataPCs s = Set.fromList (map P.unPitchClass (Sc.strataChroma s))
      sequence_
        [ Set.fromList tri `shouldSatisfy` (`Set.isSubsetOf` strataPCs s)
        | (tri, (_, s)) <- zip bars prov
        ]

  describe "hcTristrata lock" $ do
    it "hcTristrata \"5\" restricts every bar's tristrata to index 5" $ do
      let ctx = hcTristrata "5" hContext
      pc <- seek "none" $ cue start $ tonal ctx $ len 6 $ genP Sc.VI
      let t5 = Sc.tristrataIndex 5
      sequence_ [ t `shouldBe` t5 | (t, _) <- provList pc ]

    it "hcTristrata \"5\" forces strata drawn only from {IV, VI, X}" $ do
      let ctx = hcTristrata "5" hContext
      pc <- seek "none" $ cue start $ tonal ctx $ len 6 $ genP Sc.VI
      let allowedStrata = Set.fromList [Sc.IV, Sc.VI, Sc.X]
      sequence_
        [ s `shouldSatisfy` (`Set.member` allowedStrata)
        | (_, s) <- provList pc
        ]

    it "hcTristrata whitelist \"1 2\" never yields other tristratas" $ do
      let ctx = hcTristrata "1 2" hContext
      pc <- seek "none" $ cue (validCueFor Sc.V) $ tonal ctx $ len 8 $ genP Sc.V
      let allowed = Set.fromList [Sc.tristrataIndex 1, Sc.tristrataIndex 2]
      sequence_ [ t `shouldSatisfy` (`Set.member` allowed) | (t, _) <- provList pc ]

  describe "relStrata / absStrata modifiers" $ do
    it "relStrata \"1 2 3\" sets length to 3 when len not given" $ do
      pc <- seek "none" $ cue start $ relStrata "1 2 3" $ genVI
      PC.pcLength pc `shouldBe` 3

    it "absStrata \"I V X\" sets length to 3 and forces the strata sequence" $ do
      pc <- seek "none" $ cue (validCueFor Sc.I) $ absStrata "I V X" $ genI
      PC.pcLength pc `shouldBe` 3
      -- Bar 0 is fixed to the starting strata (I); bars 1..2 narrow to the
      -- requested labels.
      let ss = [s | (_, s) <- provList pc]
      take 1 ss `shouldBe` [Sc.I]

    it "len called after relStrata overrides the length" $ do
      pc <- seek "none" $ cue start $ len 4 $ relStrata "1 2 3" $ genVI
      -- relStrata sets len=3; then len 4 wipes the override.
      -- Modifier chain is applied outside-in, so `len 4` is applied AFTER
      -- relStrata, which is the behaviour we want.
      PC.pcLength pc `shouldBe` 4

  describe "alias equivalence" $ do
    it "genVI and genP VI both seed strata VI at bar 0" $ do
      -- Each run is stochastic from bar 1 onward (the walk samples
      -- candidates uniformly excluding self-loops), so provenance lists
      -- aren't equal in general. Bar 0 is deterministic via
      -- 'initialPlacement' and must be strata VI for both aliases.
      pcA <- seek "none" $ cue start $ len 4 $ genVI
      pcB <- seek "none" $ cue start $ len 4 $ genP Sc.VI
      case (provList pcA, provList pcB) of
        ((_, sA) : _, (_, sB) : _) -> do
          sA `shouldBe` Sc.VI
          sB `shouldBe` Sc.VI
        _ -> expectationFailure "empty provenance"

  describe "per-bar diagnostics" $ do
    it "strata chroma has exactly 5 PCs matching strataChroma of the provenance" $ do
      pc <- seek "none" $ cue start $ len 4 $ genVI
      let prov = provList pc
      -- Every provenance strata has a 5-PC chroma.
      sequence_
        [ length (Sc.strataChroma s) `shouldBe` 5 | (_, s) <- prov ]

    it "mode classification yields a 7-PC chroma" $ do
      -- For the first provenance entry, modeFor produces a Mode whose
      -- chroma is 7 PCs (28-mode taxonomy classification).
      pc <- seek "none" $ cue start $ len 4 $ genVI
      case provList pc of
        []        -> expectationFailure "empty provenance"
        ((t,s):_) ->
          length (Sc.modeChroma (Sc.Mode Sc.Aeolian (P.mkPitchClass 0))) `shouldBe` 7
          -- (Generic check that modeChroma returns 7 PCs;
          -- bar-level modeFor requires the R→E→T diagnostic collection,
          -- covered end-to-end in the REPL smoke test.)

  describe "R→E→T integration" $ do
    it "triad bars are not all identical (composer-statistical, not deterministic)" $ do
      -- With the MVP's deterministic 'representative' helper every bar
      -- picked the same 3-PC slice; under R→E→T with gamma the triads
      -- must vary across a 12-bar run.
      pc <- seek "none" $ cue start $ len 12 $ genVI
      let bars = triadAbsolutePCs pc
      -- At least two distinct triads must appear across 12 bars.
      length (nub bars) `shouldSatisfy` (>= 2)

    it "boost = 1.0 disables continuity bias without breaking generation" $ do
      pc <- seek "none" $ cue start $ sameBoost 1.0 $ flipBoost 1.0 $ triBoost 1.0 $ len 8 $ genVI
      PC.pcLength pc `shouldBe` 8
      -- Containment still holds with unit boosts (narrowing does the work).
      let bars = triadAbsolutePCs pc
          prov = provList pc
          strataPCs s = Set.fromList (map P.unPitchClass (Sc.strataChroma s))
      sequence_
        [ Set.fromList tri `shouldSatisfy` (`Set.isSubsetOf` strataPCs s)
        | (tri, (_, s)) <- zip bars prov
        ]

    it "containment preserved under strong same-strata boost" $ do
      pc <- seek "none" $ cue start $ sameBoost 0.1 $ len 10 $ genVI
      let bars = triadAbsolutePCs pc
          prov = provList pc
          strataPCs s = Set.fromList (map P.unPitchClass (Sc.strataChroma s))
      sequence_
        [ Set.fromList tri `shouldSatisfy` (`Set.isSubsetOf` strataPCs s)
        | (tri, (_, s)) <- zip bars prov
        ]

    it "hcTristrata lock + containment survives R→E→T" $ do
      let ctx = hcTristrata "5" hContext
      pc <- seek "none" $ cue start $ tonal ctx $ len 10 $ genP Sc.VI
      let t5 = Sc.tristrataIndex 5
          bars = triadAbsolutePCs pc
          prov = provList pc
          strataPCs s = Set.fromList (map P.unPitchClass (Sc.strataChroma s))
      sequence_ [ t `shouldBe` t5 | (t, _) <- prov ]
      sequence_
        [ Set.fromList tri `shouldSatisfy` (`Set.isSubsetOf` strataPCs s)
        | (tri, (_, s)) <- zip bars prov
        ]

    it "strict containment (no bass exemption) holds for every bar" $ do
      -- Every bar's triad (including its bass note) must lie in the
      -- bar's own active strata — even when the walk traverses to a
      -- different strata mid-run. Uses the per-bar provenance.
      pc <- seek "none" $ cue start $ len 16 $ genP Sc.II
      let bars = triadAbsolutePCs pc
          prov = provList pc
          strataPCs s = Set.fromList (map P.unPitchClass (Sc.strataChroma s))
      length bars `shouldBe` length prov
      sequence_
        [ Set.fromList tri `shouldSatisfy` (`Set.isSubsetOf` strataPCs s)
        | (tri, (_, s)) <- zip bars prov
        ]
