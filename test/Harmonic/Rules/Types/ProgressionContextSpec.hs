-- |
-- Module      : Harmonic.Rules.Types.ProgressionContextSpec
-- Description : Tests for ProgressionContext splicing and layer semantics

module Harmonic.Rules.Types.ProgressionContextSpec (spec) where

import           Test.Hspec
import qualified Data.Sequence as Seq
import           Data.Foldable (toList)
import           Data.List (nub, sort)

import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Progression as Prog
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import qualified Harmonic.Rules.Types.Scale as Sc
import qualified Harmonic.Rules.Types.Pitch as P

-- |Construct a triad-only ProgressionContext from a list of (rootName, intervals).
mkTriadCtx :: [(String, [Int])] -> PC.ProgressionContext
mkTriadCtx cs =
  let states = [ H.initCadenceState 0 r ints | (r, ints) <- cs ]
      prog   = Prog.Progression (Seq.fromList states)
  in PC.fromProgression prog

-- |Construct a synthetic provenance ProgressionContext for testing the
-- provenance splice path. All three layers duplicate the triad; provenance
-- is set explicitly per bar.
mkProvCtx :: [((String, [Int]), (Sc.Tristrata, Sc.StrataLabel))] -> PC.ProgressionContext
mkProvCtx xs =
  let states  = [ H.initCadenceState 0 r ints | ((r, ints), _) <- xs ]
      prog    = Prog.Progression (Seq.fromList states)
      provSeq = Seq.fromList [ pr | (_, pr) <- xs ]
  in PC.ProgressionContext
       { PC.triadLayer   = prog
       , PC.strataLayer  = prog
       , PC.modeLayer    = prog
       , PC.pcProvenance = Just provSeq
       , PC.pcFamily     = PC.FStrata
       }

-- |Convenience: extract triad roots as a string list.
roots :: PC.ProgressionContext -> [P.NoteName]
roots pc = map H.stateCadenceRoot
               (toList (Prog.unProgression (PC.triadLayer pc)))

spec :: Spec
spec = do
  familySpec
  layerSpec
  algebraSpec

  describe "pcSplice — non-wrapping range" $ do
    it "replaces bars 2..3 of a 4-bar progression" $ do
      let src   = mkTriadCtx [("C", [0,4,7]), ("D", [0,4,7])
                             ,("E", [0,4,7]), ("F", [0,4,7])]
          ins   = mkTriadCtx [("G", [0,4,7]), ("A", [0,4,7])]
          out   = PC.pcSplice src 2 3 ins
      roots out `shouldBe` [P.C, P.G, P.A, P.F]
      PC.pcLength out `shouldBe` 4

    it "preserves invariant: layer lengths equal across triad/strata/mode" $ do
      let src   = mkTriadCtx [("C", [0,4,7]), ("D", [0,4,7]), ("E", [0,4,7])]
          ins   = mkTriadCtx [("G", [0,4,7])]
          out   = PC.pcSplice src 2 2 ins
      Prog.progLength (PC.triadLayer out)  `shouldBe` 3
      Prog.progLength (PC.strataLayer out) `shouldBe` 3
      Prog.progLength (PC.modeLayer out)   `shouldBe` 3

  describe "pcSplice — wrapping range" $ do
    it "replaces bars 4..2 (wrap) of a 5-bar progression" $ do
      let src = mkTriadCtx [("C", [0,4,7]), ("D", [0,4,7]), ("E", [0,4,7])
                           ,("F", [0,4,7]), ("G", [0,4,7])]
          -- Range size = 5 - 4 + 1 + 2 = 4 bars
          ins = mkTriadCtx [("Bb", [0,4,7]), ("Ab", [0,4,7])
                           ,("Gb", [0,4,7]), ("Eb", [0,4,7])]
          out = PC.pcSplice src 4 2 ins
      -- Replaced positions 4,5,1,2 (in temporal-walk order); kept position 3.
      -- ins[0] -> position 4 (start of wrap), ins[1] -> position 5,
      -- ins[2] -> position 1 (wrap), ins[3] -> position 2.
      -- Source-index order is therefore: [ins[2], ins[3], src[2], ins[0], ins[1]]
      --                                 = [Gb,     Eb,     E,      Bb,     Ab]
      PC.pcLength out `shouldBe` 5
      roots out `shouldBe` [P.Gb, P.Eb, P.E, P.Bb, P.Ab]

  describe "pcSplice — provenance handling" $ do
    it "preserves Just-Just provenance with length equal to the spliced triad" $ do
      let t1 = Sc.tristrataIndex 1
          t5 = Sc.tristrataIndex 5
          src = mkProvCtx
            [ (("C", [0,4,7]), (t1, Sc.I))
            , (("D", [0,4,7]), (t1, Sc.V))
            , (("E", [0,4,7]), (t1, Sc.X))
            , (("F", [0,4,7]), (t1, Sc.I))
            ]
          ins = mkProvCtx
            [ (("G", [0,4,7]), (t5, Sc.IV))
            , (("A", [0,4,7]), (t5, Sc.VI))
            ]
          out = PC.pcSplice src 2 3 ins
      case PC.pcProvenance out of
        Just sq -> do
          Seq.length sq `shouldBe` 4
          toList sq `shouldBe` [(t1, Sc.I), (t5, Sc.IV), (t5, Sc.VI), (t1, Sc.I)]
        Nothing -> expectationFailure "expected Just provenance after splicing two Just-provenance contexts"

    it "drops to Nothing when either side lacks provenance" $ do
      let src   = mkTriadCtx [("C", [0,4,7]), ("D", [0,4,7])]   -- Nothing
          ins   = mkTriadCtx [("G", [0,4,7])]                    -- Nothing
          out   = PC.pcSplice src 1 1 ins
      PC.pcProvenance out `shouldBe` Nothing

layerSpec :: Spec
layerSpec = describe "layer — combination selectors" $ do
  let mkCS r ivs = H.mkCadenceStatePCs
        ([P.C, P.Db, P.D, P.Eb, P.E, P.F, P.Gb,
          P.G, P.Ab, P.A, P.Bb, P.B] !! (r `mod` 12)) H.Unison ivs
      absPCs cs =
        let r = P.unPitchClass (P.pitchClass (H.stateCadenceRoot cs))
        in sort [ (r + P.unPitchClass iv) `mod` 12
                | iv <- H.cadenceIntervals (H.stateCadence cs) ]
      barsOf p = toList (Prog.unProgression p)
      -- Common-dyad polytonal bar: T = C maj {0,4,7}, S = A min {9,0,4},
      -- M = E {4,11,0} — all three share the dyad {0,4}.
      polyCtx = PC.ProgressionContext
        { PC.triadLayer   = Prog.fromCadenceStates [mkCS 0 [0,4,7]]
        , PC.strataLayer  = Prog.fromCadenceStates [mkCS 9 [0,3,7]]
        , PC.modeLayer    = Prog.fromCadenceStates [mkCS 4 [0,7,8]]
        , PC.pcProvenance = Nothing
        , PC.pcFamily     = PC.FPoly
        }
      barPCs sel = map absPCs (barsOf (PC.layer sel polyCtx))

  it "pairs union to 4 tones, all three to 5, pivot to the shared dyad" $ do
    barPCs PC.TS  `shouldBe` [[0, 4, 7, 9]]
    barPCs PC.TM  `shouldBe` [[0, 4, 7, 11]]
    barPCs PC.TSM `shouldBe` [[0, 4, 7, 9, 11]]
    barPCs PC.PT  `shouldBe` [[0, 4]]

  it "merged bars root on the lowest constituent layer (T owns the bass; SM roots on S)" $ do
    map H.stateCadenceRoot (barsOf (PC.layer PC.TS polyCtx))  `shouldBe` [P.C]
    map H.stateCadenceRoot (barsOf (PC.layer PC.TSM polyCtx)) `shouldBe` [P.C]
    map H.stateCadenceRoot (barsOf (PC.layer PC.SM polyCtx))  `shouldBe` [P.A]
    -- SM = S ∪ M as a set (both partners share the same dyad here)
    barPCs PC.SM `shouldBe` [[0, 4, 9, 11]]

  it "keeps the zero-form invariant on synthesized bars" $ do
    let zfOK cs = case map P.unPitchClass (H.cadenceIntervals (H.stateCadence cs)) of
          []        -> False
          ivs@(i:_) -> i == 0 && ivs == sort (nub ivs)
    mapM_ (\sel -> all zfOK (barsOf (PC.layer sel polyCtx)) `shouldBe` True)
          [PC.TS, PC.TM, PC.SM, PC.TSM, PC.PT]

  it "degrades to the stored progression when all layers duplicate it (raw hand-built)" $ do
    let genCtx = PC.fromProgression
          (Prog.fromCadenceStates [mkCS 0 [0,4,7], mkCS 7 [0,3,7]])
    mapM_ (\sel -> map absPCs (barsOf (PC.layer sel genCtx))
                     `shouldBe` map absPCs (barsOf (PC.triadLayer genCtx)))
          [PC.TS, PC.TM, PC.SM, PC.TSM, PC.PT]

algebraSpec :: Spec
algebraSpec = describe "context algebra (Monoid identity + combinator provenance)" $ do
  let t1 = Sc.tristrataIndex 1
      t5 = Sc.tristrataIndex 5
      strataCtx = mkProvCtx
        [ (("C", [0,4,7]), (t1, Sc.I))
        , (("D", [0,4,7]), (t1, Sc.V))
        , (("E", [0,4,7]), (t5, Sc.X))
        ]
      provOf pc = toList <$> PC.pcProvenance pc
      mkCS r ivs = H.mkCadenceStatePCs r H.Unison ivs
      polyCtx = PC.ProgressionContext
        { PC.triadLayer   = Prog.fromCadenceStates [mkCS P.C [0,4,7], mkCS P.F [0,4,7]]
        , PC.strataLayer  = Prog.fromCadenceStates [mkCS P.A [0,3,7], mkCS P.D [0,3,7]]
        , PC.modeLayer    = Prog.fromCadenceStates [mkCS P.E [0,7,8], mkCS P.A [0,4,7]]
        , PC.pcProvenance = Nothing
        , PC.pcFamily     = PC.FPoly
        }

  it "mempty is a true identity (family and provenance survive fuse of one or two)" $ do
    (strataCtx <> mempty) `shouldBe` strataCtx
    (mempty <> strataCtx) `shouldBe` strataCtx
    let fused = mconcat [strataCtx]
    PC.pcFamily fused `shouldBe` PC.FStrata
    provOf fused `shouldBe` provOf strataCtx
    let both = mconcat [strataCtx, strataCtx]
    PC.pcFamily both `shouldBe` PC.FStrata
    fmap length (provOf both) `shouldBe` Just 6
    (mconcat [] :: PC.ProgressionContext) `shouldBe` mempty
    PC.pcFamily (mconcat [polyCtx]) `shouldBe` PC.FPoly

  it "liftPC drops provenance and normalizes FStrata to FTriad (deliberate)" $ do
    let out = PC.liftPC id strataCtx
    PC.pcProvenance out `shouldBe` Nothing
    PC.pcFamily out `shouldBe` PC.FTriad

  it "liftPCAligned permutes provenance in lockstep and keeps the family" $ do
    let rev = PC.liftPCAligned Seq.reverse strataCtx
    PC.pcFamily rev `shouldBe` PC.FStrata
    provOf rev `shouldBe` fmap reverse (provOf strataCtx)
    -- FPoly (no provenance): alignment keeps the family
    PC.pcFamily (PC.liftPCAligned Seq.reverse polyCtx) `shouldBe` PC.FPoly

  it "liftPCSubst downgrades FPoly and (via normalize) provenance-less FStrata" $ do
    PC.pcFamily (PC.liftPCSubst id polyCtx) `shouldBe` PC.FTriad
    PC.pcFamily (PC.liftPCSubst id strataCtx) `shouldBe` PC.FTriad

familySpec :: Spec
familySpec = describe "pcFamily" $ do
  let triadProg = Prog.fromCadenceStates
        [ H.initCadenceState m "C" [0,4,7] | m <- [0, 5] ]
      extendedProg = Prog.fromCadenceStates
        [ H.mkCadenceStatePCs P.C H.Unison [0,4,7,10]
        , H.mkCadenceStatePCs P.F H.Unison [0,3,7,10] ]

  it "fromProgression infers FTriad for triads, FExtended for uniform 4-note" $ do
    PC.pcFamily (PC.fromProgression triadProg) `shouldBe` PC.FTriad
    PC.pcFamily (PC.fromProgression extendedProg) `shouldBe` PC.FExtended

  it "explicit stamps survive liftPC and pcSplice" $ do
    let jazzPC = (PC.fromProgression triadProg) { PC.pcFamily = PC.FJazz }
    PC.pcFamily (PC.liftPC id jazzPC) `shouldBe` PC.FJazz
    PC.pcFamily (PC.pcSplice jazzPC 1 1 (PC.fromProgression
      (Prog.fromCadenceStates [H.initCadenceState 0 "D" [0,3,7]])))
      `shouldBe` PC.FJazz

  it "fusing differing families downgrades to FTriad; equal families keep" $ do
    let jazzPC = (PC.fromProgression triadProg) { PC.pcFamily = PC.FJazz }
        triPC  = PC.fromProgression triadProg
    PC.pcFamily (jazzPC <> triPC) `shouldBe` PC.FTriad
    PC.pcFamily (jazzPC <> jazzPC) `shouldBe` PC.FJazz
