-- |
-- Module      : Harmonic.Rules.Types.ProgressionContextSpec
-- Description : Tests for ProgressionContext splicing and layer semantics

module Harmonic.Rules.Types.ProgressionContextSpec (spec) where

import           Test.Hspec
import qualified Data.Sequence as Seq
import           Data.Foldable (toList)

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
       }

-- |Convenience: extract triad roots as a string list.
roots :: PC.ProgressionContext -> [P.NoteName]
roots pc = map H.stateCadenceRoot
               (toList (Prog.unProgression (PC.triadLayer pc)))

spec :: Spec
spec = do

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
