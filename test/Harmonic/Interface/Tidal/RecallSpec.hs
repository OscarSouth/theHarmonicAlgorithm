-- |
-- Module      : Harmonic.Interface.Tidal.RecallSpec
-- Description : Lossless save / recall of ProgressionContexts as chart text

module Harmonic.Interface.Tidal.RecallSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Sequence as Seq
import Data.Either (isLeft)
import Data.List (isInfixOf, nub, sort, dropWhileEnd, elemIndices)

import Harmonic.Interface.Tidal.Recall (recall, recallE, renderChart, renderState, parseState, familyWord)
import Harmonic.Interface.Tidal.Arranger (fromChords)
import Harmonic.Evaluation.Analysis.KeyArea (chordscale)
import Harmonic.Rules.Types.Harmony (CadenceState(..), Cadence(..), Movement(..), EnharmonicSpelling(..), mkCadenceStatePCs, corpusFunctionality)
import Harmonic.Rules.Types.Pitch (NoteName(..), PitchClass(..))
import Harmonic.Rules.Types.Progression (fromCadenceStates)
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import qualified Harmonic.Rules.Types.Scale as Sc

-- Round trip through the exact text 'save' prints.
roundTrip :: PC.ProgressionContext -> Either String PC.ProgressionContext
roundTrip pc = recallE (familyWord (PC.pcFamily pc)) (renderChart pc)

st :: NoteName -> Movement -> [Int] -> CadenceState
st = mkCadenceStatePCs

-- A context whose three layers differ bar by bar (what genP / genE / genJ produce).
layered :: PC.Family -> Maybe [(Sc.Tristrata, Sc.StrataLabel)] -> PC.ProgressionContext
layered fam prov = PC.ProgressionContext
  { PC.triadLayer   = fromCadenceStates [ st B (Asc (P 5)) [0,3,7],    st G (Desc (P 4)) [0,4,7] ]
  , PC.strataLayer  = fromCadenceStates [ st B Unison [0,2,3,7,10],    st G Unison [0,2,4,7,9] ]
  , PC.modeLayer    = fromCadenceStates [ st B Unison [0,2,3,5,7,8,10], st G Unison [0,2,4,5,7,9,11] ]
  , PC.pcProvenance = Seq.fromList <$> prov
  , PC.pcFamily     = fam
  }

genState :: Gen CadenceState
genState = do
  root     <- elements [minBound .. maxBound]
  spelling <- elements [FlatSpelling, SharpSpelling]
  move     <- elements ([Unison, Tritone, Empty] ++ [ c (P n) | c <- [Asc, Desc], n <- [1..5] ])
  upper    <- sublistOf [1..11]
  name     <- elements ("" : "maj7no3" : "sus2/4no5" : "sus4_1stInv"
                        : [ corpusFunctionality (map P [0,a,b]) | a <- [1..10], b <- [a+1..11] ])
  let ivs = map P (sort (nub (0 : upper)))
  pure (CadenceState (Cadence name move ivs) root spelling)

spec :: Spec
spec = do
  describe "renderState / parseState" $ do
    it "round-trips any state (root × spelling × movement × intervals × name)" $
      forAll genState $ \cs -> parseState (renderState cs) == Right cs

    it "writes a plain lead sheet in the common case" $
      renderState (st D (Asc (P 5)) [0,3,7]) `shouldBe` "D F A min (asc 5)"

    it "appends the spelling word only when the notes cannot carry it" $ do
      renderState (CadenceState (Cadence "maj" Unison (map P [0,4,7])) C SharpSpelling)
        `shouldBe` "C E G maj (pedal) sharp"
      renderState (st Eb Unison [0,4,7]) `shouldSatisfy` (not . ("flat" `isInfixOf`))

    it "spells upper notes with the stored spelling" $
      renderState (st Db Unison [0,4,7]) `shouldBe` "Db F Ab maj (pedal)"

    it "renders an empty chord name as '-' and reads it back" $ do
      let cs = CadenceState (Cadence "" Unison (map P [0,4,7])) C FlatSpelling
      renderState cs `shouldBe` "C E G - (pedal)"
      parseState "C E G - (pedal)" `shouldBe` Right cs

    it "accepts lead's integer movement form" $
      fmap (cadenceMovement . stateCadence) (parseState "A C E min (-5)")
        `shouldBe` Right (Desc (P 5))

    it "names a bare hand-written chord through the corpus namer and defaults to pedal" $ do
      let Right cs = parseState "e g c"
      cadenceFunctionality (stateCadence cs) `shouldBe` "maj_1stInv"
      cadenceMovement (stateCadence cs) `shouldBe` Unison
      stateCadenceRoot cs `shouldBe` E

    it "rejects garbage" $ do
      parseState "" `shouldSatisfy` isLeft
      parseState "C E G (sideways)" `shouldSatisfy` isLeft
      parseState "C E G maj min" `shouldSatisfy` isLeft

  describe "recall (whole contexts)" $ do
    it "round-trips a chordscale'd triad context" $
      let pc = chordscale (fromChords [[0,4,7],[5,9,0],[7,11,2],[9,0,4]])
      in roundTrip pc `shouldBe` Right pc

    it "round-trips an extended (4-note) context" $
      let pc = fromChords [[0,4,7,11],[5,9,0,4],[7,11,2,5],[9,0,4,7]]
      in (PC.pcFamily pc, roundTrip pc) `shouldBe` (PC.FExtended, Right pc)

    it "round-trips a strata context with provenance" $
      let pc = layered PC.FStrata (Just [(Sc.Tristrata Sc.I Sc.V Sc.X, Sc.V), (Sc.Tristrata Sc.II Sc.VI Sc.X, Sc.II)])
      in roundTrip pc `shouldBe` Right pc

    it "round-trips jazz and poly stamps with independent layers" $ do
      roundTrip (layered PC.FJazz Nothing) `shouldBe` Right (layered PC.FJazz Nothing)
      roundTrip (layered PC.FPoly Nothing) `shouldBe` Right (layered PC.FPoly Nothing)

    it "round-trips the empty context" $
      roundTrip mempty `shouldBe` Right mempty

    it "prints provenance as a fourth column in report notation" $
      let pc = layered PC.FStrata (Just [(Sc.Tristrata Sc.I Sc.V Sc.X, Sc.V), (Sc.Tristrata Sc.II Sc.VI Sc.X, Sc.II)])
      in map (last . splitBars) (renderChart pc) `shouldBe` ["I-V-X V", "II-VI-X II"]

    it "aligns the layer separators across bars" $
      let ls = renderChart (layered PC.FJazz Nothing)
      in map (elemIndices '|') ls `shouldSatisfy` ((== 1) . length . nub)

    it "errors with the bar and column on malformed text" $ do
      recallE "triad" ["C E G maj (pedal) | C E G maj (pedal)"] `shouldSatisfy` leftContaining "bar 1"
      recallE "triad" ["C E G | C E G | C E G (nope)"] `shouldSatisfy` leftContaining "M "
      recallE "strata" ["C E G | C E G | C E G | I-V-X V", "D F A | D F A | D F A"]
        `shouldSatisfy` leftContaining "provenance"
      recallE "baroque" ["C E G | C E G | C E G"] `shouldSatisfy` leftContaining "unknown family"

    it "downgrades a provenance-less strata claim, like the rest of the type" $
      PC.pcFamily (recall "strata" ["C E G | C E G | C E G"]) `shouldBe` PC.FTriad
  where
    splitBars = map (dropWhileEnd (== ' ') . dropWhile (== ' '))
              . foldr (\c (w : ws) -> if c == '|' then [] : w : ws else (c : w) : ws) [[]]
    leftContaining needle = either (needle `isInfixOf`) (const False)
