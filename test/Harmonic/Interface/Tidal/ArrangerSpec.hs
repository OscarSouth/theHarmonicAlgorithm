-- |
-- Module      : Harmonic.Interface.Tidal.ArrangerSpec
-- Description : Tests for lead token parsing and starting state construction

module Harmonic.Interface.Tidal.ArrangerSpec (spec) where

import Test.Hspec
import Harmonic.Interface.Tidal.Arranger (parseLeadTokens, LeadToken(..), lead, lead', strataModeFlow, flow, grid)
import Harmonic.Rules.Types.Harmony (CadenceState(..), Cadence(..), Movement(..), stateCadenceRoot, stateCadence, stateSpelling, cadenceIntervals, cadenceFunctionality, cadenceMovement, EnharmonicSpelling(..), mkCadenceStatePCs)
import Harmonic.Rules.Types.Progression (fromCadenceStates, unProgression)
import Harmonic.Interface.Tidal.Arranger (fromChords)
import Harmonic.Rules.Types.ProgressionContext (triadLayer)
import qualified Data.Foldable
import Harmonic.Rules.Types.Pitch (NoteName(..), PitchClass(..))

spec :: Spec
spec = do
  describe "parseLeadTokens" $ do
    it "parses root from 'E min (5)'" $
      [r | RootTok r <- parseLeadTokens "E min (5)"] `shouldBe` ["E"]

    it "parses quality from 'E min (5)'" $
      [q | QualTok q <- parseLeadTokens "E min (5)"] `shouldBe` ["min"]

    it "parses movement from 'E min (5)'" $
      [m | MoveTok m <- parseLeadTokens "E min (5)"] `shouldBe` [5]

    it "parses negative movement from 'E min (-3)'" $
      [m | MoveTok m <- parseLeadTokens "E min (-3)"] `shouldBe` [-3]

    it "parses movement-only '(5)'" $
      [m | MoveTok m <- parseLeadTokens "(5)"] `shouldBe` [5]

    it "parses root-only 'C#'" $
      [r | RootTok r <- parseLeadTokens "C#"] `shouldBe` ["C#"]

    it "parses flat roots canonically" $
      [r | RootTok r <- parseLeadTokens "Bb"] `shouldBe` ["Bb"]

    it "parses root case-insensitively" $
      [r | RootTok r <- parseLeadTokens "eb"] `shouldBe` ["Eb"]

    it "unrecognised token becomes QualTok" $
      [q | QualTok q <- parseLeadTokens "E xyz (5)"] `shouldBe` ["xyz"]

    it "empty string gives no tokens" $
      parseLeadTokens "" `shouldBe` []

    it "quality-only 'min' gives no root or movement" $ do
      [r | RootTok r <- parseLeadTokens "min"] `shouldBe` []
      [m | MoveTok m <- parseLeadTokens "min"] `shouldBe` []
      [q | QualTok q <- parseLeadTokens "min"] `shouldBe` ["min"]

  describe "lead" $ do
    it "lead 'E min (5)' produces E root" $ do
      cs <- lead "E min (5)"
      stateCadenceRoot cs `shouldBe` E

    it "lead 'C maj' produces C root" $ do
      cs <- lead "C maj"
      stateCadenceRoot cs `shouldBe` C

    it "lead empty string completes without error" $ do
      _ <- lead ""
      pure ()

  describe "lead' (note-name list cue)" $ do
    it "builds Eb m7 from 'Eb Gb Bb Db' with flat spelling" $ do
      cs <- lead' "Eb Gb Bb Db"
      stateCadenceRoot cs `shouldBe` Eb
      cadenceIntervals (stateCadence cs) `shouldBe` [P 0, P 3, P 7, P 10]
      cadenceFunctionality (stateCadence cs) `shouldBe` "m7"
      stateSpelling cs `shouldBe` FlatSpelling

    it "honors the movement token" $ do
      cs <- lead' "A C E G (5)"
      stateCadenceRoot cs `shouldBe` A
      cadenceIntervals (stateCadence cs) `shouldBe` [P 0, P 3, P 7, P 10]
      cadenceMovement (stateCadence cs) `shouldBe` Asc (P 5)

    it "names a triad input with the triad namer (lead parity)" $ do
      cs <- lead' "C E G"
      stateCadenceRoot cs `shouldBe` C
      cadenceIntervals (stateCadence cs) `shouldBe` [P 0, P 4, P 7]
      cadenceFunctionality (stateCadence cs) `shouldBe` "maj"

    it "skips unrecognized tokens gracefully" $ do
      cs <- lead' "Eb xyz Gb Bb Db"
      cadenceIntervals (stateCadence cs) `shouldBe` [P 0, P 3, P 7, P 10]

    it "sharp accidental implies sharp spelling" $ do
      cs <- lead' "F# A# C# E"
      stateCadenceRoot cs `shouldBe` F'
      stateSpelling cs `shouldBe` SharpSpelling

    it "falls back to random lead when no valid notes" $ do
      _ <- lead' "xyz qqq"
      pure ()

  describe "strataModeFlow pedal behavior + big-chroma routing" $ do
    let csOf root ivs = mkCadenceStatePCs root Unison ivs
        pOf = fromCadenceStates

    it "static root: shared tones hold exact MIDI across a mode change" $ do
      let voiced = strataModeFlow (pOf [csOf E [0,2,3,5,7,9,10], csOf E [0,2,4,5,7,9,10]])
      voiced `shouldBe` [[-8,-6,-5,-3,-1,1,2],[-8,-6,-4,-3,-1,1,2]]

    it "static root: 5-PC strata pair pedals shared tones" $ do
      let voiced = strataModeFlow (pOf [csOf E [0,2,3,7,10], csOf E [0,2,3,7,9]])
      take 4 (head voiced) `shouldBe` take 4 (voiced !! 1)

    it "root motion picks the max-common-tone octave" $ do
      let voiced = strataModeFlow (pOf [csOf E [0,2,3,5,7,9,10], csOf A [0,2,4,5,7,9,10]])
          shared = length (filter (`elem` head voiced) (voiced !! 1))
      shared `shouldSatisfy` (>= 4)

    it "flow routes >=6-PC bars to the chroma engine" $ do
      let big = pOf [csOf E [0,2,3,5,7,9,10], csOf G [0,2,4,5,7,9,10]]
      flow big `shouldBe` strataModeFlow big
      grid big `shouldBe` strataModeFlow big

    it "flow keeps the DP for harmony-sized bars (mixed 3/4 seam)" $ do
      let seam = pOf [csOf C [0,4,7], csOf C [0,4,7,10], csOf F [0,4,7]]
      (flow seam !! 1) `shouldBe` [-12,-8,-5,-2]

  describe "enharmonic spelling continuity (2026-08-20 curation)" $ do
    it "fromChords holds the spelling side while the root stands still" $ do
      -- the sixteen_bars case: Gb 6b5 -> maj7b5 -> maj7 (root PC 6 held)
      let pc = fromChords [[1,5,6,8],[6,10,0,3],[6,10,0,5],[6,10,1,5]]
          sps = [ stateSpelling cs
                | cs <- Data.Foldable.toList (unProgression (triadLayer pc)) ]
      drop 1 sps `shouldBe` [FlatSpelling, FlatSpelling, FlatSpelling]
