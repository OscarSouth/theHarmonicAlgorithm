-- |
-- Module      : Harmonic.Interface.Tidal.ArrangerSpec
-- Description : Tests for lead token parsing and starting state construction

module Harmonic.Interface.Tidal.ArrangerSpec (spec) where

import Test.Hspec
import Harmonic.Interface.Tidal.Arranger (parseLeadTokens, LeadToken(..), lead)
import Harmonic.Rules.Types.Harmony (CadenceState(..), stateCadenceRoot)
import Harmonic.Rules.Types.Pitch (NoteName(..))

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
