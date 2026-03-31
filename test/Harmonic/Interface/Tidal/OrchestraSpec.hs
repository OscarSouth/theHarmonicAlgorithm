{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Interface.Tidal.OrchestraSpec
-- Description : Tests for Orchestra module (Voice, VoiceLines, clip, articulations)

module Harmonic.Interface.Tidal.OrchestraSpec (spec) where

import Test.Hspec

import qualified Harmonic.Rules.Types.Progression as P
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as Pitch
import Harmonic.Interface.Tidal.Orchestra
import Harmonic.Interface.Tidal.Form (Kinetics(..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Sound.Tidal.Context hiding (clip)

-------------------------------------------------------------------------------
-- Test Fixtures
-------------------------------------------------------------------------------

mkCadenceState :: Pitch.NoteName -> String -> [Integer] -> H.CadenceState
mkCadenceState rootNote func intervals =
  let pcs = map (Pitch.mkPitchClass . fromIntegral) (take 3 intervals)
      cadence = H.Cadence func H.Unison pcs
  in H.CadenceState cadence rootNote H.FlatSpelling

testProgA :: P.Progression
testProgA = P.Progression $ Seq.fromList
  [ mkCadenceState Pitch.C "maj" [0, 4, 7]
  , mkCadenceState Pitch.G "maj" [0, 4, 7]
  ]

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- |Query a Pattern Int at the first cycle and extract values
queryIntPat :: Pattern Int -> [Int]
queryIntPat pat = map value $ queryArc pat (Arc 0 1)

-- |Compare two Pattern Int by querying them
patsEqual :: Pattern Int -> Pattern Int -> Bool
patsEqual a b = queryIntPat a == queryIntPat b

extractNote :: ValueMap -> Maybe Double
extractNote vm = case Map.lookup "note" vm of
  Just (VN n) -> Just (unNote n)
  Just (VF v) -> Just v
  _           -> Nothing

queryNotes :: ControlPattern -> Arc -> [Double]
queryNotes pat arc =
  [ noteVal
  | ev <- queryArc pat arc
  , Just noteVal <- [extractNote (value ev)]
  ]

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Voice and VoiceLines" $ do
    it "vlGet Soprano returns soprano pattern" $
      patsEqual (vlGet Soprano voiceLines) (soprano voiceLines) `shouldBe` True

    it "vlGet covers all voices" $ do
      patsEqual (vlGet Alto voiceLines) (alto voiceLines) `shouldBe` True
      patsEqual (vlGet Tenor voiceLines) (tenor voiceLines) `shouldBe` True
      patsEqual (vlGet Bass voiceLines) (bass voiceLines) `shouldBe` True

    it "octave variants map to base voice" $ do
      patsEqual (vlGet Soprano8va voiceLines) (soprano voiceLines) `shouldBe` True
      patsEqual (vlGet Tenor8vb voiceLines) (tenor voiceLines) `shouldBe` True
      patsEqual (vlGet Bass15vb voiceLines) (bass voiceLines) `shouldBe` True

  describe "voiceOct" $ do
    it "loco voices return 0" $ do
      voiceOct Soprano `shouldBe` 0
      voiceOct Alto `shouldBe` 0
      voiceOct Tenor `shouldBe` 0
      voiceOct Bass `shouldBe` 0

    it "8va returns 1" $
      voiceOct Soprano8va `shouldBe` 1

    it "8vb returns -1" $
      voiceOct Tenor8vb `shouldBe` (-1)

    it "15va returns 2" $
      voiceOct Alto15va `shouldBe` 2

    it "15vb returns -2" $
      voiceOct Bass15vb `shouldBe` (-2)

  describe "clip" $ do
    it "passes notes within range" $ do
      let pat = note 60 :: ControlPattern
          clipped = clip (48, 72) pat
          evs = queryArc clipped (Arc 0 1)
      length evs `shouldSatisfy` (> 0)

    it "filters notes outside range" $ do
      let pat = note 80 :: ControlPattern
          clipped = clip (48, 72) pat
          evs = queryArc clipped (Arc 0 1)
      length evs `shouldBe` 0

    it "passes events without note field" $ do
      let pat = s "thru" :: ControlPattern
          clipped = clip (48, 72) pat
          evs = queryArc clipped (Arc 0 1)
      length evs `shouldSatisfy` (> 0)

  describe "Articulations" $ do
    it "articulation values are distinct channel patterns" $ do
      let extract p = queryArc p (Arc 0 1)
          pEvs  = extract pizz
          sEvs  = extract spicc
          mEvs  = extract marc
          lEvs  = extract legg
          aEvs  = extract arco
      -- All should produce events
      length pEvs `shouldSatisfy` (> 0)
      length sEvs `shouldSatisfy` (> 0)
      length mEvs `shouldSatisfy` (> 0)
      length lEvs `shouldSatisfy` (> 0)
      length aEvs `shouldSatisfy` (> 0)
      -- They should be distinct
      map value pEvs `shouldNotBe` map value sEvs
      map value pEvs `shouldNotBe` map value mEvs
      map value sEvs `shouldNotBe` map value lEvs

  describe "Unpitched percussion" $ do
    it "bassdrum produces events on struct true" $ do
      let pat = bassdrum "t" :: ControlPattern
          evs = queryArc pat (Arc 0 1)
      length evs `shouldSatisfy` (> 0)

    it "tamtam produces events on struct true" $ do
      let pat = tamtam "t" :: ControlPattern
          evs = queryArc pat (Arc 0 1)
      length evs `shouldSatisfy` (> 0)
