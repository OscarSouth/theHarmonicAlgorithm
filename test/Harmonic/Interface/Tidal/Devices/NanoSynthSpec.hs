-- |
-- Module      : Harmonic.Interface.Tidal.Devices.NanoSynthSpec
-- Description : Pins the NanoSynth rig: channel map, ranges, shared pizzicato, silent articulations, GM kit
module Harmonic.Interface.Tidal.Devices.NanoSynthSpec (spec) where

import Test.Hspec
import Harmonic.Interface.Tidal.Orchestra
import Harmonic.Interface.Tidal.Devices.NanoSynth
import Harmonic.Interface.Tidal.Instruments (ch)
import qualified Data.Map.Strict as Map
import Sound.Tidal.Context hiding (clip)

spec :: Spec
spec = describe "NanoSynth rig (one section per channel, pizzicato on 7)" $ do
  let num v = case v of { VF x -> Just x; VN x -> Just (realToFrac (unNote x)); VI x -> Just (fromIntegral x); _ -> Nothing }
      field f e = Map.lookup f (value e) >>= num
      onsets p = [ (field "note" e, field "midichan" e) | e <- queryArc p (Arc 0 1), eventHasOnset e ]

  it "winds, brass, harp and timpani sit on the 2021 channels" $
    map ($ nanoSynth) [rigFlute, rigOboe, rigClarinet, rigBassoon, rigHorn, rigTrombone, rigBasstrom, rigHarp, rigTimpani]
      `shouldBe` [1, 2, 11, 3, 4, 5, 5, 9, 6]

  it "each string section has its own channel; viola shares the bass channel" $
    map ($ nanoSynth) [rigViolin1, rigViolin2, rigViola, rigCello, rigContrabass]
      `shouldBe` [13, 14, 16, 15, 16]

  it "pizz re-routes to channel 7; the other articulations leave the line alone" $ do
    let line = note 1 # ch 13 :: ControlPattern
    [ field "midichan" e | e <- queryArc (line # pizz nanoSynth) (Arc 0 1) ] `shouldBe` [Just 6]
    [ map value (queryArc (line # art nanoSynth) (Arc 0 1)) | art <- [spicc, marc, legg, arco] ]
      `shouldBe` replicate 4 (map value (queryArc line (Arc 0 1)))

  it "the bass drum is the orchestra hit four octaves down on channel 8; the tam-tam is silent" $ do
    onsets (bassdrum nanoSynth "t") `shouldBe` [(Just (-48), Just 7)]
    onsets (tamtam nanoSynth "t") `shouldBe` []

  it "the continuo is the harp channel" $ do
    rigContinuo nanoSynth `shouldBe` 9
    nanoHarpV `shouldBe` (3, 0, 46)
    nanoHarpsichordV `shouldBe` (1, 0, 6)

  describe "cold configuration" $ do
    let ccs p = [ (start (part e), field "ctlNum" e, field "control" e, field "midichan" e) | e <- queryArc p (Arc 0 1), eventHasOnset e, Map.member "ctlNum" (value e) ]
        pcs p = [ (start (part e), field "progNum" e, field "midichan" e) | e <- queryArc p (Arc 0 1), eventHasOnset e, Map.member "progNum" (value e) ]

    it "covers all sixteen channels with the 2021 programs" $ do
      map fst nanoPrograms `shouldBe` [1 .. 16]
      lookup 1 nanoPrograms `shouldBe` Just (74, 3)
      lookup 10 nanoPrograms `shouldBe` Just (127, 1)
      lookup 12 nanoPrograms `shouldBe` Just (127, 1)
      lookup 16 nanoPrograms `shouldBe` Just (41, 1)

    it "sends the bank on CC0 before the program change, on the channel" $ do
      ccs (nanoProgram 6 (60, 4)) `shouldBe` [(0, Just 0, Just 4, Just 5)]
      pcs (nanoProgram 6 (60, 4)) `shouldBe` [(1/4, Just 60, Just 5)]

    it "nanoKit puts 15ips Kit (118 / Preset 3) on channel 10" $ do
      ccs nanoKit `shouldBe` [(0, Just 0, Just 3, Just 9)]
      pcs nanoKit `shouldBe` [(1/4, Just 118, Just 9)]

    it "the setup is sixteen programs then level, pan, depth, modulation, expression" $ do
      length nanoSetup `shouldBe` 21
      [ cn | p <- drop 16 nanoSetup, (_, Just cn, _, _) <- take 1 (ccs p) ] `shouldBe` [7, 10, 91, 1, 11]
      map (length . ccs) (drop 16 nanoSetup) `shouldBe` replicate 5 16

    it "scales the tables to 0-127 with pan centred: firsts hard left, seconds hard right, channel 12 closed" $ do
      let row p c = [ round v :: Int | (_, _, Just v, Just mc) <- ccs p, mc == fromIntegral (c - 1 :: Int) ]
          [level, pan, _, _, expr] = drop 16 nanoSetup
      row pan 13 `shouldBe` [13]
      row pan 14 `shouldBe` [114]
      row pan 8 `shouldBe` [64]
      row level 1 `shouldBe` [46]
      row level 12 `shouldBe` [0]
      row expr 12 `shouldBe` [0]
      row expr 1 `shouldBe` [127]

  it "the GM kit answers on channel 10 with General MIDI note numbers" $
    [ onsets (v "1") | v <- [gmKick, gmKick2, gmSnare, gmHatClosed, gmRide, gmCrash, gmTamb, gmChimes] ]
      `shouldBe` [ [(Just (fromIntegral n - 60), Just 9)] | n <- [36, 35, 40, 42, 51, 49, 54, 84 :: Int] ]
