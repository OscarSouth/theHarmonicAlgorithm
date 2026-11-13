-- |
-- Module      : Harmonic.Interface.Tidal.Devices.NanoSynthSpec
-- Description : Pins the NanoSynth rig: channel map, shared pizzicato, silent articulations, the mix, GM kit
module Harmonic.Interface.Tidal.Devices.NanoSynthSpec (spec) where

import Test.Hspec
import Harmonic.Interface.Tidal.Orchestra
import Harmonic.Interface.Tidal.Devices.NanoSynth
import Harmonic.Interface.Tidal.Instruments (ch)
import qualified Data.Map.Strict as Map
import Sound.Tidal.Context hiding (clip)

spec :: Spec
spec = describe "NanoSynth rig (the JV-1010 order, the Mother-32 on 6, pizzicato on 12)" $ do
  let num v = case v of { VF x -> Just x; VN x -> Just (realToFrac (unNote x)); VI x -> Just (fromIntegral x); _ -> Nothing }
      field f e = Map.lookup f (value e) >>= num
      onsets p = [ (field "note" e, field "midichan" e) | e <- queryArc p (Arc 0 1), eventHasOnset e ]

  it "winds, brass, harp and timpani follow the JV-1010 channels; the trombones take 11" $
    map ($ nanoSynth) [rigFlute, rigOboe, rigClarinet, rigBassoon, rigHorn, rigTrombone, rigBasstrom, rigHarp, rigTimpani]
      `shouldBe` [1, 2, 3, 4, 5, 11, 11, 7, 8]

  it "each string section has its own channel; viola shares the bass channel" $
    map ($ nanoSynth) [rigViolin1, rigViolin2, rigViola, rigCello, rigContrabass]
      `shouldBe` [13, 14, 16, 15, 16]

  it "pizz re-routes to channel 12, as on the JV; the other articulations leave the line alone" $ do
    let line = note 1 # ch 13 :: ControlPattern
    [ field "midichan" e | e <- queryArc (line # pizz nanoSynth) (Arc 0 1) ] `shouldBe` [Just 11]
    [ map value (queryArc (line # art nanoSynth) (Arc 0 1)) | art <- [spicc, marc, legg, arco] ]
      `shouldBe` replicate 4 (map value (queryArc line (Arc 0 1)))

  it "the bass drum is the orchestra hit four octaves down on channel 9; the tam-tam is silent" $ do
    onsets (bassdrum nanoSynth "t") `shouldBe` [(Just (-48), Just 8)]
    onsets (tamtam nanoSynth "t") `shouldBe` []

  it "the continuo is the harp channel, 7 as on the JV" $ do
    rigContinuo nanoSynth `shouldBe` 7
    nanoHarpV `shouldBe` (3, 0, 46)
    nanoHarpsichordV `shouldBe` (1, 0, 6)

  describe "the mix" $ do
    it "covers all sixteen channels, the Mother-32's blank" $ do
      map fst nanoPrograms `shouldBe` [1 .. 16]
      lookup 1 nanoPrograms `shouldBe` Just (74, 3)
      lookup 6 nanoPrograms `shouldBe` Just (127, 1)
      lookup 10 nanoPrograms `shouldBe` Just (118, 3)
      lookup 12 nanoPrograms `shouldBe` Just (45, 3)
      lookup 16 nanoPrograms `shouldBe` Just (41, 1)

    it "keeps the 2021 concert values with each program, whatever its channel" $ do
      let byProgram = [ (pb, (l, pn, d, m, e)) | (_, pb, l, pn, d, m, e) <- nanoMix ]
          concert2021 =
            [ ((74, 3), (0.36, -0.2,  0.85, 0, 1)), ((68, 3), (0.32,  0.2,  0.90, 0, 1))
            , ((71, 2), (0.32, -0.25, 0.85, 0, 1)), ((70, 2), (0.40,  0.25, 0.90, 0, 1))
            , ((60, 1), (0.60, -0.4,  0.95, 0, 1)), ((57, 1), (0.90,  0.1,  0.80, 0, 1))
            , ((60, 4), (0.90, -0.3,  0.85, 0, 1)), ((45, 3), (0.50,  0.15, 0.90, 0, 1))
            , ((55, 3), (1.00,  0,    0.90, 1, 1)), ((46, 3), (0.10, -0.2,  0.95, 0, 1))
            , ((118, 3), (0.70, -0.2, 0.85, 0, 1)), ((43, 3), (0.50,  0.3,  0.85, 0, 1))
            , ((41, 1), (0.20, -0.3,  0.95, 0, 1)), ((127, 1), (0, 0, 0, 0, 0)) ]
      [ lookup pb byProgram | (pb, _) <- concert2021 ] `shouldBe` map (Just . snd) concert2021
      -- the two violins share a program and differ only in pan and depth
      [ (l, pn, d) | (c, (49, 0), l, pn, d, _, _) <- nanoMix, c == 13 ] `shouldBe` [(0.40, -0.8, 0.80)]
      [ (l, pn, d) | (c, (49, 0), l, pn, d, _, _) <- nanoMix, c == 14 ] `shouldBe` [(0.30,  0.8, 0.95)]

  describe "cold configuration" $ do
    let ccs p = [ (start (part e), field "ctlNum" e, field "control" e, field "midichan" e) | e <- queryArc p (Arc 0 1), eventHasOnset e, Map.member "ctlNum" (value e) ]
        pcs p = [ (start (part e), field "progNum" e, field "midichan" e) | e <- queryArc p (Arc 0 1), eventHasOnset e, Map.member "progNum" (value e) ]

    it "sends the bank on CC0 before the program change, on the channel" $ do
      ccs (nanoProgram 8 (60, 4)) `shouldBe` [(0, Just 0, Just 4, Just 7)]
      pcs (nanoProgram 8 (60, 4)) `shouldBe` [(1/4, Just 60, Just 7)]

    it "nanoKit puts 15ips Kit (118 / Preset 3) on channel 10" $ do
      ccs nanoKit `shouldBe` [(0, Just 0, Just 3, Just 9)]
      pcs nanoKit `shouldBe` [(1/4, Just 118, Just 9)]

    it "the setup is sixteen programs then level, pan, depth, modulation, expression" $ do
      length nanoSetup `shouldBe` 21
      [ cn | p <- drop 16 nanoSetup, (_, Just cn, _, _) <- take 1 (ccs p) ] `shouldBe` [7, 10, 91, 1, 11]
      map (length . ccs) (drop 16 nanoSetup) `shouldBe` replicate 5 16

    it "scales the tables to 0-127 with pan centred: firsts hard left, seconds hard right, channel 6 closed" $ do
      let row p c = [ round v :: Int | (_, _, Just v, Just mc) <- ccs p, mc == fromIntegral (c - 1 :: Int) ]
          [level, pan, _, modw, expr] = drop 16 nanoSetup
      row pan 13 `shouldBe` [13]
      row pan 14 `shouldBe` [114]
      row pan 9 `shouldBe` [64]
      row modw 9 `shouldBe` [127]
      row level 1 `shouldBe` [46]
      row level 6 `shouldBe` [0]
      row expr 6 `shouldBe` [0]
      row expr 1 `shouldBe` [127]

  it "the orchestra's percussion is the kit on channel 10, above the MPC's band" $ do
    map ($ nanoSynth) [rigTambourine, rigTriangle, rigWoodblock] `shouldBe` [Just (54, 10), Just (81, 10), Just (76, 10)]
    [ onsets (v nanoSynth "1") | v <- [tambourine, triangle, woodblock] ]
      `shouldBe` [ [(Just (fromIntegral n - 60), Just 9)] | n <- [54, 81, 76 :: Int] ]

  it "the GM kit answers on channel 10 with General MIDI note numbers" $
    [ onsets (v "1") | v <- [gmKick, gmKick2, gmSnare, gmHatClosed, gmRide, gmCrash, gmTamb, gmChimes] ]
      `shouldBe` [ [(Just (fromIntegral n - 60), Just 9)] | n <- [36, 35, 40, 42, 51, 49, 54, 84 :: Int] ]
