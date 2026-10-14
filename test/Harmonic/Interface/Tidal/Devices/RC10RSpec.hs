-- |
-- Module      : Harmonic.Interface.Tidal.Devices.RC10RSpec
-- Description : Pins the RC-10R lanes: CC numbers, press shape, level scaling, kinetics gate, drum pads
module Harmonic.Interface.Tidal.Devices.RC10RSpec (spec) where

import Test.Hspec
import qualified Harmonic.Rules.Types.Progression as P
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as Pitch
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import Harmonic.Interface.Tidal.Form
import Harmonic.Interface.Tidal.Devices.RC10R
import Harmonic.Interface.Tidal.Devices.JV1010
import Harmonic.Interface.Tidal.Instruments (vel)
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Sound.Tidal.Context

spec :: Spec
spec = describe "RC-10R lane (ch 10, CC 70-84)" $ do
  let num v = case v of { VF x -> Just x; VN x -> Just (realToFrac (unNote x)); VI x -> Just (fromIntegral x); _ -> Nothing }
      field f e = Map.lookup f (value e) >>= num
      -- (onset, ctlNum, value) for every onset in the arc
      ccs p a b = [ (start (part e), cn, v)
                  | e <- queryArc p (Arc a b), eventHasOnset e
                  , Just cn <- [field "ctlNum" e], Just v <- [field "control" e] ]

  it "a press is 127 at the onset, then 0 a sixteenth later, on midichan 9" $ do
    ccs (rcFill "1") 0 1 `shouldBe` [(0, 79, 127), (1/16, 79, 0)]
    [ field "midichan" e | e <- queryArc (rcFill "1") (Arc 0 1), eventHasOnset e ] `shouldBe` [Just 9, Just 9]

  it "each function sits on its assigned CC" $
    [ cn | p <- [ rcTrk1, rcTrk1Stop, rcTrk2, rcTrk2Stop, rcStop, rcBreak, rcRhythmBreak
                , rcRhythm, rcRhythmStop, rcFill, rcPattern ]
         , (_, cn, 127) <- ccs (p "1") 0 1 ]
      `shouldBe` [70 .. 80]

  it "rcKit sends the kit number itself on CC 84" $
    ccs (rcKit 8) 0 1 `shouldBe` [(0, 84, 8)]

  it "levels scale 0-1 to 0-127, four messages per cycle (sixteen per bar)" $ do
    ccs (rcLoopLevel 0.5) 0 1 `shouldBe` [ (t, 81, 64) | t <- [0, 1/4, 1/2, 3/4] ]
    [ v | (_, 82, v) <- ccs (rcRhythmLevel 2) 0 1 ] `shouldBe` replicate 4 127
    [ cn | (_, cn, _) <- ccs (rcLevel 1) 0 1 ] `shouldBe` replicate 4 83

  describe "rcGate: a track entering and leaving with the form's kinetics" $ do
    let cs   = H.CadenceState (H.Cadence "maj" H.Unison (map Pitch.mkPitchClass [0,4,7])) Pitch.C H.FlatSpelling
        prog = PC.fromProgression $ P.Progression $ Seq.fromList [cs]
        -- 120 bpm, snaps: kinetics 0 for bars 0-1, 0.5 for bars 2-3, 0 for bars 4-5; 6-bar loop
        kin  = formK 120 [rh' 1 0 0 prog, rh' 3 0.5 0.5 prog, rh' 5 0 0 prog, rh' 7 0 0 prog]
        k    = (kin, pure 1 :: Pattern Int)

    it "START on the first bar inside the band, STOP on the first bar outside, nothing else" $
      [ (t, cn) | (t, cn, 127) <- ccs (rcGate (0.3, 1) k 1) 0 24 ] `shouldBe` [(8, 70), (16, 71)]

    it "track 2 uses TRK2's CCs" $
      [ cn | (_, cn, 127) <- ccs (rcGate (0.3, 1) k 2) 0 24 ] `shouldBe` [72, 73]

    it "a band the form never enters sends nothing" $
      ccs (rcGate (0.8, 1) k 1) 0 24 `shouldBe` []

  describe "x3rc10: the drum lane (ch 10, CC 33-48)" $ do
    it "a pad's hit is CC 33 + pad, on midichan 9" $ do
      ccs (x3rc10 (kick "1" |* vel 1)) 0 1 `shouldBe` [(0, 33, 127)]
      [ field "midichan" e | e <- queryArc (x3rc10 (kick "1")) (Arc 0 1) ] `shouldBe` [Just 9]

    it "a hit without amp takes SuperDirt's default (0.4)" $
      ccs (x3rc10 (kick "1")) 0 1 `shouldBe` [(0, 33, 51)]

    it "the final velocity is the value" $
      ccs (x3rc10 (snare "1" |* vel 0.8)) 0 1 `shouldBe` [(0, 40, 102)]

    it "the sixteen pads sit on CC 33-48" $
      [ cn | p <- [ kick, snap, hhcl, hhop, ride, crash, click, snare
                  , ride2, crash2, claves, snare2, hhpd, bell, ftom, kick2 ]
           , (_, cn, _) <- ccs (x3rc10 (p "1")) 0 1 ]
        `shouldBe` [33 .. 48]

    it "drops silent hits and notes outside the 16 pads" $ do
      ccs (x3rc10 (kick "1" |* vel 0)) 0 1 `shouldBe` []
      ccs (x3rc10 (kick' "1")) 0 1 `shouldBe` []
