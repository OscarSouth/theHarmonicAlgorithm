-- |
-- Module      : Harmonic.Interface.Tidal.DisplaySpec
-- Description : Pins the display time-unit doctrine (unprimed = bars, primed = seconds)
module Harmonic.Interface.Tidal.DisplaySpec (spec) where

import Test.Hspec
import qualified Harmonic.Rules.Types.Progression as P
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as Pitch
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import Harmonic.Interface.Tidal.Form
import Harmonic.Interface.Tidal.Display
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Sound.Tidal.Context

spec :: Spec
spec = describe "display feed: unprimed = bars, primed = seconds" $ do
  let cs   = H.CadenceState (H.Cadence "maj" H.Unison (map Pitch.mkPitchClass [0,4,7])) Pitch.C H.FlatSpelling
      prog = PC.fromProgression $ P.Progression $ Seq.fromList [cs, cs]
      -- 120 bpm (cps 2), 32-bar form: loop = 128 cycles = 64 seconds
      kin  = formK 120 [rh 0 0 0 prog, rh 16 1 1 prog, rh 32 0 0 prog]
      k    = (kin, pure 1 :: Pattern Int)
      num v = case v of { VF x -> Just x; VN x -> Just (realToFrac x); _ -> Nothing }
      cc n p = [ v | e <- queryArc p (Arc 0 1), eventHasOnset e
                   , (Map.lookup "ctlNum" (value e) >>= num) == Just n
                   , Just v <- [Map.lookup "control" (value e) >>= num] ]

  it "displayClock broadcasts the loop length in BARS (CC 115 low byte = 32)" $
    cc 115 (displayClock k) `shouldBe` [32]

  it "displayClock' broadcasts the loop length in SECONDS (CC 115 low byte = 64)" $
    cc 115 (displayClock' k) `shouldBe` [64]
