-- |
-- Module      : Harmonic.Interface.Tidal.UtilsSpec
-- Description : Tests for the Tidal helper utilities (mono')
module Harmonic.Interface.Tidal.UtilsSpec (spec) where

import Test.Hspec
import Harmonic.Interface.Tidal.Utils (mono', retrig)
import Harmonic.Interface.Tidal.Instruments (pad)
import qualified Data.Map.Strict as Map
import Sound.Tidal.Context
  (Pattern, Time, ArcF(..), Arc, queryArc, wholeOrPart, eventHasOnset, value, stack, slow, Value(VF, VN))

spec :: Spec
spec = describe "mono' (latest-note priority monophony)" $ do
  let spans :: Time -> Pattern Int -> [(Time, Time, Int)]
      spans to p = [ (start w, stop w, value e)
                   | e <- queryArc p (Arc 0 to), eventHasOnset e, let w = wholeOrPart e ]

  it "truncates the earlier event at the next onset and keeps the later intact" $
    spans 1 (mono' (stack ["0", "~ 1"])) `shouldBe` [(0, 1/2, 0), (1/2, 1, 1)]

  it "keeps only the first of simultaneous onsets" $
    spans 1 (mono' (stack ["0", "1"])) `shouldBe` [(0, 1, 0)]

  it "leaves non-overlapping events untouched" $
    spans 1 (mono' "0 1 2 3") `shouldBe` [(0, 1/4, 0), (1/4, 1/2, 1), (1/2, 3/4, 2), (3/4, 1, 3)]

  it "truncates across cycle boundaries (lookahead)" $
    spans 2 (mono' (stack [slow 2 "0", "~ ~ ~ 1"]))
      `shouldBe` [(0, 3/4, 0), (3/4, 1, 1), (7/4, 2, 1)]

  it "retrig is mono'" $
    spans 1 (retrig (stack ["0", "~ 1"])) `shouldBe` spans 1 (mono' (stack ["0", "~ 1"]))

  describe "pad (MPC misc-sample pad, ch 12)" $ do
    let num v = case v of { VF x -> Just x; VN x -> Just (realToFrac x); _ -> Nothing }
        evs = [ value e | e <- queryArc (pad 1) (Arc 0 1), eventHasOnset e ]
    it "is pad 1 = MIDI 36 (note -24), channel 12, sustain 0.1, one per cycle" $
      case evs of
        [vm] -> do
          (Map.lookup "note" vm >>= num) `shouldBe` Just (-24)
          (Map.lookup "midichan" vm >>= num) `shouldBe` Just 11
          (Map.lookup "sustain" vm >>= num) `shouldBe` Just 0.1
        _ -> expectationFailure ("expected one onset per cycle, got " ++ show (length evs))
