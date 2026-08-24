-- |
-- Module      : Harmonic.Interface.Tidal.LineHarmonySpec
-- Description : Smoke tests for the walking-bass Tidal interface
module Harmonic.Interface.Tidal.LineHarmonySpec (spec) where

import Test.Hspec

import qualified Harmonic.Rules.Types.Progression as P
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import Harmonic.Interface.Tidal.Form (Kinetics(..))
import Harmonic.Interface.Tidal.Groove (fund)
import Harmonic.Interface.Tidal.LineHarmony
import Harmonic.Interface.Tidal.Bridge (warp, rep)
import Harmonic.Traversal.WalkingBass (closestLowMidi, walkLine)

import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Sound.Tidal.Context

-------------------------------------------------------------------------------
-- Fixtures
-------------------------------------------------------------------------------

testProgression :: P.Progression
testProgression = P.Progression $ Seq.fromList
  [ H.initCadenceState 0 "C" [0,4,7]
  , H.initCadenceState 0 "G" [0,4,7]
  , H.initCadenceState 0 "A" [0,3,7]
  , H.initCadenceState 0 "F" [0,4,7]
  ]

fullKin :: P.Progression -> Kinetics
fullKin pr = Kinetics (pure 1.0) (pure 1.0) (pure (PC.fromProgression pr)) 0 0

mutedKin :: P.Progression -> Kinetics
mutedKin pr = Kinetics (pure 0.0) (pure 1.0) (pure (PC.fromProgression pr)) 0 0

kinAt :: Double -> P.Progression -> Kinetics
kinAt v pr = Kinetics (pure v) (pure 1.0) (pure (PC.fromProgression pr)) 0 0

-------------------------------------------------------------------------------
-- Onset helpers (mirrors BridgeSpec)
-------------------------------------------------------------------------------

queryOnsets :: ControlPattern -> Arc -> [(Rational, Double)]
queryOnsets pat arc =
  [ (start (part ev), noteVal)
  | ev <- queryArc pat arc
  , isOnset ev
  , Just noteVal <- [extractNote (value ev)]
  ]
  where
    isOnset ev = case whole ev of
      Just w  -> start w == start (part ev)
      Nothing -> False
    extractNote vm = case Map.lookup "note" vm of
      Just (VN n) -> Just (unNote n)
      Just (VF v) -> Just v
      _           -> Nothing

onsetCount :: ControlPattern -> Arc -> Int
onsetCount pat arc = length (queryOnsets pat arc)

onsetNotes :: ControlPattern -> Arc -> [Double]
onsetNotes pat arc = map snd (queryOnsets pat arc)

-------------------------------------------------------------------------------
-- Spec
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "lineHarmony" $ do

    it "produces events from a simple beat pattern" $ do
      let chordSel = parseBP_E "1" :: Pattern Int
          result  = lineHarmony (pure 1.0) (fullKin testProgression, chordSel) fund
                      [parseBP_E "[1 2 3 4]"]
      onsetCount result (Arc 0 1) `shouldSatisfy` (> 0)

    it "returns silence for empty progression" $ do
      let emptyProg = P.Progression Seq.empty
          chordSel  = parseBP_E "1" :: Pattern Int
          result    = lineHarmony (pure 1.0) (fullKin emptyProg, chordSel) fund
                        [parseBP_E "[1 2 3 4]"]
      onsetCount result (Arc 0 1) `shouldBe` 0

    it "beat 1 event at bar 0 equals walking-bass first note (Tidal-shifted)" $ do
      let chordSel = parseBP_E "1" :: Pattern Int
          result   = lineHarmony (pure 1.0) (fullKin testProgression, chordSel) fund
                       [parseBP_E "1"]
          notes    = onsetNotes result (Arc 0 1)
          -- MIDI 36 (C2) emitted as note -12 after tidalNoteOffset=48 shift.
          expected = fromIntegral (closestLowMidi 0 - 48) :: Double
      take 1 notes `shouldBe` [expected]

    it "kinetics windowing: upper window plays at high kinetics" $ do
      -- N=2 list ["~", "[1 2 3 4]"]: window 1 = (0.5, 1] → playing pattern.
      let chordSel = parseBP_E "1" :: Pattern Int
          result   = lineHarmony (pure 1.0) (fullKin testProgression, chordSel) fund
                       [parseBP_E "~", parseBP_E "[1 2 3 4]"]
      onsetCount result (Arc 0 1) `shouldSatisfy` (> 0)

    it "kinetics windowing: lower window plays at low kinetics" $ do
      -- N=2 list ["~", "[1 2 3 4]"]: window 0 = [0, 0.5] → silence pattern.
      let chordSel = parseBP_E "1" :: Pattern Int
          result   = lineHarmony (pure 1.0) (mutedKin testProgression, chordSel) fund
                       [parseBP_E "~", parseBP_E "[1 2 3 4]"]
      onsetCount result (Arc 0 1) `shouldBe` 0

    it "kinetics windowing: boundary belongs to the lower window" $ do
      -- N=2 boundary at 0.5 belongs to window 0; "~" plays, no events.
      let chordSel = parseBP_E "1" :: Pattern Int
          result   = lineHarmony (pure 1.0) (kinAt 0.5 testProgression, chordSel) fund
                       [parseBP_E "~", parseBP_E "[1 2 3 4]"]
      onsetCount result (Arc 0 1) `shouldBe` 0

    it "kinetics windowing: N=3 mid-kinetics activates middle window" $ do
      -- N=3 windows: [0, 1/3], (1/3, 2/3], (2/3, 1].
      -- kSignal=0.5 ∈ (1/3, 2/3] → middle pattern "[1 2 3 4]" plays.
      let chordSel = parseBP_E "1" :: Pattern Int
          result   = lineHarmony (pure 1.0) (kinAt 0.5 testProgression, chordSel) fund
                       [parseBP_E "~", parseBP_E "[1 2 3 4]", parseBP_E "~"]
      onsetCount result (Arc 0 1) `shouldSatisfy` (> 0)

    it "events stay within the cycle bounds" $ do
      let chordSel = parseBP_E "1" :: Pattern Int
          result   = lineHarmony (pure 1.0) (fullKin testProgression, chordSel) fund
                       [parseBP_E "[1 2 3 4]"]
          onsets   = queryOnsets result (Arc 0 1)
      all (\(t, _) -> t >= 0 && t < 1) onsets `shouldBe` True

  describe "resolvePerformedSeq" $ do

    it "rep s 1 resolves to sequential stored order" $ do
      let tc = PC.fromProgression testProgression
      resolvePerformedSeq (rep tc 1) `shouldBe` Just [1, 2, 3, 4]

    it "rep s 2 duplicates each bar in place" $ do
      let tc = PC.fromProgression testProgression
      resolvePerformedSeq (rep tc 2) `shouldBe` Just [1, 1, 2, 2, 3, 3, 4, 4]

    it "warp resolves to the written bar order" $ do
      resolvePerformedSeq (warp "[1 2 1 3]/4") `shouldBe` Just [1, 2, 1, 3]

    it "a constant selector resolves to a one-bar period" $ do
      resolvePerformedSeq (parseBP_E "1" :: Pattern Int) `shouldBe` Just [1]

    it "sub-bar selection falls back to Nothing" $ do
      resolvePerformedSeq (warp "[1 2]/1") `shouldBe` Nothing

    it "gapped selection falls back to Nothing" $ do
      resolvePerformedSeq (warp "[1 ~]/2") `shouldBe` Nothing

  describe "performed-order walking" $ do

    -- One bar = 4 cycles; the beat pattern "[1 2 3 4]/4" emits one beat per
    -- cycle, so querying 4*P cycles yields the full performed period.
    let runWalk chordSel nBarsQ =
          let result = lineHarmony (pure 1.0)
                         (fullKin testProgression, chordSel) fund
                         [parseBP_E "[1 2 3 4]/4"]
          in onsetNotes result (Arc 0 (4 * fromIntegral (nBarsQ :: Int)))
        shifted line = [ fromIntegral m - 48 :: Double | bar <- line, m <- bar ]
        storedBars   = [ H.initCadenceState 0 "C" [0,4,7]
                       , H.initCadenceState 0 "G" [0,4,7]
                       , H.initCadenceState 0 "A" [0,3,7]
                       , H.initCadenceState 0 "F" [0,4,7] ]

    it "warp \"[1 2 1 3]/4\": the emitted line is the walk of the PERFORMED \
       \progression (approach tones aim at performed successors)" $ do
      let performed = P.fromCadenceStates
            [ storedBars !! 0, storedBars !! 1, storedBars !! 0, storedBars !! 2 ]
      runWalk (warp "[1 2 1 3]/4") 4 `shouldBe` shifted (walkLine fund performed)

    it "rep s 2: adjacent duplicate bars walk as neighbours, not verbatim copies" $ do
      let tc        = PC.fromProgression testProgression
          performed = P.fromCadenceStates (concatMap (replicate 2) storedBars)
          notes     = runWalk (rep tc 2) 8
      notes `shouldBe` shifted (walkLine fund performed)
      -- The two consecutive C-major bars differ (root-fifth beat-1 idiom).
      take 4 notes `shouldNotBe` take 4 (drop 4 notes)

    it "rep s 1 renders identically to the stored-order walk" $ do
      let tc = PC.fromProgression testProgression
      runWalk (rep tc 1) 4 `shouldBe` shifted (walkLine fund testProgression)
