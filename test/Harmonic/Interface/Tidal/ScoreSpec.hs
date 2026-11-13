-- |
-- Module      : Harmonic.Interface.Tidal.ScoreSpec
-- Description : Tests for score mode (bars, score, inKey, degrees, dyn, ramp, preroll)

module Harmonic.Interface.Tidal.ScoreSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Data.Foldable (toList)

import Harmonic.Interface.Tidal.Score
import Harmonic.Interface.Tidal.Form (IK, lK, iK, rh, preroll, Kinetics(..))
import Harmonic.Interface.Tidal.Arranger (fromChords)
import Harmonic.Interface.Tidal.Recall (recallE, renderChart, familyWord)
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as Pitch
import qualified Harmonic.Rules.Types.Progression as Pr
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import Harmonic.Rules.Types.ProgressionContext (Layer(..))
import Sound.Tidal.Context

-- | Onsets and values of a pattern over an arc.
onsets :: Pattern a -> Arc -> [(Time, a)]
onsets pat arc = [ (start (part e), value e) | e <- queryArc pat arc, eventHasOnset e ]

-- | Absolute pitch classes of bar n of a layer.
layerPCs :: Layer -> PC.ProgressionContext -> Int -> [Int]
layerPCs lyr ctx n =
  let cs = toList (Pr.unProgression (PC.layer lyr ctx)) !! n
      r  = Pitch.unPitchClass (Pitch.pitchClass (H.stateCadenceRoot cs))
  in [ (r + Pitch.unPitchClass iv) `mod` 12 | iv <- H.cadenceIntervals (H.stateCadence cs) ]

-- | A constant performance context over a progression, one chord per bar.
constK :: PC.ProgressionContext -> IK
constK ctx = lK 1 1 ctx (slow (fromIntegral (PC.pcLength ctx) * 4) (fastcat (map pure [1 .. PC.pcLength ctx])))

-- | A three-bar ostinato in the Andante's shape: one sub-bar per bar.
ostinato :: Pattern String
ostinato = "[[<~ x x> p] q r]/12"

spec :: Spec
spec = do
  describe "bars" $ do
    it "addresses 1-indexed bars of four cycles, end inclusive" $ do
      let (s, e, _) = bars 1 1 (pure 'a')
          (s', e', _) = bars 4 6 (pure 'a')
      (s, e) `shouldBe` (0, 4)
      (s', e') `shouldBe` (12, 24)

    it "runs the block on its own clock: an ostinato starts on its first sub-bar wherever it is placed" $ do
      let at17 = seqP [bars 17 19 ostinato]
      map snd (onsets at17 (Arc 64 68)) `shouldBe` ["p"]        -- bar 17: no leading x, then p
      map snd (onsets at17 (Arc 68 72)) `shouldBe` ["q"]
      map snd (onsets at17 (Arc 72 76)) `shouldBe` ["r"]

    it "counts <> alternation passes from the block entry" $ do
      let blk = seqP [bars 1 9 ostinato]
      map snd (onsets blk (Arc 0 4))  `shouldBe` ["p"]           -- pass 0: rest first
      map snd (onsets blk (Arc 12 16)) `shouldBe` ["x", "p"]     -- pass 1: x
      map snd (onsets blk (Arc 24 28)) `shouldBe` ["x", "p"]     -- pass 2: x

    it "plays nothing outside its range" $ do
      let blk = seqP [bars 2 2 (pure 'a' :: Pattern Char)]
      onsets blk (Arc 0 4) `shouldBe` []
      map fst (onsets blk (Arc 0 12)) `shouldBe` [4, 5, 6, 7]

  describe "score" $ do
    it "shifts the whole sequence by the pre-roll, keeping block order" $ do
      let sq = score 3 [bars 1 1 (pure 'a'), bars 2 2 (pure 'b')]
      onsets sq (Arc 0 12) `shouldBe` []
      map snd (onsets sq (Arc 12 16)) `shouldBe` "aaaa"
      map snd (onsets sq (Arc 16 20)) `shouldBe` "bbbb"

    it "with no pre-roll bar 1 is cycle 0" $
      map fst (onsets (score 0 [bars 1 1 (pure 'a')]) (Arc 0 8)) `shouldBe` [0, 1, 2, 3]

  describe "inKey" $ do
    it "D major: tonic triad on T, the major pentatonic on S, Ionian on M" $ do
      let key = inKey Pitch.D "major"
      PC.pcLength key `shouldBe` 1
      layerPCs T key 0 `shouldBe` [2, 6, 9]
      layerPCs S key 0 `shouldBe` [2, 4, 6, 9, 11]
      layerPCs M key 0 `shouldBe` [2, 4, 6, 7, 9, 11, 1]
      map (H.stateCadenceRoot . (!! 0) . toList . Pr.unProgression . flip PC.layer key) [T, S, M]
        `shouldBe` [Pitch.D, Pitch.D, Pitch.D]

    it "A minor: minor triad, the minor pentatonic rooted on A, Aeolian" $ do
      let key = inKey Pitch.A "minor"
      layerPCs T key 0 `shouldBe` [9, 0, 4]
      layerPCs S key 0 `shouldBe` [9, 0, 2, 4, 7]
      layerPCs M key 0 `shouldBe` [9, 11, 0, 2, 4, 5, 7]

    it "D dorian reads the parent key's pentatonic from D; a Locrian tonic falls back to its parent root" $ do
      layerPCs S (inKey Pitch.D "dorian") 0 `shouldBe` [2, 4, 7, 9, 0]
      layerPCs T (inKey Pitch.D "dorian") 0 `shouldBe` [2, 5, 9]
      H.stateCadenceRoot (toList (Pr.unProgression (PC.strataLayer (inKey Pitch.B "locrian"))) !! 0)
        `shouldBe` Pitch.C

    it "melodic and harmonic families take the mode's degrees 1 2 3 5 6 as the pentatonic" $
      layerPCs S (inKey Pitch.C "harmonic minor") 0 `shouldBe` [0, 2, 3, 7, 8]

    it "chromatic puts all twelve on every layer" $
      map (\l -> length (layerPCs l (inKey Pitch.C "chromatic") 0)) [T, S, M] `shouldBe` [12, 12, 12]

    it "accepts constructor names and is case-insensitive" $ do
      layerPCs M (inKey Pitch.C "LydDom") 0 `shouldBe` [0, 2, 4, 6, 7, 9, 10]
      layerPCs M (inKey Pitch.C "Major") 0 `shouldBe` layerPCs M (inKey Pitch.C "major") 0

    it "rejects an unknown name at declaration" $
      evaluate (length (show (inKey Pitch.D "majr"))) `shouldThrow` anyException

    it "round-trips through the recall chart" $ do
      let key = inKey Pitch.D "major"
      recallE (familyWord (PC.pcFamily key)) (renderChart key) `shouldBe` Right key

  describe "degrees" $ do
    let keyD = constK (inKey Pitch.D "major")

    it "at octave 1 reproduces the Andante degree table −14..9 on the M layer" $ do
      let degs = [-14 .. 9] :: [Int]
          expect = [-22, -20, -18, -17, -15, -13, -11, -10, -8, -6, -5, -3, -1, 1,
                    2, 4, 6, 7, 9, 11, 13, 14, 16, 18]
      map snd (onsets (degrees M 1 keyD (fastcat (map pure degs))) (Arc 0 1))
        `shouldBe` map fromIntegral expect

    it "degree 0 at octave 0 is the root at the lattice's bass register (D3)" $ do
      map snd (onsets (degrees M 0 keyD "0 7") (Arc 0 1)) `shouldBe` [-10, 2]
      map snd (onsets (degrees T 0 keyD "0 1 2 3") (Arc 0 1)) `shouldBe` [-10, -6, -3, 2]
      map snd (onsets (degrees S 0 keyD "0 4 5") (Arc 0 1)) `shouldBe` [-10, -1, 2]

    it "reads the chord current at each onset and wraps by octave" $ do
      let cg = constK (fromChords [[0, 4, 7], [7, 11, 2]])     -- C major, then G major
      map snd (onsets (degrees T 0 cg "0") (Arc 0 8)) `shouldBe` replicate 4 (-12) ++ replicate 4 (-5)
      map snd (onsets (degrees T 0 cg "-1 3") (Arc 0 1)) `shouldBe` [-17, 0]

    it "an octave chord doubles each note" $
      map snd (onsets (degrees M "[1,2]" keyD "0") (Arc 0 1)) `shouldBe` [2, 14]

  describe "preroll" $ do
    let key = inKey Pitch.D "major"
        k   = iK 120 [rh 1 0 0.2 key, rh 3 0 0.8 key] (rep' key)
        rep' ctx = slow 4 (fastcat (map pure [1 .. PC.pcLength ctx]))
        k'  = preroll 3 k
        vals pat arc = map value (queryArc pat arc)

    it "delays the dynamic arc and the chord selection by the pre-roll bars" $ do
      vals (kDynamic (fst k')) (Arc 12 13) `shouldBe` vals (kDynamic (fst k)) (Arc 0 1)
      vals (snd k') (Arc 12 13) `shouldBe` vals (snd k) (Arc 0 1)
      vals (kDynamic (fst k')) (Arc 12 13) /= vals (kDynamic (fst k')) (Arc 16 17) `shouldBe` True

  describe "staff" $ do
    it "joins one string per bar into a weighted sequence over the given beats" $ do
      -- two bars: a minim and two crotchets, then a semibreve held from a tie
      let pat = staff 8 ["4@2 7 9", "12@4"] :: Pattern Int
      onsets pat (Arc 0 8) `shouldBe` [(0, 4), (2, 7), (3, 9), (4, 12)]

    it "a note held across a bar line is one step: the next string carries only the rest of its bar" $ do
      let pat = staff 8 ["~@3 4@3", "7@2"] :: Pattern Int
      onsets pat (Arc 0 8) `shouldBe` [(3, 4), (6, 7)]
      map (\e -> (start (part e), stop (part e))) (queryArc pat (Arc 0 8)) `shouldBe` [(3, 6), (6, 8)]

  describe "dyn" $ do
    it "is the 2021 ladder, p = 0.40" $ do
      map snd (onsets (dyn "ffff fff ff f mf mp p pp ppp pppp") (Arc 0 1))
        `shouldBe` [1, 0.9, 0.75, 0.65, 0.55, 0.45, 0.40, 0.35, 0.30, 0.25]

    it "drops a mark it does not know" $
      onsets (dyn "p forte mf") (Arc 0 1) `shouldBe` [(0, 0.40), (2/3, 0.55)]

  describe "ramp" $ do
    it "is 64 ascending steps from lo towards hi over one cycle" $ do
      let vs = map snd (onsets (ramp 0.9 1.3) (Arc 0 1))
      length vs `shouldBe` 64
      take 1 vs `shouldBe` [0.9]
      and (zipWith (<) vs (drop 1 vs)) `shouldBe` True
      all (< 1.3) vs `shouldBe` True
