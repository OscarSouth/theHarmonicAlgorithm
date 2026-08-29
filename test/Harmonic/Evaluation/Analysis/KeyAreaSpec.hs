-- |
-- Module      : Harmonic.Evaluation.Analysis.KeyAreaSpec
-- Description : Key-area detection, chordscale M\/S derivation
--
-- Pins the probe-validated behaviour of the whole-progression key-area
-- analysis (archive\/analysis\/keyarea.md \/ penta.md): composite-minor
-- handling of the minor ii-V-i, DP boundary placement, cyclicity,
-- determinism, the pentatonic pass, and the 'chordscale' layer contract.
--
-- Four of the five key fixtures were harvested from the retired
-- 'inferKeyCentre' spec (notes\/walking_bass_theory.md:477-486). The
-- BbM7-Eb7-AbM7 fixture is deliberately re-pinned: the old pooled-window
-- vote answered Ab for all three bars, but BbM7's chord tones (D, A) are
-- not in Ab major — the per-bar honest reading is Bb Ab Ab (ruled
-- 2026-08-29 alongside the calibration freeze).
module Harmonic.Evaluation.Analysis.KeyAreaSpec (spec) where

import Test.Hspec

import           Data.Foldable (toList)
import           Data.List (nub)
import qualified Data.Set as Set
import qualified Data.Vector as V

import           Harmonic.Evaluation.Analysis.KeyArea
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as Pt
import qualified Harmonic.Rules.Types.Progression as Pr
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import qualified Harmonic.Rules.Types.Scale as Sc

mkCS :: Pt.NoteName -> [Int] -> H.CadenceState
mkCS r = H.mkCadenceStatePCs r H.Unison

prog :: [H.CadenceState] -> Pr.Progression
prog = Pr.fromCadenceStates

keysOf :: Pr.Progression -> [KeyArea]
keysOf = map baKey . analyzeProgression

maj, minor :: Int -> KeyArea
maj t   = KeyArea t MajorKey
minor t = KeyArea t MinorKey

-- Fixtures ------------------------------------------------------------------

iiVIpair :: Pr.Progression
iiVIpair = prog
  [ mkCS Pt.D [0,3,7,10], mkCS Pt.G [0,4,7,10], mkCS Pt.C [0,4,7,11], mkCS Pt.C [0,4,7,11]
  , mkCS Pt.G [0,3,7,10], mkCS Pt.C [0,4,7,10], mkCS Pt.F [0,4,7,11], mkCS Pt.F [0,4,7,11] ]

minorIIVi :: Pr.Progression
minorIIVi = prog
  [ mkCS Pt.B [0,3,6,10], mkCS Pt.E [0,1,4,7,10]
  , mkCS Pt.A [0,3,7,11], mkCS Pt.A [0,3,7,9] ]

autumnA :: Pr.Progression
autumnA = prog
  [ mkCS Pt.C [0,3,7,10], mkCS Pt.F [0,4,7,10], mkCS Pt.Bb [0,4,7,11], mkCS Pt.Eb [0,4,7,11]
  , mkCS Pt.A [0,3,6,10], mkCS Pt.D [0,1,4,7,10], mkCS Pt.G [0,3,7,9], mkCS Pt.G [0,3,7,9] ]

spec :: Spec
spec = do

  describe "key detection (harvested fixtures)" $ do

    it "Am7 Em7 Bm7 infers G" $
      keysOf (prog [mkCS Pt.A [0,3,7,10], mkCS Pt.E [0,3,7,10], mkCS Pt.B [0,3,7,10]])
        `shouldBe` replicate 3 (maj 7)

    it "FM7 Am7 BbM7 infers F" $
      keysOf (prog [mkCS Pt.F [0,4,7,11], mkCS Pt.A [0,3,7,10], mkCS Pt.Bb [0,4,7,11]])
        `shouldBe` replicate 3 (maj 5)

    it "FM7 Em7 Dm7 infers C" $
      keysOf (prog [mkCS Pt.F [0,4,7,11], mkCS Pt.E [0,3,7,10], mkCS Pt.D [0,3,7,10]])
        `shouldBe` replicate 3 (maj 0)

    it "DM7 F#m7 Bm7 GM7 infers D" $
      keysOf (prog [ mkCS Pt.D [0,4,7,11], mkCS Pt.F' [0,3,7,10]
                   , mkCS Pt.B [0,3,7,10], mkCS Pt.G [0,4,7,11] ])
        `shouldBe` replicate 4 (maj 2)

    -- Re-pinned: the old pooled-window vote said Ab throughout, but BbM7's
    -- chord tones (D, A) are not in Ab major. Per-bar honesty reads Bb.
    it "BbM7 Eb7 AbM7 infers Bb Ab Ab (re-pinned from whole-phrase Ab)" $
      keysOf (prog [mkCS Pt.Bb [0,4,7,11], mkCS Pt.Eb [0,4,7,10], mkCS Pt.Ab [0,4,7,11]])
        `shouldBe` [maj 10, maj 8, maj 8]

  describe "composite minor (the minor ii-V-i is ONE key area)" $ do

    it "Bm7b5 E7b9 AmM7 Am6 sits in a single A-minor area" $
      keysOf minorIIVi `shouldBe` replicate 4 (minor 9)

    it "per-bar forms: harmonic on ii and V, melodic on the tonic bars" $
      map baForm (analyzeProgression minorIIVi)
        `shouldBe` [HarmForm, HarmForm, MelForm, MelForm]

    it "modes classify as Locrian nat.6 / Phrygian dominant / MelMin at the bar roots" $
      map (fmap Sc.modeQuality . baMode) (analyzeProgression minorIIVi)
        `shouldBe` map Just [Sc.LocNat6, Sc.PhryNat3, Sc.MelMin, Sc.MelMin]

    it "every chord is contained in its bar's M set (no overrides needed)" $ do
      map baTier (analyzeProgression minorIIVi) `shouldBe` replicate 4 TierForm

  describe "boundary placement" $ do

    it "the modulating ii-V-I pair splits C -> F exactly at the second ii" $ do
      keysOf iiVIpair `shouldBe` (replicate 4 (maj 0) ++ replicate 4 (maj 5))
      map baBoundary (analyzeProgression iiVIpair)
        `shouldBe` [True, False, False, False, True, False, False, False]

    it "Autumn Leaves A splits Bb -> Gm at the half-diminished bar" $
      keysOf autumnA
        `shouldBe` (replicate 4 (maj 10) ++ replicate 4 (minor 7))

  describe "cyclicity and determinism" $ do

    it "rotating the bars rotates the keys (wrap edge is real)" $ do
      let rot n xs = drop n xs ++ take n xs
          bars = toList (Pr.unProgression autumnA)
      keysOf (prog (rot 3 bars)) `shouldBe` rot 3 (keysOf autumnA)

    it "analysis is deterministic" $
      analyzeProgression iiVIpair `shouldBe` analyzeProgression iiVIpair

    it "empty progression is total" $
      analyzeProgression (prog []) `shouldBe` []

    it "single-bar progression is total" $
      length (analyzeProgression (prog [mkCS Pt.C [0,4,7]])) `shouldBe` 1

  describe "silent bars" $ do

    -- A genuinely empty cadence (mkCadenceStatePCs would force-insert the
    -- root, leaving one tone; raw construction keeps zero).
    let restBar  = H.CadenceState (H.Cadence "" H.Unison []) Pt.C H.FlatSpelling
        withRest = prog [ mkCS Pt.C [0,4,7], restBar
                        , mkCS Pt.G [0,4,7], mkCS Pt.F [0,4,7] ]

    it "a bar with no chord tones is key-neutral (inherits the neighbouring key)" $
      keysOf withRest `shouldBe` replicate 4 (maj 0)

    it "the derived M layer stays exactly 7 tones across a silent bar" $ do
      let ctx = chordscale (PC.fromProgression withRest)
      [ length (H.cadenceIntervals (H.stateCadence cs))
        | cs <- toList (Pr.unProgression (PC.modeLayer ctx)) ]
        `shouldBe` replicate 4 7

  describe "pentatonic pass" $ do

    it "S vocabulary facts: 3 pentatonics per diatonic set, 1 in melodic minor, 0 in harmonic minor/major" $ do
      let pentaAt r  = Set.fromList
            [ Pt.unPitchClass p | p <- map (Pt.transpose r) (Sc.pentaChroma
                (Sc.Pentatonic Sc.MajorPenta (Pt.mkPitchClass 0))) ]
          setOf q    = Set.fromList (map Pt.unPitchClass
                         (Sc.modeChroma (Sc.Mode q (Pt.mkPitchClass 0))))
          countIn s  = length [ r | r <- [0 .. 11], pentaAt r `Set.isSubsetOf` s ]
      countIn (setOf Sc.Ionian)  `shouldBe` 3
      countIn (setOf Sc.MelMin)  `shouldBe` 1
      countIn (setOf Sc.HarmMin) `shouldBe` 0
      countIn (setOf Sc.HarmMaj) `shouldBe` 0

    it "holds one pentatonic across the whole C -> F ii-V-I (F pent sits in both keys)" $ do
      let anns = analyzeProgression iiVIpair
      nub (map baPentaRoot anns) `shouldBe` [5]
      map baPentaInKey anns `shouldBe` replicate 8 True

    it "flags out-of-key pentatonics only on the harmonic-form bars of Autumn Leaves" $ do
      let anns = analyzeProgression autumnA
      [ i | (i, a) <- zip [1 :: Int ..] anns, not (baPentaInKey a) ]
        `shouldBe` [5, 6]

  describe "barPalettes" $ do

    it "one 7-PC palette per bar, equal to the M sets" $ do
      let anns = analyzeProgression autumnA
          pals = barPalettes autumnA
      V.length pals `shouldBe` 8
      V.toList pals `shouldBe` map baModeSet anns
      all ((== 7) . Set.size) (V.toList pals) `shouldBe` True

  describe "chordscale (layer derivation)" $ do

    let ctx  = PC.fromProgression autumnA
        ctx' = chordscale ctx
        cardsOf lyr = [ length (H.cadenceIntervals (H.stateCadence cs))
                      | cs <- toList (Pr.unProgression (PC.layer lyr ctx')) ]

    it "fills S with 5-tone and M with 7-tone bars; T untouched" $ do
      cardsOf PC.S `shouldBe` replicate 8 5
      cardsOf PC.M `shouldBe` replicate 8 7
      PC.triadLayer ctx' `shouldBe` PC.triadLayer ctx

    it "keeps family and leaves provenance empty" $ do
      PC.pcFamily ctx' `shouldBe` PC.pcFamily ctx
      PC.pcProvenance ctx' `shouldBe` Nothing

    it "layer bars carry the analysis sets (M on the harmonic root, S on the pentatonic root)" $ do
      let absPCs cs =
            let r = Pt.unPitchClass (Pt.pitchClass (H.stateCadenceRoot cs))
            in Set.fromList [ (r + Pt.unPitchClass iv) `mod` 12
                            | iv <- H.cadenceIntervals (H.stateCadence cs) ]
          anns = analyzeProgression autumnA
      map absPCs (toList (Pr.unProgression (PC.modeLayer ctx')))
        `shouldBe` map baModeSet anns
      [ Pt.unPitchClass (Pt.pitchClass (H.stateCadenceRoot cs))
        | cs <- toList (Pr.unProgression (PC.strataLayer ctx')) ]
        `shouldBe` map baPentaRoot anns

    it "is the identity on FStrata and FPoly contexts" $ do
      let fake = ctx { PC.pcFamily = PC.FPoly }
      chordscale fake `shouldBe` fake

    it "is total on the empty context" $
      chordscale mempty `shouldBe` mempty
