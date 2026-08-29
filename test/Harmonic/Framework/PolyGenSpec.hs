-- |
-- Module      : Harmonic.Framework.PolyGenSpec
-- Description : Tests for the polytonal genE paradigm (PolyGen)
--
-- Offline pins (seek "none"): the partner lists are empty, so every bar
-- draws from the pure enumeration tier — the invariants below must hold
-- there exactly as on the list tier, and offline runs exercise the
-- fallback floor categorically. The LIST tier needs a populated graph, so
-- its guard (partner masks are 3-PC) is covered only by the shared
-- cardinality pin below until the online suite exists.

module Harmonic.Framework.PolyGenSpec (spec) where

import           Test.Hspec
import           Control.Exception (evaluate, try, SomeException)
import           Data.Bits ((.&.), (.|.), popCount, setBit)
import           Data.Foldable (toList)
import           Data.List (foldl', nub, sort)

import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as P
import qualified Harmonic.Rules.Types.Progression as Prog
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import           Harmonic.Evaluation.Scoring.Dissonance (dissonanceScore)
import           Harmonic.Interface.Tidal.PolytonalT (polyLayerViews)
import           Harmonic.Interface.Tidal.Bridge (layerForVoicing)
import           Harmonic.Framework.Builder
                   (genE, genFrom, cue, len, entropy, seek, tonal, hcOvertones, hContext)

absMask :: H.CadenceState -> Int
absMask cs =
  let r = P.unPitchClass (P.pitchClass (H.stateCadenceRoot cs))
  in foldl' setBit 0 [ (r + P.unPitchClass iv) `mod` 12
                     | iv <- H.cadenceIntervals (H.stateCadence cs) ]

barsOf :: Prog.Progression -> [H.CadenceState]
barsOf = toList . Prog.unProgression

genOffline :: Int -> Double -> IO PC.ProgressionContext
genOffline n e =
  seek "none" $ cue (H.initCadenceState 0 "C" [0, 4, 7]) $ len n $ entropy e genE

spec :: Spec
spec = describe "genE (polytonal, offline enumeration tier)" $ do

  -- Union-5 does not by itself force triadic partners: a 4-PC partner
  -- sharing two tones with T also contributes two new ones. The layer
  -- cardinality is its own contract, so it gets its own pin.
  it "every layer is triadic in every bar" $ do
    s <- genOffline 8 0.4
    let card = map (popCount . absMask)
    card (barsOf (PC.triadLayer  s)) `shouldSatisfy` all (== 3)
    card (barsOf (PC.strataLayer s)) `shouldSatisfy` all (== 3)
    card (barsOf (PC.modeLayer   s)) `shouldSatisfy` all (== 3)

  it "every bar satisfies the overlap algebra: partners share 2 with T, union is 5" $ do
    s <- genOffline 8 0.4
    let ts = map absMask (barsOf (PC.triadLayer s))
        ss = map absMask (barsOf (PC.strataLayer s))
        ms = map absMask (barsOf (PC.modeLayer s))
        ok (t, sm, mm) =
          popCount (t .&. sm) == 2 && popCount (t .&. mm) == 2
            && popCount (t .|. sm .|. mm) == 5
    all ok (zip3 ts ss ms) `shouldBe` True

  it "geometry per bar is common-dyad or base-anchored, never hub-tone" $ do
    s <- genOffline 8 0.4
    let triple (t, sm, mm) = popCount (t .&. sm .&. mm)
        layersOf = zip3 (map absMask (barsOf (PC.triadLayer s)))
                        (map absMask (barsOf (PC.strataLayer s)))
                        (map absMask (barsOf (PC.modeLayer s)))
    all (\b -> triple b `elem` [1, 2]) layersOf `shouldBe` True

  it "stamps FPoly, keeps layer lengths equal, keeps the requested length" $ do
    s <- genOffline 6 0.3
    PC.pcFamily s `shouldBe` PC.FPoly
    PC.pcProvenance s `shouldBe` Nothing
    Prog.progLength (PC.triadLayer s)  `shouldBe` 6
    Prog.progLength (PC.strataLayer s) `shouldBe` 6
    Prog.progLength (PC.modeLayer s)   `shouldBe` 6

  it "orders S below M by whole-layer dissonance total" $ do
    s <- genOffline 8 0.4
    let total lyr = sum [ dissonanceScore
                            (map P.unPitchClass (H.cadenceIntervals (H.stateCadence cs)))
                        | cs <- barsOf (lyr s) ]
    total PC.strataLayer <= total PC.modeLayer `shouldBe` True

  it "keeps the zero-form invariant on every partner bar" $ do
    s <- genOffline 8 0.4
    let zfOK cs = case map P.unPitchClass (H.cadenceIntervals (H.stateCadence cs)) of
          []        -> False
          ivs@(i:_) -> i == 0 && ivs == sort (nub ivs) && length ivs == 3
    all zfOK (barsOf (PC.strataLayer s) ++ barsOf (PC.modeLayer s)) `shouldBe` True

  it "every layer selector on FPoly routes to the user VoiceFunction (no chroma engine)" $ do
    s <- genOffline 4 0.3
    let sels = [PC.T, PC.S, PC.M, PC.TS, PC.TM, PC.SM, PC.TSM, PC.PT]
        routes = [ fst (layerForVoicing sel s) | sel <- sels ]
    routes `shouldBe` replicate 8 False
    -- and the projected cardinalities match the polytonal contract
    let cardOf sel = map (length . H.cadenceIntervals . H.stateCadence)
                         (barsOf (PC.layer sel s))
    cardOf PC.TS  `shouldBe` replicate 4 4
    cardOf PC.TM  `shouldBe` replicate 4 4
    cardOf PC.TSM `shouldBe` replicate 4 5
    mapM_ (\c -> c `shouldSatisfy` (`elem` [1, 2])) (cardOf PC.PT)

  it "polyLayerViews labels all eight views for FPoly, Nothing otherwise" $ do
    s <- genOffline 4 0.3
    case polyLayerViews s of
      Just views -> map fst views `shouldBe`
        [ "T (foundation)", "S (partner)", "M (partner)"
        , "TS", "TM", "SM", "TSM (pentad)", "PT (pivot tones)" ]
      Nothing -> expectationFailure "expected Just views for an FPoly context"
    let plain = PC.fromProgression $ Prog.fromCadenceStates
          [ H.initCadenceState 0 "C" [0, 4, 7] ]
    polyLayerViews plain `shouldSatisfy` (== Nothing)

  it "genFrom regenerates a range in place: kept bars byte-identical, invariants hold" $ do
    src <- genOffline 6 0.3
    out <- seek "none" $ genFrom src 3 4
    PC.pcFamily out `shouldBe` PC.FPoly
    PC.pcLength out `shouldBe` 6
    let keptIdx = [0, 1, 4, 5]
        barsAt lyr pc = [ barsOf (lyr pc) !! i | i <- keptIdx ]
        sameBars lyr = map absMask (barsAt lyr out) `shouldBe`
                       map absMask (barsAt lyr src)
    sameBars PC.triadLayer
    sameBars PC.strataLayer
    sameBars PC.modeLayer
    -- The regenerated bars still satisfy the overlap algebra.
    let ok i = let t = absMask (barsOf (PC.triadLayer out) !! i)
                   sm = absMask (barsOf (PC.strataLayer out) !! i)
                   mm = absMask (barsOf (PC.modeLayer out) !! i)
               in popCount (t .&. sm) == 2 && popCount (t .&. mm) == 2
                    && popCount (t .|. sm .|. mm) == 5
    all ok [2, 3] `shouldBe` True

  it "refuses a non-triadic cue" $ do
    let bad = H.mkCadenceStatePCs P.C H.Unison [0, 3, 7, 10]
    r <- try (seek "none" (cue bad (len 4 genE)) >>= evaluate . PC.pcLength)
          :: IO (Either SomeException Int)
    case r of
      Left _  -> pure ()
      Right _ -> expectationFailure "expected genE to refuse a 4-note cue"

  it "keeps partner bars inside a constraining harmonic space (tonal palette)" $ do
    -- C major palette via pinned single pitches; the cue triad is inside it.
    let ctx = hcOvertones "C' D' E' F' G' A' B'" hContext
    s <- seek "none" $ tonal ctx
           $ cue (H.initCadenceState 0 "C" [0, 4, 7]) $ len 6 $ entropy 0.4 genE
    let palette = [0, 2, 4, 5, 7, 9, 11]
        inPalette cs = all (`elem` palette)
          [ p | let r = P.unPitchClass (P.pitchClass (H.stateCadenceRoot cs))
              , iv <- H.cadenceIntervals (H.stateCadence cs)
              , let p = (r + P.unPitchClass iv) `mod` 12 ]
    all inPalette (barsOf (PC.strataLayer s) ++ barsOf (PC.modeLayer s))
      `shouldBe` True
