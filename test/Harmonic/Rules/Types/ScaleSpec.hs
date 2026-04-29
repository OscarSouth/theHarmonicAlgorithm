-- |Tests for the octatripentatonic vocabulary (strata, tristrata, modes).

module Harmonic.Rules.Types.ScaleSpec (spec) where

import Test.Hspec
import Data.List (nub, sort, (\\))
import Data.Maybe (isJust, mapMaybe)

import Harmonic.Rules.Types.Pitch (PitchClass(..), unPitchClass)
import Harmonic.Rules.Types.Scale

-- |The canonical octatripentatonic eight-tone union (E-minor base).
eightToneUnion :: [Int]
eightToneUnion = [1, 2, 4, 6, 7, 9, 10, 11]

-- |The four pitch classes omitted by every tristrata.
fourToneOmission :: [Int]
fourToneOmission = [0, 3, 5, 8]

spec :: Spec
spec = do
  describe "strataChroma" $ do
    it "returns 5 unique pitch classes for every label" $
      all (\s -> length (nub (strataChroma s)) == 5) allStrataLabels
        `shouldBe` True

    it "matches the E-minor base prime-form table" $ do
      map unPitchClass (strataChroma I)    `shouldBe` [2, 4, 6, 7, 11]
      map unPitchClass (strataChroma II)   `shouldBe` [1, 2, 4, 6, 9]
      map unPitchClass (strataChroma III)  `shouldBe` [1, 2, 4, 6, 11]
      map unPitchClass (strataChroma IV)   `shouldBe` [2, 4, 6, 7, 9]
      map unPitchClass (strataChroma V)    `shouldBe` [1, 2, 4, 7, 9]
      map unPitchClass (strataChroma VI)   `shouldBe` [1, 2, 4, 7, 11]
      map unPitchClass (strataChroma VII)  `shouldBe` [2, 4, 6, 7, 10]
      map unPitchClass (strataChroma VIII) `shouldBe` [1, 2, 6, 7, 9]
      map unPitchClass (strataChroma IX)   `shouldBe` [1, 2, 6, 7, 11]
      map unPitchClass (strataChroma X)    `shouldBe` [1, 2, 6, 7, 10]
      map unPitchClass (strataChroma XI)   `shouldBe` [1, 4, 6, 7, 10]

  describe "strataDissonance" $ do
    it "matches the spec table" $ do
      strataDissonance I    `shouldBe` 47
      strataDissonance II   `shouldBe` 46
      strataDissonance III  `shouldBe` 53
      strataDissonance IV   `shouldBe` 52
      strataDissonance V    `shouldBe` 68
      strataDissonance VI   `shouldBe` 72
      strataDissonance VII  `shouldBe` 71
      strataDissonance VIII `shouldBe` 74
      strataDissonance IX   `shouldBe` 75
      strataDissonance X    `shouldBe` 72
      strataDissonance XI   `shouldBe` 91

  describe "validTristrata" $ do
    it "has exactly 12 entries" $
      length validTristrata `shouldBe` 12

    it "has expected strata composition (records 1..12)" $ do
      (ts1 (tristrataIndex 1),  ts2 (tristrataIndex 1),  ts3 (tristrataIndex 1))
        `shouldBe` (I, V, X)
      (ts1 (tristrataIndex 2),  ts2 (tristrataIndex 2),  ts3 (tristrataIndex 2))
        `shouldBe` (II, VI, X)
      (ts1 (tristrataIndex 9),  ts2 (tristrataIndex 9),  ts3 (tristrataIndex 9))
        `shouldBe` (V, VII, IX)
      (ts1 (tristrataIndex 12), ts2 (tristrataIndex 12), ts3 (tristrataIndex 12))
        `shouldBe` (VI, VIII, XI)

    it "every tristrata unions to the 8-tone canonical set" $ do
      let unionChroma (Tristrata a b c) =
            sort . nub . map unPitchClass $
              strataChroma a ++ strataChroma b ++ strataChroma c
      mapM_ (\t -> unionChroma t `shouldBe` eightToneUnion) validTristrata

    it "every tristrata omits the four complement pitches" $ do
      let unionChroma (Tristrata a b c) =
            sort . nub . map unPitchClass $
              strataChroma a ++ strataChroma b ++ strataChroma c
      mapM_
        (\t -> ([0..11] \\ unionChroma t) `shouldBe` fourToneOmission)
        validTristrata

    it "tristrataDissonance matches expected row sums" $ do
      tristrataDissonance (tristrataIndex 1)  `shouldBe` 187
      tristrataDissonance (tristrataIndex 2)  `shouldBe` 190
      tristrataDissonance (tristrataIndex 3)  `shouldBe` 192
      tristrataDissonance (tristrataIndex 4)  `shouldBe` 193
      tristrataDissonance (tristrataIndex 5)  `shouldBe` 196
      tristrataDissonance (tristrataIndex 6)  `shouldBe` 206
      tristrataDissonance (tristrataIndex 7)  `shouldBe` 209
      tristrataDissonance (tristrataIndex 8)  `shouldBe` 212
      tristrataDissonance (tristrataIndex 9)  `shouldBe` 214
      tristrataDissonance (tristrataIndex 10) `shouldBe` 215
      tristrataDissonance (tristrataIndex 11) `shouldBe` 234
      tristrataDissonance (tristrataIndex 12) `shouldBe` 237

  describe "tristrataOf" $ do
    it "returns the expected memberships for VI" $
      tristrataOf VI `shouldBe`
        [ (tristrataIndex 2,  2)
        , (tristrataIndex 5,  2)
        , (tristrataIndex 7,  2)
        , (tristrataIndex 10, 2)
        , (tristrataIndex 12, 1)
        ]

    it "returns the expected memberships for V" $
      tristrataOf V `shouldBe`
        [ (tristrataIndex 1,  2)
        , (tristrataIndex 3,  2)
        , (tristrataIndex 4,  2)
        , (tristrataIndex 6,  2)
        , (tristrataIndex 8,  2)
        , (tristrataIndex 9,  1)
        , (tristrataIndex 11, 1)
        ]

    it "returns singleton for VIII" $
      tristrataOf VIII `shouldBe` [(tristrataIndex 12, 2)]

    it "every strata is a member of at least one tristrata" $
      all (\s -> not (null (tristrataOf s))) allStrataLabels `shouldBe` True

  describe "tristrataModes" $ do
    it "classifies every pair-union of every tristrata" $ do
      let classified (a, b, c) = isJust a && isJust b && isJust c
      mapM_
        (\t -> classified (tristrataModes t) `shouldBe` True)
        validTristrata

  describe "classifyMode" $ do
    -- A 7-note diatonic set is ambiguous without a designated root; classifyMode
    -- returns the first valid (root, quality) pairing it finds. We verify that
    -- the returned mode regenerates the input chroma.
    it "classifies the C-major collection as some valid mode of the same chroma" $ do
      let pcs = map P [0, 2, 4, 5, 7, 9, 11]
      case classifyMode pcs of
        Just m  -> modeChroma m `shouldBe` pcs
        Nothing -> expectationFailure "expected a mode classification"

    it "classifies unambiguous non-diatonic 7-sets by their distinguishing interval" $
      fmap modeQuality (classifyMode (map P [0, 2, 4, 6, 7, 9, 11]))
        `shouldBe` Just Lydian

    it "returns Nothing for non-7-note sets" $
      classifyMode (map P [0, 2, 4]) `shouldBe` Nothing

  describe "parsers" $ do
    it "parseTristrataList accepts bare and bracketed forms" $ do
      parseTristrataList ""              `shouldBe` []
      parseTristrataList "5"             `shouldBe` [5]
      parseTristrataList "1 2 5"         `shouldBe` [1, 2, 5]
      parseTristrataList "[1 2 5]"       `shouldBe` [1, 2, 5]
      parseTristrataList "[1, 2, 5]"     `shouldBe` [1, 2, 5]

    it "parseTristrataList rejects out-of-range indices" $ do
      parseTristrataList "0 13 99"       `shouldBe` []
      parseTristrataList "1 13 5"        `shouldBe` [1, 5]

    it "parseRelStrata restricts to positions 1..3" $ do
      parseRelStrata "1 2 3 1"           `shouldBe` [1, 2, 3, 1]
      parseRelStrata "[1 1 2 2 3 3]"     `shouldBe` [1, 1, 2, 2, 3, 3]
      parseRelStrata "4 0"               `shouldBe` []

    it "parseAbsStrata accepts Roman numerals" $ do
      parseAbsStrata "I V X"             `shouldBe` [I, V, X]
      parseAbsStrata "[I V X]"           `shouldBe` [I, V, X]
      parseAbsStrata "i v x"             `shouldBe` [I, V, X]
      parseAbsStrata "III VIII XI"       `shouldBe` [III, VIII, XI]

  describe "pentaFromChroma" $ do
    it "round-trips MajorPenta at C" $
      fmap (\p -> (pentaFamily p, unPitchClass (pentaRoot p)))
           (pentaFromChroma (map P [0, 2, 4, 7, 9]))
        `shouldBe` Just (MajorPenta, 0)

    it "round-trips Iwato at D" $
      fmap (\p -> (pentaFamily p, unPitchClass (pentaRoot p)))
           (pentaFromChroma (pentaChroma (Pentatonic Iwato (P 2))))
        `shouldBe` Just (Iwato, 2)

  describe "classifyModeAt (root-pinned classifier)" $ do
    it "identifies F# Phrygian over the II∪VI union" $
      -- union II∪VI = {C# D E F# G A B} = [1,2,4,6,7,9,11]; pin to PC 6 (F#)
      classifyModeAt 6 (map P [1,2,4,6,7,9,11])
        `shouldBe` Just (Mode Phrygian (P 6))

    it "identifies A Mixolydian over the same union" $
      classifyModeAt 9 (map P [1,2,4,6,7,9,11])
        `shouldBe` Just (Mode Mixolydian (P 9))

    it "identifies D Ionian over the same union" $
      classifyModeAt 2 (map P [1,2,4,6,7,9,11])
        `shouldBe` Just (Mode Ionian (P 2))

    it "returns Nothing for a non-7-PC set" $
      classifyModeAt 0 (map P [0,4,7]) `shouldBe` Nothing

    it "returns Nothing when the set isn't a valid mode at the pinned root" $
      -- Whole-tone hexatonic [0,2,4,6,8,10,1] — 7 PCs but not a 28-mode pattern
      -- pinned to 0
      classifyModeAt 0 (map P [0,2,4,6,8,10,1]) `shouldBe` Nothing

  describe "modeFamily (28-mode partition)" $ do
    it "Aeolian → Major" $        modeFamily Aeolian   `shouldBe` Major
    it "Locrian → Major" $        modeFamily Locrian   `shouldBe` Major
    it "MelMin → MelodicMinor" $  modeFamily MelMin    `shouldBe` MelodicMinor
    it "AltDom → MelodicMinor" $  modeFamily AltDom    `shouldBe` MelodicMinor
    it "HarmMin → HarmonicMinor"$ modeFamily HarmMin   `shouldBe` HarmonicMinor
    it "AltBb7 → HarmonicMinor" $ modeFamily AltBb7    `shouldBe` HarmonicMinor
    it "HarmMaj → HarmonicMajor"$ modeFamily HarmMaj   `shouldBe` HarmonicMajor
    it "LocBb7 → HarmonicMajor" $ modeFamily LocBb7    `shouldBe` HarmonicMajor

  describe "parentKey" $ do
    it "C# Aeolian → E Major" $
      parentKey (Mode Aeolian (P 1)) `shouldBe` (P 4, Major)

    it "A Mixolydian → D Major" $
      parentKey (Mode Mixolydian (P 9)) `shouldBe` (P 2, Major)

    it "F# Phrygian → D Major" $
      parentKey (Mode Phrygian (P 6)) `shouldBe` (P 2, Major)

    it "D Ionian → D Major (no shift for Ionian)" $
      parentKey (Mode Ionian (P 2)) `shouldBe` (P 2, Major)

    it "C HarmMin → C Harmonic Minor (no shift)" $
      parentKey (Mode HarmMin (P 0)) `shouldBe` (P 0, HarmonicMinor)

    it "C HarmMaj → C Harmonic Major (no shift)" $
      parentKey (Mode HarmMaj (P 0)) `shouldBe` (P 0, HarmonicMajor)

  describe "showScaleFamily" $ do
    it "renders names with whitespace where needed" $ do
      showScaleFamily Major          `shouldBe` "Major"
      showScaleFamily MelodicMinor   `shouldBe` "Melodic Minor"
      showScaleFamily HarmonicMinor  `shouldBe` "Harmonic Minor"
      showScaleFamily HarmonicMajor  `shouldBe` "Harmonic Major"
