-- |
-- Module      : Harmonic.Rules.Import.JazzSpec
-- Description : Bunks corpus chord-symbol parser tests
--
-- Validates the quality table against the corpus census (every quality
-- string observed in the 2,614-tune corpus must resolve, and the table
-- must contain nothing else), the notation-honouring parse semantics
-- (root, slash bass, anchor, enharmonic fold), and refusal recording.

module Harmonic.Rules.Import.JazzSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Data.List (nub, sort)
import qualified Data.Text as T
import Harmonic.Rules.Import.Jazz
import Harmonic.Rules.Types.Pitch (PitchClass(..))
import Harmonic.Rules.Types.Harmony (Movement(..))

-- Every quality string in the corpus (census 2026-08-25: 134,355 tokens,
-- 123 unique qualities of which NC is token-level, leaving 122).
censusQualities :: [T.Text]
censusQualities =
  [ "7", "m7", "", "M7", "m", "6", "m7b5", "7b9", "o7", "9", "m6", "7#9"
  , "7alt", "7+", "13", "m9", "7#11", "7sus4", "7#5", "M7#11", "69", "7b5"
  , "+", "m11", "M9", "mM7", "9sus4", "o", "7#5#9", "maj7", "7#5b9", "7sus"
  , "9#11", "sus4", "M7b5", "13b9", "dim", "m69", "add9", "m+", "M7#5", "11"
  , "13#11", "7b9#11", "9#5", "13sus4", "M6", "9b5", "9+", "7b9sus4", "maj9"
  , "5", "mMaj7", "2", "7sus4b9", "9sus", "madd9", "13#9", "sus", "m13"
  , "7susb9", "m#5", "7b9b13", "13b9#11", "7#9#11", "13sus", "m9b5", "M"
  , "sus24", "7b5#9", "m7b9", "M69", "13b5", "7b13", "M9#5", "maj13"
  , "m7add11", "dim7", "m7add4", "sus2", "oM7", "m7#5", "M13#11", "7#9b13"
  , "mM9", "mb6", "M#5add9", "addb9", "7add6", "+7", "maj7#5", "M9#11"
  , "M7add13", "7b6", "Mb5", "mb5", "maj9#11", "M69#11", "7#5b9#11", "67"
  , "6#11", "mM7b6", "M7+", "add9no3", "4", "+add9", "madd4", "M7#9b5"
  , "7add13", "6b5", "susb9", "o7M7", "mi", "M7#9#11", "M13", "m11b5"
  , "9b13", "7sus4b9b13", "7b9b5", "+add#9", "7b5b9", "h7"
  ]

parsed :: T.Text -> JazzChord
parsed t = case parseToken t of
  Right (Sounding c) -> c
  other              -> error ("expected chord for " ++ T.unpack t ++ ": " ++ show other)

spec :: Spec
spec = do
  describe "qualityIntervals" $ do
    it "covers the corpus census exactly (both directions)" $
      sort qualityNames `shouldBe` sort censusQualities

    it "every set is rooted, sorted, deduplicated, in range" $
      mapM_ (\(q, ivs) -> do
                (q, take 1 ivs) `shouldBe` (q, [0])
                (q, ivs) `shouldBe` (q, sort (nub ivs))
                (q, filter (\i -> i < 0 || i > 11) ivs) `shouldBe` (q, []))
            (Map.toList qualityIntervals)

    it "unifies spelling variants to identical sets" $ do
      let same a b = Map.lookup a qualityIntervals
                       `shouldBe` Map.lookup b qualityIntervals
      same "h7" "m7b5"
      same "dim" "o"
      same "dim7" "o7"
      same "maj7" "M7"
      same "mMaj7" "mM7"
      same "7+" "7#5"
      same "+7" "7#5"
      same "mi" "m"
      same "sus" "sus4"
      same "4" "sus4"
      same "2" "sus2"
      same "9sus" "9sus4"
      same "7alt" "7#5b9"
      same "m#5" "m+"

  describe "parseToken" $ do
    it "parses a bare major triad" $ do
      let c = parsed "C"
      jcRoot c `shouldBe` P 0
      jcBass c `shouldBe` Nothing
      jcAnchor c `shouldBe` P 0
      jcTones c `shouldBe` [P 0, P 4, P 7]

    it "honours the slash bass as anchor and unions it into the set" $ do
      let c = parsed "Dm7/G"
      jcRoot c `shouldBe` P 2
      jcBass c `shouldBe` Just (P 7)
      jcAnchor c `shouldBe` P 7
      jcTones c `shouldBe` [P 0, P 2, P 5, P 7, P 9]

    it "a slash bass already in the chord adds nothing to the set" $ do
      let c = parsed "C/E"
      jcAnchor c `shouldBe` P 4
      jcTones c `shouldBe` [P 0, P 4, P 7]

    it "folds enharmonic roots by arithmetic" $ do
      jcRoot (parsed "Cb") `shouldBe` P 11
      jcRoot (parsed "B#") `shouldBe` P 0
      jcRoot (parsed "E#7") `shouldBe` P 5
      jcRoot (parsed "Fb") `shouldBe` P 4
      jcRoot (parsed "A#m7") `shouldBe` P 10

    it "reads NC as NoChord" $
      parseToken "NC" `shouldBe` Right NoChord

    it "parses a deep tail symbol" $ do
      let c = parsed "Eb7b9#11"
      jcRoot c `shouldBe` P 3
      jcQuality c `shouldBe` "7b9#11"
      jcTones c `shouldBe` map P (sort (map (\i -> (3 + i) `mod` 12) [0,1,4,6,7,10]))

    it "refuses an unknown root" $
      parseToken "H7" `shouldBe`
        Left (JazzRefusal "H7" "no root note")

    it "refuses an unknown quality" $
      parseToken "Cfoo" `shouldBe`
        Left (JazzRefusal "Cfoo" "unknown quality: foo")

    it "refuses a malformed slash bass" $ do
      parseToken "C7/H" `shouldBe`
        Left (JazzRefusal "C7/H" "malformed slash bass")
      parseToken "C7/Eb7" `shouldBe`
        Left (JazzRefusal "C7/Eb7" "malformed slash bass")

  describe "jazzFunctionality" $ do
    it "names direct table sets with the corpus-preferred spelling" $ do
      jazzFunctionality [0,4,7] `shouldBe` Just "maj"
      jazzFunctionality [0,3,7,10] `shouldBe` Just "m7"
      jazzFunctionality [0,3,6,10] `shouldBe` Just "m7b5"   -- beats h7
      jazzFunctionality [0,4,8,10] `shouldBe` Just "7+"     -- corpus prefers 7+ over 7#5
      jazzFunctionality [0,1,4,8,10] `shouldBe` Just "7alt" -- beats 7#5b9
      jazzFunctionality [0,2,5,7,10] `shouldBe` Just "9sus4" -- beats 11, 9sus

    it "names slash shapes by rotation: quality over bass degree" $ do
      jazzFunctionality [0,5,9] `shouldBe` Just "maj/5"     -- G/D
      jazzFunctionality [0,2,5,9] `shouldBe` Just "m7/b7"   -- Cm7/Bb
      jazzFunctionality [0,3,5,9] `shouldBe` Just "7/5"     -- A7/E
      jazzFunctionality [0,3,6,8] `shouldBe` Just "7/3"     -- G7/B
      jazzFunctionality [0,1,5,8] `shouldBe` Just "maj/7"   -- C/B
      jazzFunctionality [0,1,5,10] `shouldBe` Just "m/2"    -- Am/B

    it "refuses non-zero-form or alien sets" $ do
      jazzFunctionality [1,2,3] `shouldBe` Nothing
      jazzFunctionality [0,1,2] `shouldBe` Nothing
      jazzFunctionality [] `shouldBe` Nothing

  describe "beatSlots" $ do
    it "shares bar beats front-loaded" $ do
      beatSlots (4,4) 1 `shouldBe` [4]
      beatSlots (4,4) 2 `shouldBe` [2,2]
      beatSlots (4,4) 3 `shouldBe` [2,1,1]
      beatSlots (4,4) 4 `shouldBe` [1,1,1,1]
      beatSlots (3,4) 2 `shouldBe` [2,1]
    it "gives every chord one slot when harmony is sub-beat" $ do
      beatSlots (4,4) 8 `shouldBe` replicate 8 1
      beatSlots (3,4) 6 `shouldBe` replicate 6 1
    it "reads compound x/8 meters as numerator-div-3 beats" $ do
      beatSlots (6,8) 1 `shouldBe` [2]
      beatSlots (12,8) 2 `shouldBe` [2,2]

  describe "parseSong / songCadences" $ do
    let songText = T.unlines
          [ "Title = Test Tune"
          , "ComposedBy = Nobody"
          , "DBKeySig = C"
          , "TimeSig = 4 4"
          , "Bars = 2"
          , " Dm7 G7 | CM7 | NC | Am7 |"
          ]
        song = either (error . show) id (parseSong "TestFile" songText)

    it "reads headers and bars" $ do
      jsTitle song `shouldBe` "Test Tune"
      jsComposer song `shouldBe` "Nobody"
      jsTimeSig song `shouldBe` (4,4)
      length (jsBars song) `shouldBe` 4

    it "expands beats, sustains as self-slots, NC vanishing (bridge)" $
      -- Dm7 Dm7 G7 G7 | CM7 x4 | (NC dropped) | Am7 x4 = 12 slots
      length (beatStream song) `shouldBe` 12

    it "derives the cadence chain with pedal self-cadences and bridged NC" $ do
      let cs = songCadences song
      length cs `shouldBe` 11
      -- first step: Dm7 sustained -> pedal m7
      head cs `shouldBe` JazzCadence Unison [0,3,7,10] "m7"
      -- Dm7 -> G7: anchor D to G
      map jzName (filter ((/= Unison) . jzMovement) cs)
        `shouldBe` ["7", "M7", "m7"]
      -- the NC bar bridges: CM7 (anchor 0) -> Am7 (anchor 9) directly
      jzMovement (cs !! 6) `shouldBe` Unison  -- last CM7 sustain
      (jzMovement (cs !! 7), jzName (cs !! 7)) `shouldBe` (Desc (P 3), "m7")

    it "falls back to the given name for a corrupt Title header" $ do
      let bad = T.unlines
            [ "mpTitle = Bebop", "ComposedBy = X", "DBKeySig = C"
            , "TimeSig = 4 4", "Bars = 1", " C7 |" ]
      jsTitle (either (error . show) id (parseSong "Bebop" bad))
        `shouldBe` "Bebop"

    it "refuses a song with a missing TimeSig" $ do
      let bad = T.unlines ["Title = X", "ComposedBy = Y", "DBKeySig = C"
                          , "Bars = 1", "", " C7 |"]
      parseSong "X" bad `shouldBe`
        Left (JazzRefusal "X" "missing or malformed TimeSig header")

  describe "jazzShow" $
    it "mirrors the classical key shape" $
      jazzShow (JazzCadence Unison [0,3,7,10] "m7")
        `shouldBe` "( pedal -> m7 )"

  describe "normalizeComposer" $
    it "lower-cases, strips non-alphanumerics, maps empty to unknown" $ do
      normalizeComposer "Richard Rodgers" `shouldBe` "richardrodgers"
      normalizeComposer "J.S. Bach!" `shouldBe` "jsbach"
      normalizeComposer "" `shouldBe` "unknown"
      normalizeComposer "---" `shouldBe` "unknown"

  describe "buildChangeEdges" $
    it "merges weights across songs, never bridges song boundaries" $ do
      let mk composer barLine = either (error . show) id $ parseSong "t" $ T.unlines
            [ "Title = T", "ComposedBy = " <> composer, "DBKeySig = C"
            , "TimeSig = 4 4", "Bars = 2", barLine ]
          s1 = mk "Alpha" " Dm7 G7 | CM7 Am7 |"
          s2 = mk "Beta"  " Dm7 G7 | FM7 Bb7 |"
          edges = buildChangeEdges [s1, s2]
          totalWeight = sum (concatMap (Map.elems . snd) edges)
      -- each song: 8 slots -> 7 cadences -> 6 edges
      totalWeight `shouldBe` 12
      -- the shared opening (Dm7 sustain -> Dm7->G7 step) merges composers
      let shared = [ w | ((a, b), w) <- edges
                       , jzName a == "m7", jzMovement b /= Unison, jzName b == "7" ]
      map (Map.keys) shared `shouldBe` [["alpha", "beta"]]
