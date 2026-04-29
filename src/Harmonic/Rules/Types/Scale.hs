-- |
-- Module      : Harmonic.Rules.Types.Scale
-- Description : Pentatonic, modal, and octatripentatonic vocabulary
--
-- Vocabulary for the octatripentatonic framework: pentatonic families,
-- the 28-mode taxonomy, the eleven canonical strata, and the twelve
-- curated tristrata. E-minor-base chroma throughout.

module Harmonic.Rules.Types.Scale
  ( -- * Pentatonic families
    PentaFamily(..)
  , familyChroma
  , Pentatonic(..)
  , pentaChroma
  , pentaFromChroma

    -- * Mode taxonomy
  , ModeQuality(..)
  , Mode(..)
  , modeChroma
  , classifyMode
  , classifyModeAt
  , ScaleFamily(..)
  , modeFamily
  , modeDegree
  , parentKey
  , showScaleFamily
  , showModeQuality
  , ModeResult(..)

    -- * Strata
  , StrataLabel(..)
  , strataChroma
  , strataDissonance
  , allStrataLabels

    -- * Tristrata
  , Tristrata(..)
  , validTristrata
  , tristrataIndex
  , tristrataStrataAt
  , tristrataDissonance
  , tristrataOf
  , tristrataModes

    -- * Parsers
  , parseTristrataList
  , parseRelStrata
  , parseAbsStrata
  ) where

import Data.List (nub, sort, find)
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)
import Data.Char (toUpper)

import Harmonic.Rules.Types.Pitch (PitchClass(..), mkPitchClass, unPitchClass)

-------------------------------------------------------------------------------
-- Pentatonic families
-------------------------------------------------------------------------------

-- |Pentatonic family: the four ancestral pentatonic shapes recognised
-- throughout the legacy theory (major, Okinawan, Iwato, Kumoi).
data PentaFamily = MajorPenta | Okinawan | Iwato | Kumoi
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- |Chroma (pitch-class set) for a pentatonic family, rooted at 0.
familyChroma :: PentaFamily -> [PitchClass]
familyChroma MajorPenta = map P [0, 2, 4, 7, 9]
familyChroma Okinawan   = map P [0, 4, 5, 7, 11]
familyChroma Iwato      = map P [0, 1, 5, 6, 10]
familyChroma Kumoi      = map P [0, 2, 3, 7, 9]

-- |A pentatonic scale is a family + a root pitch class.
data Pentatonic = Pentatonic
  { pentaFamily :: PentaFamily
  , pentaRoot   :: PitchClass
  } deriving (Eq, Ord, Show)

-- |Concrete chroma of a rooted pentatonic.
pentaChroma :: Pentatonic -> [PitchClass]
pentaChroma (Pentatonic fam (P r)) =
  sort $ nub [ mkPitchClass (unPitchClass p + r) | p <- familyChroma fam ]

-- |Reverse lookup: given a 5-PC set, identify its family+root if any.
pentaFromChroma :: [PitchClass] -> Maybe Pentatonic
pentaFromChroma pcs
  | length uniq /= 5 = Nothing
  | otherwise = listToMaybe
      [ Pentatonic fam (P r)
      | fam <- [minBound .. maxBound]
      , r   <- [0 .. 11]
      , sort (map unPitchClass (pentaChroma (Pentatonic fam (P r))))
          == sort (map unPitchClass uniq)
      ]
  where
    uniq = nub pcs

-------------------------------------------------------------------------------
-- Mode taxonomy (28 modes)
-------------------------------------------------------------------------------

-- |28-mode taxonomy ported from the legacy @toMode@ function.
data ModeQuality
  -- Major modes (7)
  = Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian
  -- Melodic minor modes (7)
  | MelMin | DorB2 | LydS5 | LydDom | MixoB6 | LocNat2 | AltDom
  -- Harmonic minor modes (7)
  | HarmMin | LocNat6 | IonS5 | DorS4 | PhryNat3 | LydS2 | AltBb7
  -- Harmonic major modes (7)
  | HarmMaj | DorB5 | PhryB4 | LydB3 | MixoB2 | LydAugS2 | LocBb7
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- |Interval pattern (from the root) for each mode quality.
modeIntervals :: ModeQuality -> [Int]
-- Major
modeIntervals Ionian     = [0,2,4,5,7,9,11]
modeIntervals Dorian     = [0,2,3,5,7,9,10]
modeIntervals Phrygian   = [0,1,3,5,7,8,10]
modeIntervals Lydian     = [0,2,4,6,7,9,11]
modeIntervals Mixolydian = [0,2,4,5,7,9,10]
modeIntervals Aeolian    = [0,2,3,5,7,8,10]
modeIntervals Locrian    = [0,1,3,5,6,8,10]
-- Melodic minor
modeIntervals MelMin     = [0,2,3,5,7,9,11]
modeIntervals DorB2      = [0,1,3,5,7,9,10]
modeIntervals LydS5      = [0,2,4,6,8,9,11]
modeIntervals LydDom     = [0,2,4,6,7,9,10]
modeIntervals MixoB6     = [0,2,4,5,7,8,10]
modeIntervals LocNat2    = [0,2,3,5,6,8,10]
modeIntervals AltDom     = [0,1,3,4,6,8,10]
-- Harmonic minor
modeIntervals HarmMin    = [0,2,3,5,7,8,11]
modeIntervals LocNat6    = [0,1,3,5,6,9,10]
modeIntervals IonS5      = [0,2,4,5,8,9,11]
modeIntervals DorS4      = [0,2,3,6,7,9,10]
modeIntervals PhryNat3   = [0,1,4,5,7,8,10]
modeIntervals LydS2      = [0,3,4,6,7,9,11]
modeIntervals AltBb7     = [0,1,3,4,6,8,9]
-- Harmonic major
modeIntervals HarmMaj    = [0,2,4,5,7,8,11]
modeIntervals DorB5      = [0,2,3,5,6,9,10]
modeIntervals PhryB4     = [0,1,3,4,7,8,10]
modeIntervals LydB3      = [0,2,3,6,7,9,11]
modeIntervals MixoB2     = [0,1,4,5,7,9,10]
modeIntervals LydAugS2   = [0,3,4,6,8,9,11]
modeIntervals LocBb7     = [0,1,3,5,6,8,9]

allModeQualities :: [ModeQuality]
allModeQualities = [minBound .. maxBound]

-- |A mode is a quality rooted at a pitch class.
data Mode = Mode
  { modeQuality :: ModeQuality
  , modeRoot    :: PitchClass
  } deriving (Eq, Ord, Show)

-- |Concrete chroma of a rooted mode.
modeChroma :: Mode -> [PitchClass]
modeChroma (Mode q (P r)) =
  sort [ mkPitchClass (i + r) | i <- modeIntervals q ]

-- |Identify a 7-PC set as a mode by trying each element as root
-- and pattern-matching against the 28 templates.
classifyMode :: [PitchClass] -> Maybe Mode
classifyMode pcs
  | length uniq /= 7 = Nothing
  | otherwise = listToMaybe $ mapMaybe tryRoot uniq
  where
    uniq = nub $ sort $ map unPitchClass pcs
    tryRoot r =
      let shifted = sort [ (p - r) `mod` 12 | p <- uniq ]
      in fmap (\q -> Mode q (P r)) (find (\q -> modeIntervals q == shifted) allModeQualities)

-- |Pinned-root classifier: identify a 7-PC set as a mode rooted on the
-- given pitch class, exhaustively checking the 28 mode quality patterns.
-- Mirrors legacy 'toMode' semantics (root explicit, not inferred).
-- Returns 'Nothing' when the set isn't 7 unique PCs or when no quality
-- matches the shifted interval pattern.
classifyModeAt :: Int -> [PitchClass] -> Maybe Mode
classifyModeAt rootPC pcs
  | length uniq /= 7 = Nothing
  | otherwise =
      let shifted = sort [ (p - rootPC) `mod` 12 | p <- uniq ]
      in fmap (\q -> Mode q (P (rootPC `mod` 12)))
              (find (\q -> modeIntervals q == shifted) allModeQualities)
  where
    uniq = nub $ sort $ map unPitchClass pcs

-------------------------------------------------------------------------------
-- Scale families (parent-key partition of the 28-mode taxonomy)
-------------------------------------------------------------------------------

-- |Each of the 28 'ModeQuality' constructors belongs to exactly one of four
-- parent scale families.
data ScaleFamily = Major | MelodicMinor | HarmonicMinor | HarmonicMajor
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- |Map a 'ModeQuality' to its parent scale family.
modeFamily :: ModeQuality -> ScaleFamily
modeFamily q
  | q `elem` [Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian] = Major
  | q `elem` [MelMin, DorB2, LydS5, LydDom, MixoB6, LocNat2, AltDom]          = MelodicMinor
  | q `elem` [HarmMin, LocNat6, IonS5, DorS4, PhryNat3, LydS2, AltBb7]        = HarmonicMinor
  | otherwise                                                                  = HarmonicMajor

-- |Semitone offset of each mode's tonic from its parent scale's root.
-- E.g. Aeolian sits on the 6th degree of major → 9 semitones above the
-- parent root → @modeDegree Aeolian = 9@.
modeDegree :: ModeQuality -> Int
-- Major
modeDegree Ionian     = 0
modeDegree Dorian     = 2
modeDegree Phrygian   = 4
modeDegree Lydian     = 5
modeDegree Mixolydian = 7
modeDegree Aeolian    = 9
modeDegree Locrian    = 11
-- Melodic minor
modeDegree MelMin     = 0
modeDegree DorB2      = 2
modeDegree LydS5      = 3
modeDegree LydDom     = 5
modeDegree MixoB6     = 7
modeDegree LocNat2    = 9
modeDegree AltDom     = 11
-- Harmonic minor
modeDegree HarmMin    = 0
modeDegree LocNat6    = 2
modeDegree IonS5      = 3
modeDegree DorS4      = 5
modeDegree PhryNat3   = 7
modeDegree LydS2      = 8
modeDegree AltBb7     = 11
-- Harmonic major
modeDegree HarmMaj    = 0
modeDegree DorB5      = 2
modeDegree PhryB4     = 4
modeDegree LydB3      = 5
modeDegree MixoB2     = 7
modeDegree LydAugS2   = 8
modeDegree LocBb7     = 11

-- |Parent (root pitch class, family) for any mode. E.g.
-- @parentKey (Mode Aeolian (P 1)) == (P 4, Major)@ — C# Aeolian is a mode
-- of E Major.
parentKey :: Mode -> (PitchClass, ScaleFamily)
parentKey (Mode q (P r)) =
  (mkPitchClass ((r - modeDegree q) `mod` 12), modeFamily q)

-- |Render a scale family as a human-readable string.
showScaleFamily :: ScaleFamily -> String
showScaleFamily Major          = "Major"
showScaleFamily MelodicMinor   = "Melodic Minor"
showScaleFamily HarmonicMinor  = "Harmonic Minor"
showScaleFamily HarmonicMajor  = "Harmonic Major"

-- |Legacy @toMode@ mode-quality strings from
-- @theHarmonicAlgorithmLegacy/src/MusicData.hs@. Preferred over the
-- derived 'Show' (which yields compact constructor names like
-- "IonS5") for musician-facing diagnostic output.
showModeQuality :: ModeQuality -> String
-- Major modes
showModeQuality Ionian     = "Ionian"
showModeQuality Dorian     = "Dorian"
showModeQuality Phrygian   = "Phrygian"
showModeQuality Lydian     = "Lydian"
showModeQuality Mixolydian = "Mixolydian"
showModeQuality Aeolian    = "Aeolian"
showModeQuality Locrian    = "Locrian"
-- Melodic minor modes
showModeQuality MelMin     = "Mel_Min"
showModeQuality DorB2      = "Dor_b2"
showModeQuality LydS5      = "Lyd_#5"
showModeQuality LydDom     = "Lyd_Dom"
showModeQuality MixoB6     = "Mixo_b6"
showModeQuality LocNat2    = "Loc_nat.2"
showModeQuality AltDom     = "Alt_Dom"
-- Harmonic minor modes
showModeQuality HarmMin    = "Harm_Min"
showModeQuality LocNat6    = "Loc_nat.6"
showModeQuality IonS5      = "Ion_#5"
showModeQuality DorS4      = "Dor_#4"
showModeQuality PhryNat3   = "Phry_nat.3"
showModeQuality LydS2      = "Lyd_#2"
showModeQuality AltBb7     = "Alt_bb7"
-- Harmonic major modes
showModeQuality HarmMaj    = "Harm_Maj"
showModeQuality DorB5      = "Dor_b5"
showModeQuality PhryB4     = "Phry_b4"
showModeQuality LydB3      = "Lyd_b3"
showModeQuality MixoB2     = "Mixo_b2"
showModeQuality LydAugS2   = "Lyd_Aug_#2"
showModeQuality LocBb7     = "Loc_bb7"

-- |Result of a triad-anchored mode classification. 'ModeOk' carries a
-- normally classified mode; 'ModeInvalid' carries the offending
-- pair-union pitch classes when the union doesn't have exactly 7 unique
-- PCs (only reachable under 'absStrata' overrides that violate
-- tristrata adjacency).
data ModeResult
  = ModeOk Mode
  | ModeInvalid [PitchClass]
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Strata
-------------------------------------------------------------------------------

-- |The eleven canonical strata labels (Roman I–XI).
data StrataLabel
  = I | II | III | IV | V | VI | VII | VIII | IX | X | XI
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- |All strata labels in order.
allStrataLabels :: [StrataLabel]
allStrataLabels = [minBound .. maxBound]

-- |Chroma (E-minor base) of each strata.
strataChroma :: StrataLabel -> [PitchClass]
strataChroma I    = map P [2, 4, 6, 7, 11]
strataChroma II   = map P [1, 2, 4, 6, 9]
strataChroma III  = map P [1, 2, 4, 6, 11]
strataChroma IV   = map P [2, 4, 6, 7, 9]
strataChroma V    = map P [1, 2, 4, 7, 9]
strataChroma VI   = map P [1, 2, 4, 7, 11]
strataChroma VII  = map P [2, 4, 6, 7, 10]
strataChroma VIII = map P [1, 2, 6, 7, 9]
strataChroma IX   = map P [1, 2, 6, 7, 11]
strataChroma X    = map P [1, 2, 6, 7, 10]
strataChroma XI   = map P [1, 4, 6, 7, 10]

-- |Prime-form dissonance of each strata (from the octatripentatonic spec).
strataDissonance :: StrataLabel -> Int
strataDissonance I    = 47
strataDissonance II   = 46
strataDissonance III  = 53
strataDissonance IV   = 52
strataDissonance V    = 68
strataDissonance VI   = 72
strataDissonance VII  = 71
strataDissonance VIII = 74
strataDissonance IX   = 75
strataDissonance X    = 72
strataDissonance XI   = 91

-------------------------------------------------------------------------------
-- Tristrata
-------------------------------------------------------------------------------

-- |A tristrata is three strata whose pair-unions are diatonic 7-note sets
-- and whose three-way union is the canonical 8-note set @[1,2,4,6,7,9,10,11]@.
data Tristrata = Tristrata
  { ts1 :: StrataLabel
  , ts2 :: StrataLabel
  , ts3 :: StrataLabel
  } deriving (Eq, Ord, Show)

-- |Twelve canonical tristrata (records 1..12 of the octatripentatonic corpus).
validTristrata :: [Tristrata]
validTristrata =
  [ Tristrata I   V    X      --  1
  , Tristrata II  VI   X      --  2
  , Tristrata III V    VII    --  3
  , Tristrata III V    X      --  4
  , Tristrata IV  VI   X      --  5
  , Tristrata I   V    XI     --  6
  , Tristrata II  VI   XI     --  7
  , Tristrata III V    XI     --  8
  , Tristrata V   VII  IX     --  9
  , Tristrata IV  VI   XI     -- 10
  , Tristrata V   IX   XI     -- 11
  , Tristrata VI  VIII XI     -- 12
  ]

-- |Index into 'validTristrata' using a 1-based ordinal.
tristrataIndex :: Int -> Tristrata
tristrataIndex n
  | n < 1 || n > length validTristrata =
      error $ "tristrataIndex: out of range 1.." ++ show (length validTristrata) ++ ": " ++ show n
  | otherwise = validTristrata !! (n - 1)

-- |Project a tristrata to its strata at position 1, 2, or 3.
tristrataStrataAt :: Tristrata -> Int -> StrataLabel
tristrataStrataAt t 1 = ts1 t
tristrataStrataAt t 2 = ts2 t
tristrataStrataAt t 3 = ts3 t
tristrataStrataAt _ p = error $ "tristrataStrataAt: position must be 1..3, got " ++ show p

-- |Sum of prime-form dissonances of the tristrata's three strata.
tristrataDissonance :: Tristrata -> Int
tristrataDissonance (Tristrata a b c) =
  strataDissonance a + strataDissonance b + strataDissonance c

-- |All (tristrata, position) pairs that contain a given strata.
tristrataOf :: StrataLabel -> [(Tristrata, Int)]
tristrataOf s =
  [ (t, p)
  | t <- validTristrata
  , p <- [1, 2, 3]
  , tristrataStrataAt t p == s
  ]

-- |Classify the three pair-unions of a tristrata as diatonic modes.
-- Returns @(mode(ts1∪ts2), mode(ts1∪ts3), mode(ts2∪ts3))@.
tristrataModes :: Tristrata -> (Maybe Mode, Maybe Mode, Maybe Mode)
tristrataModes (Tristrata a b c) =
  let union2 x y = sort $ nub (strataChroma x ++ strataChroma y)
  in ( classifyMode (union2 a b)
     , classifyMode (union2 a c)
     , classifyMode (union2 b c)
     )

-------------------------------------------------------------------------------
-- Parsers (string surface for live-coding modifiers)
-------------------------------------------------------------------------------

-- |Strip surrounding brackets, commas, and whitespace from a list literal.
-- Accepts @"5"@, @"1 2 5"@, @"[1 2 5]"@, @"[1,2,5]"@.
stripListLit :: String -> String
stripListLit = map replaceComma . dropBrackets . trim
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
    dropBrackets ('[':rest) = reverse (dropBracketEnd (reverse rest))
    dropBrackets s          = s
    dropBracketEnd (']':r)  = r
    dropBracketEnd s        = s
    replaceComma ','        = ' '
    replaceComma c          = c

-- |Parse a tristrata allow-list string to a list of 1-based indices.
-- @""@ → empty (meaning "all allowed"); @"5"@ → @[5]@; @"1 2 5"@ → @[1,2,5]@.
parseTristrataList :: String -> [Int]
parseTristrataList s =
  let toks = words (stripListLit s)
  in [ n | t <- toks, Just n <- [readMaybeInt t], n >= 1, n <= length validTristrata ]

-- |Parse a per-bar position sequence (elements ∈ {1,2,3}).
parseRelStrata :: String -> [Int]
parseRelStrata s =
  let toks = words (stripListLit s)
  in [ n | t <- toks, Just n <- [readMaybeInt t], n >= 1, n <= 3 ]

-- |Parse a per-bar absolute strata sequence. Accepts @"I V X"@ or @"[I V X]"@.
parseAbsStrata :: String -> [StrataLabel]
parseAbsStrata s =
  let toks = words (stripListLit s)
  in mapMaybe readStrata toks
  where
    readStrata t = case map toUpper t of
      "I"    -> Just I
      "II"   -> Just II
      "III"  -> Just III
      "IV"   -> Just IV
      "V"    -> Just V
      "VI"   -> Just VI
      "VII"  -> Just VII
      "VIII" -> Just VIII
      "IX"   -> Just IX
      "X"    -> Just X
      "XI"   -> Just XI
      _      -> Nothing

-- |Parse an integer, returning Nothing on failure.
readMaybeInt :: String -> Maybe Int
readMaybeInt s = case reads s of
  [(n, "")] -> Just n
  _         -> Nothing
