{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

-- |
-- Module      : Harmonic.Rules.Types.Progression
-- Description : Progression type with Monoid structure and manipulation functions
-- 
-- This module defines 'Progression' as a sequence of 'CadenceState' values
-- with Monoid structure for composability.
--
-- DESIGN DECISION (from evaluation):
--   "Stick to the Strict interpretation for the data structure (Seq CadenceState).
--    Let the VoiceLeading module handle the cost calculation separately.
--    Don't over-complicate the data type itself."
--
-- Therefore:
--   * 'mempty' = empty progression
--   * '(<>)' = simple concatenation (seam smoothing is handled externally)
--
-- The manipulation functions (rotate, excerpt, insert, etc.) are ported
-- from legacy Arranger.hs to enable macro-level musical operations.

module Harmonic.Rules.Types.Progression
  ( -- * Core Type
    Progression(..)
  
    -- * Construction
  , singleton
  , fromCadenceStates
  , fromChordStates
  
    -- * Queries
  , progLength
  , progChords
  , progCadences
  , getCadenceState
  , getChordState
  
    -- * Manipulation (ported from legacy Arranger.hs)
  , rotateProgression
  , excerptProgression
  , insertProgression
  , fuseProgression
  , transposeProgression
  , overlapProgression
  , expandProgression
  
    -- * Voicing Extractors
  , literalVoicing
  , harmonyVoicing
  , closeVoicing
  , wideVoicing
  
    -- * Display Helpers
  , showTriad
  ) where

import GHC.Generics (Generic)
import Data.List (sort)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.List.Split (chunksOf)
import qualified Data.List as List
import qualified Data.Char as Char

import Harmonic.Rules.Types.Pitch (PitchClass(..), mkPitchClass, unPitchClass, transpose, NoteName(..), pitchClass, enharmFromNoteName)
-- |Import zeroFormPC for zero-form normalization in toCadenceStateFromPair to match DB convention
import Harmonic.Rules.Types.Harmony (Chord(..), Cadence(..), ChordState(..), CadenceState(..), fromCadenceState, Movement(..), fromMovement, EnharmonicSpelling(..), zeroFormPC, defaultEnharm)

-------------------------------------------------------------------------------
-- Progression Type
-------------------------------------------------------------------------------

-- |A Progression is a sequence of CadenceStates.
-- Using 'Seq' for O(1) access to both ends and O(log n) concatenation.
--
-- Note: This is the "strict interpretation" - the sequence holds concrete
-- states, and voice leading costs are computed externally when needed.
newtype Progression = Progression { unProgression :: Seq CadenceState }
  deriving (Eq, Generic)

-- |Visual Show instance for Progression (ported from legacy MusicData.hs).
-- Displays progressions in a 4-column grid with bar number labels.
-- Each chord is rendered using the enharmonic spelling stored in its CadenceState root.
-- The final grid shows consistent note names throughout the progression.
instance Show Progression where
  show (Progression seq) 
    | Seq.null seq = "[empty progression]"
    | otherwise = 
      let cadenceStates = toList seq
          -- Get enharmonic function from each cadence's root
          enharms = map (enharmFromNoteName . stateCadenceRoot) cadenceStates
          -- Build chords from cadence states
          chords = map fromCadenceState cadenceStates
          -- Show each triad using its root's enharmonic spelling
          showChords = zipWith showTriad enharms chords
          -- Pad each chord string to fixed width (14 chars) and add separator
          paddedChords = map ((++"|   ") . padTo 14) showChords
          -- Group into lines of 4 chords with bar labels (consistent 3-space indent)
          barLabels = ["\n   1   ||   ", "\n   5    |   ", "\n   9    |   ", "\n   13  |   ",
                       "\n   17  |   ", "\n   21  |   ", "\n   25  |   ", "\n   29  |   ",
                       "\n   33  |   ", "\n   37  |   ", "\n   41  |   ", "\n   45  |   ",
                       "\n   49  |   ", "\n   53  |   ", "\n   57  |   ", "\n   61  |   "]
          groupedChords = chunksOf 4 paddedChords
          -- Format each 4-bar group (remove trailing separator)
          formattedGroups = map (init . init . init . concat) groupedChords
          -- Combine with labels
          result = concat $ zipWith (++) barLabels formattedGroups
      in result ++ "|"
    where
      padTo n s = s ++ replicate (n - length s) ' '

-- |Monoid instance: empty progression as identity, concatenation as operation.
-- The "seam" between concatenated progressions is NOT automatically smoothed;
-- that responsibility lies with the VoiceLeading module.
instance Semigroup Progression where
  (<>) :: Progression -> Progression -> Progression
  (Progression a) <> (Progression b) = Progression (a >< b)

instance Monoid Progression where
  mempty :: Progression
  mempty = Progression Seq.empty

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- |Create a progression from a single CadenceState
singleton :: CadenceState -> Progression
singleton cs = Progression (Seq.singleton cs)

-- |Create a progression from a list of CadenceStates
fromCadenceStates :: [CadenceState] -> Progression
fromCadenceStates = Progression . Seq.fromList

-- |Create a progression from a list of ChordStates (inferring cadences)
-- Each consecutive pair forms a cadence.
fromChordStates :: [ChordState] -> Progression
fromChordStates [] = mempty
fromChordStates [_] = mempty  -- Need at least 2 chords for a cadence
fromChordStates states = 
  let pairs = zip states (tail states)
      cadenceStates = map toCadenceStateFromPair pairs
  in fromCadenceStates cadenceStates

-- |Helper: create CadenceState from a pair of ChordStates
-- When converting stored progressions, we apply defaultEnharm to the posterior root
-- since we don't have a prior CadenceState to guide enharmonic preference.
toCadenceStateFromPair :: (ChordState, ChordState) -> CadenceState
toCadenceStateFromPair (from, to) =
  let fromChord = stateChord from
      toChord = stateChord to
      -- Calculate movement
      fromRoot = pitchClass (stateRoot from)
      toRoot = pitchClass (stateRoot to)
      mvmt = calculateMovement fromRoot toRoot
      -- Build cadence with zero-form intervals
      cad = Cadence (chordFunctionality toChord) mvmt (zeroFormPC $ map mkPitchClass $ take 3 $ map fromIntegral (chordIntervals toChord))
      -- Apply default enharmonic preference (C→flat, others→natural)
      defaultSpelling = defaultEnharm toRoot
  in CadenceState cad (stateRoot to) defaultSpelling
  where
    calculateMovement :: PitchClass -> PitchClass -> Movement
    calculateMovement (P from) (P to) = 
      let diff = (to - from) `mod` 12
      in if diff <= 6 
         then if diff == 0 then Unison
              else if diff == 6 then Tritone
              else Asc (P diff)
         else Desc (P (12 - diff))

-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------

-- |Get the length of a progression
progLength :: Progression -> Int
progLength = Seq.length . unProgression

-- |Extract all chords from a progression
progChords :: Progression -> [Chord]
progChords (Progression seq) = map fromCadenceState (toList seq)

-- |Extract all cadences from a progression
progCadences :: Progression -> [Cadence]
progCadences (Progression seq) = map stateCadence (toList seq)

-- |Get a CadenceState at a specific index (1-indexed for user friendliness)
getCadenceState :: Progression -> Int -> Maybe CadenceState
getCadenceState (Progression seq) idx = Seq.lookup (idx - 1) seq

-- |Get a ChordState at a specific index (derived from CadenceState)
getChordState :: Progression -> Int -> Maybe ChordState
getChordState prog idx = do
  cs <- getCadenceState prog idx
  let chord = fromCadenceState cs
  return $ ChordState chord (stateCadenceRoot cs)

-------------------------------------------------------------------------------
-- Manipulation Functions (Ported from legacy Arranger.hs)
-------------------------------------------------------------------------------

-- |Rotate a progression by n positions
-- Positive n rotates left (first elements move to end)
-- Negative n rotates right (last elements move to front)
rotateProgression :: Int -> Progression -> Progression
rotateProgression n (Progression seq)
  | Seq.null seq = Progression seq
  | otherwise = 
    let len = Seq.length seq
        n' = n `mod` len
        (front, back) = Seq.splitAt n' seq
    in Progression (back >< front)

-- |Extract a subsequence from a progression
-- Start and end are 1-indexed, inclusive
excerptProgression :: Int -> Int -> Progression -> Progression
excerptProgression start end (Progression seq) =
  let start' = max 0 (start - 1)
      len = max 0 (end - start + 1)
  in Progression $ Seq.take len $ Seq.drop start' seq

-- |Insert a progression at a specific position (1-indexed)
insertProgression :: Int -> Progression -> Progression -> Progression
insertProgression pos insert (Progression target) =
  let pos' = max 0 (pos - 1)
      (before, after) = Seq.splitAt pos' target
  in Progression (before >< unProgression insert >< after)

-- |Fuse (interleave) two progressions
fuseProgression :: Progression -> Progression -> Progression
fuseProgression (Progression a) (Progression b) =
  Progression $ Seq.fromList $ interleave (toList a) (toList b)
  where
    interleave [] ys = ys
    interleave xs [] = xs
    interleave (x:xs) (y:ys) = x : y : interleave xs ys

-- |Transpose a progression by n semitones
transposeProgression :: Int -> Progression -> Progression
transposeProgression n (Progression seq) =
  Progression $ fmap (transposeCadenceState n) seq

-- |Helper: transpose a CadenceState
transposeCadenceState :: Int -> CadenceState -> CadenceState
transposeCadenceState n (CadenceState cad root spelling) =
  let newRoot = transposeNoteName n root
      -- Cadence intervals are relative, so they don't change
  in CadenceState cad newRoot spelling

-- |Helper: transpose a NoteName
transposeNoteName :: Int -> NoteName -> NoteName
transposeNoteName n name =
  let pc = unPitchClass (pitchClass name)
      newPc = (pc + n) `mod` 12
  in pcToNoteName newPc
  where
    pcToNoteName 0 = C
    pcToNoteName 1 = Db
    pcToNoteName 2 = D
    pcToNoteName 3 = Eb
    pcToNoteName 4 = E
    pcToNoteName 5 = F
    pcToNoteName 6 = Gb
    pcToNoteName 7 = G
    pcToNoteName 8 = Ab
    pcToNoteName 9 = A
    pcToNoteName 10 = Bb
    pcToNoteName 11 = B
    pcToNoteName _ = C

-- |Overlap two progressions (start second before first ends)
overlapProgression :: Int -> Progression -> Progression -> Progression
overlapProgression overlap (Progression a) (Progression b)
  | overlap >= Seq.length a = Progression b
  | otherwise =
    let aLen = Seq.length a
        aTruncated = Seq.take (aLen - overlap) a
    in Progression (aTruncated >< b)

-- |Expand a progression by repeating it n times
expandProgression :: Int -> Progression -> Progression
expandProgression n prog
  | n <= 0 = mempty
  | otherwise = mconcat (replicate n prog)

-------------------------------------------------------------------------------
-- Voicing Extractors
-- These convert progressions to integer pitch lists for performance
-------------------------------------------------------------------------------

-- |Extract literal voicings (pitch integers as stored)
literalVoicing :: Progression -> [[Int]]
literalVoicing (Progression seq) = 
  map (map fromIntegral . chordIntervals . fromCadenceState) (toList seq)

-- |Extract harmony voicings (pitch classes only, 0-11)
harmonyVoicing :: Progression -> [[Int]]
harmonyVoicing prog = map (map (`mod` 12)) (literalVoicing prog)

-- |Extract close voicings (smallest possible span)
closeVoicing :: Progression -> [[Int]]
closeVoicing prog = map toCloseVoicing (literalVoicing prog)
  where
    toCloseVoicing xs = 
      let sorted = sort $ map (`mod` 12) xs
      in sorted

-- |Extract wide voicings (spread across octaves)
wideVoicing :: Progression -> [[Int]]
wideVoicing prog = map toWideVoicing (literalVoicing prog)
  where
    toWideVoicing [] = []
    toWideVoicing (bass:rest) = 
      let bassOctave = bass
          -- Spread upper voices across higher octaves
          spread = zipWith (+) rest [12, 24, 36, 48]
      in bassOctave : take (length rest) spread

-------------------------------------------------------------------------------
-- Display Helpers
-------------------------------------------------------------------------------

-- |Show a chord using the given enharmonic function.
-- Ported from legacy MusicData.hs showTriad
-- Maps the chord to a human-readable string representation with
-- proper enharmonic spelling (e.g., "C maj" or "F# min/A")
showTriad :: (PitchClass -> NoteName) -> Chord -> String
showTriad f (Chord noteName functionality _)
    -- Root position sus4
    | "sus4" `List.isInfixOf` functionality && not (any (`List.isInfixOf` functionality) ["_1stInv", "_2ndInv"]) =
      show (f $ pitchClass noteName) ++ " " ++ functionality
    -- 1st inversion major: C maj/E (3rd in bass = root + 4 semitones)
    | all (`List.isInfixOf` functionality) ["_1stInv", "maj"] =
      show (f $ pitchClass noteName) ++ " " ++ takeWhile Char.isAlphaNum functionality
      ++ "/" ++ show (f (pitchClass noteName + P 4))
    -- 1st inversion minor: A min/C (3rd in bass = root + 3 semitones)
    | all (`List.isInfixOf` functionality) ["_1stInv", "min"] =
      show (f $ pitchClass noteName) ++ " " ++ takeWhile Char.isAlphaNum functionality
      ++ "/" ++ show (f (pitchClass noteName + P 3))
    -- 1st inversion sus4 -> sus2 equivalent
    | all (`List.isInfixOf` functionality) ["_1stInv", "sus4"] =
      show (f (pitchClass noteName - P 5)) ++ " sus2"
    -- 1st inversion dim (3rd in bass = root + 3 semitones)
    | all (`List.isInfixOf` functionality) ["_1stInv", "dim"] =
      show (f $ pitchClass noteName) ++ " " ++ takeWhile Char.isAlphaNum functionality ++
      "/" ++ show (f (pitchClass noteName + P 3))
    -- 2nd inversion maj/min (5th in bass = root + 7 semitones)
    | "_2ndInv" `List.isInfixOf` functionality && any (`List.isInfixOf` functionality) ["maj", "min"] =
      show (f $ pitchClass noteName) ++ " " ++
      takeWhile Char.isAlphaNum functionality ++ "/" ++ show (f (pitchClass noteName + P 7))
    -- 2nd inversion sus4 (5th in bass = root + 7 semitones)
    | "_2ndInv" `List.isInfixOf` functionality && "sus4" `List.isInfixOf` functionality =
      show (f $ pitchClass noteName) ++ " " ++
      takeWhile Char.isAlphaNum functionality ++ "/" ++ show (f (pitchClass noteName + P 7))
    -- 2nd inversion dim (5th in bass = root + 6 semitones for diminished)
    | "_2ndInv" `List.isInfixOf` functionality && "dim" `List.isInfixOf` functionality =
      show (f $ pitchClass noteName) ++ " " ++
      takeWhile Char.isAlphaNum functionality ++ "/" ++ show (f (pitchClass noteName + P 6))
    -- Root position (default)
    | otherwise = show (f $ pitchClass noteName) ++ " " ++ functionality