{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Harmonic.Interface.Tidal.Arranger
-- Description : Performance-oriented progression manipulation
-- 
-- This module provides shorthand functions for manipulating progressions
-- in a TidalCycles performance context. All functions are designed for
-- live-coding ergonomics (short names, intuitive parameter order).
--
-- Functions wrap the more verbose Progression module functions:
--   * rotate, excerpt, insert, switch, clone, extract
--   * transpose, reverse, fuse, expand
--   * overlap, overlapF, overlapB
--   * root, flow, lite (3 voicing paradigms via cyclic DP)

module Harmonic.Interface.Tidal.Arranger
  ( -- * Position/Range Operations
    rotate
  , excerpt
  , insert
  , switch
  , clone
  , extract

    -- * Transformation Operations
  , transposeP
  , Harmonic.Interface.Tidal.Arranger.reverse
  , fuse
  , fuse2
  , interleave
  , expandP

    -- * Overlap Operations (Progression-level)
  , progOverlap
  , progOverlapF
  , progOverlapB

    -- * Voicing Extractors (3 Paradigms)
  , root   -- Root always in bass, smooth compact voice leading (cyclic DP)
  , flow   -- Any inversion allowed for smoothest voice leading (cyclic DP)
  , lite   -- Literal, no transformation
  , literal -- Alias for lite
  , bass   -- Bass note only (root pitch class)

    -- * Explicit Progression Construction
  , fromChords      -- Construct Progression from pitch-class lists
  , prog            -- Legacy alias for fromChords
  , fromChordsFlat  -- fromChords with flat spelling
  , fromChordsSharp -- fromChords with sharp spelling

    -- * Scale Source (Switch Mechanism)
  , ScaleSource(..)
  , melodyStateFrom
  ) where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (><))
import Data.Foldable (toList)
import Data.List (sort, nub)

import Harmonic.Rules.Types.Progression
import Harmonic.Rules.Types.Harmony (Chord(..), Cadence(..), CadenceState(..), fromCadenceState, ChordState(..), EnharmonicSpelling(..), toFunctionality, toFunctionalityChord, Movement(..), enharmonicFunc)
import Harmonic.Rules.Types.Pitch (PitchClass(..), NoteName(..), pitchClass, enharmFromNoteName, mkPitchClass, unPitchClass)
import Harmonic.Evaluation.Scoring.VoiceLeading (solveRoot, solveFlow, liteVoicing, bassVoicing, normalizeByFirstRoot)

-------------------------------------------------------------------------------
-- Position/Range Operations
-------------------------------------------------------------------------------

-- |Rotate a progression by n bars (positive = left, negative = right)
rotate :: Int -> Progression -> Progression
rotate = rotateProgression

-- |Extract bars start to end (1-indexed, inclusive)
excerpt :: Int -> Int -> Progression -> Progression
excerpt = excerptProgression

-- |Insert a CadenceState at position (1-indexed), replacing the existing one
insert :: CadenceState -> Int -> Progression -> Progression
insert cs pos (Progression seq)
  | Seq.null seq = singleton cs
  | pos < 1 = Progression (cs Seq.<| seq)
  | pos > Seq.length seq = Progression (seq Seq.|> cs)
  | otherwise =
    let (before, rest) = Seq.splitAt (pos - 1) seq
    in case Seq.viewl rest of
         Seq.EmptyL -> Progression (before Seq.|> cs)
         _ Seq.:< after -> Progression (before >< (cs Seq.<| after))

-- |Switch two bars at positions m and n (1-indexed)
switch :: Int -> Int -> Progression -> Progression
switch m n prog@(Progression seq)
  | m == n = prog
  | Seq.null seq = prog
  | otherwise =
    let len = Seq.length seq
        m' = max 0 (min (len - 1) (m - 1))
        n' = max 0 (min (len - 1) (n - 1))
        csM = Seq.index seq m'
        csN = Seq.index seq n'
        seq' = Seq.update m' csN $ Seq.update n' csM seq
    in Progression seq'

-- |Clone bar m to position n (overwrites n with contents of m)
clone :: Int -> Int -> Progression -> Progression
clone m n prog@(Progression seq)
  | m == n = prog
  | Seq.null seq = prog
  | otherwise =
    let len = Seq.length seq
        m' = max 0 (min (len - 1) (m - 1))
        n' = max 0 (min (len - 1) (n - 1))
        csM = Seq.index seq m'
        seq' = Seq.update n' csM seq
    in Progression seq'

-- |Extract a single CadenceState at index (1-indexed)
extract :: Int -> Progression -> Maybe CadenceState
extract n prog = getCadenceState prog n

-------------------------------------------------------------------------------
-- Transformation Operations
-------------------------------------------------------------------------------

-- |Transpose a progression by n semitones
transposeP :: Int -> Progression -> Progression
transposeP = transposeProgression

-- |Reverse a progression
reverse :: Progression -> Progression
reverse (Progression seq) = 
  Progression $ Seq.reverse seq

-- |Fuse multiple progressions into one (concatenation)
-- Matches legacy behavior: simply concatenates all progressions in order.
fuse :: [Progression] -> Progression
fuse = mconcat

-- |Binary fuse for convenience in live coding
-- Example: fuse2 progA progB
fuse2 :: Progression -> Progression -> Progression
fuse2 a b = a <> b

-- |Interleave two progressions (alternating chords)
-- Takes one chord from each progression in turn.
-- Example: interleave [A,B,C] [X,Y,Z] = [A,X,B,Y,C,Z]
interleave :: Progression -> Progression -> Progression
interleave = fuseProgression

-- |Expand a progression by repeating each chord n times
expandP :: Int -> Progression -> Progression
expandP = expandProgression

-------------------------------------------------------------------------------
-- Overlap Operations
-- These create sustain/legato effects by merging pitches from adjacent chords
-------------------------------------------------------------------------------

-- |Bidirectional overlap: merge pitches from n bars in both directions
progOverlap :: Int -> Progression -> Progression
progOverlap range prog@(Progression seq)
  | range <= 0 = prog
  | Seq.null seq = prog
  | otherwise = 
    let chords = toList $ fmap fromCadenceState seq
        cadences = toList $ fmap stateCadence seq
        roots = toList $ fmap stateCadenceRoot seq
        len = length chords
        
        -- For each position, gather pitches from range bars before and after
        overlappedChords = 
          [ overlapAt i chords range | i <- [0..len-1] ]
        
        -- Rebuild CadenceStates with original cadences but new chord intervals
        newSeq = Seq.fromList $ zipWith3 rebuildCadenceState cadences roots overlappedChords
    in Progression newSeq

-- |Forward-only overlap: merge pitches from n bars ahead
progOverlapF :: Int -> Progression -> Progression
progOverlapF range prog@(Progression seq)
  | range <= 0 = prog
  | Seq.null seq = prog
  | otherwise = 
    let chords = toList $ fmap fromCadenceState seq
        cadences = toList $ fmap stateCadence seq
        roots = toList $ fmap stateCadenceRoot seq
        len = length chords
        
        overlappedChords = 
          [ overlapForwardAt i chords range | i <- [0..len-1] ]
        
        newSeq = Seq.fromList $ zipWith3 rebuildCadenceState cadences roots overlappedChords
    in Progression newSeq

-- |Backward-only overlap: merge pitches from n bars behind  
progOverlapB :: Int -> Progression -> Progression
progOverlapB range prog@(Progression seq)
  | range <= 0 = prog
  | Seq.null seq = prog
  | otherwise = 
    let chords = toList $ fmap fromCadenceState seq
        cadences = toList $ fmap stateCadence seq
        roots = toList $ fmap stateCadenceRoot seq
        len = length chords
        
        overlappedChords = 
          [ overlapBackwardAt i chords range | i <- [0..len-1] ]
        
        newSeq = Seq.fromList $ zipWith3 rebuildCadenceState cadences roots overlappedChords
    in Progression newSeq

-- Helper: get overlapped pitches for position i (bidirectional)
overlapAt :: Int -> [Chord] -> Int -> [Integer]
overlapAt i chords range =
  let len = length chords
      indices = [max 0 (i - range) .. min (len - 1) (i + range)]
      allPitches = concatMap (chordIntervals . (chords !!)) indices
  in nub allPitches

-- Helper: get overlapped pitches for position i (forward only)
overlapForwardAt :: Int -> [Chord] -> Int -> [Integer]
overlapForwardAt i chords range =
  let len = length chords
      indices = [i .. min (len - 1) (i + range)]
      allPitches = concatMap (chordIntervals . (chords !!)) indices
  in nub allPitches

-- Helper: get overlapped pitches for position i (backward only)
overlapBackwardAt :: Int -> [Chord] -> Int -> [Integer]
overlapBackwardAt i chords range =
  let indices = [max 0 (i - range) .. i]
      allPitches = concatMap (chordIntervals . (chords !!)) indices
  in nub allPitches

-- Helper: rebuild a CadenceState with new intervals
rebuildCadenceState :: Cadence -> NoteName -> [Integer] -> CadenceState
rebuildCadenceState cad root newIntervals =
  let -- Create a modified cadence with the new intervals (as PitchClasses)
      newPCs = map (\i -> mkPitchClass (fromIntegral i)) newIntervals
      newCad = cad { cadenceIntervals = newPCs }
  in CadenceState newCad root FlatSpelling

-------------------------------------------------------------------------------
-- Voicing Extractors (3 Paradigms)
-------------------------------------------------------------------------------

-- |ROOT paradigm: Smooth compact voice leading with root always in bass.
-- Uses cyclic DP to find globally optimal voicings.
-- First chord starts compact with root in bass; all subsequent chords
-- maintain root in bass with minimal voice movement.
root :: Progression -> [[Int]]
root prog = 
  let intVoicings = map (map fromIntegral) $ literalVoicing' prog
  in solveRoot intVoicings

-- |FLOW paradigm: Smoothest voice leading with any inversion allowed.
-- Uses cyclic DP to find globally optimal voicings.
-- Voice crossings permitted for optimal smoothness; bass doesn't need
-- to be the root if an inversion provides smoother voice leading.
flow :: Progression -> [[Int]]
flow prog = 
  let intVoicings = map (map fromIntegral) $ literalVoicing' prog
  in solveFlow intVoicings

-- |LITE paradigm: Literal voicings with first-root normalization.
-- Returns pitches as stored, but normalized so first chord's root is in [7,18].
-- No voice leading optimization applied (only octave normalization).
lite :: Progression -> [[Int]]
lite prog = 
  let raw = map (map fromIntegral) $ literalVoicing' prog
  in normalizeByFirstRoot raw

-- |BASS paradigm: Bass note only (root pitch class per chord).
-- Extracts the root note (first element, mod 12) from each chord.
-- Returns as single-element lists in [0,11] range.
bass :: Progression -> [[Int]]
bass prog = 
  let raw = map (map fromIntegral) $ literalVoicing' prog
  in bassVoicing raw

-- |Alias for lite (legacy compatibility)
literal :: Progression -> [[Int]]
literal = lite

-- Helper to get literal voicings as Integer lists (internal use)
literalVoicing' :: Progression -> [[Integer]]
literalVoicing' (Progression seq) =
  map (chordIntervals . fromCadenceState) (toList seq)

-------------------------------------------------------------------------------
-- Explicit Progression Construction
-------------------------------------------------------------------------------

-- |Convert pitch-class list to zero-form intervals (relative to root).
-- Zero-form means the lowest pitch class becomes 0, and all other pitch
-- classes are expressed as intervals from it.
-- Example: [5, 9, 0] (F major) becomes [0, 4, 7]
toZeroForm :: [Int] -> [Int]
toZeroForm [] = []
toZeroForm pcs =
  let sorted = sort pcs
      root = head sorted
   in map (\p -> (p - root) `mod` 12) sorted

-- |Name a chord from its zero-form intervals.
-- Uses legacy chord naming logic (toFunctionality for 3-note chords,
-- toFunctionalityChord for extended harmonies).
nameChord :: [Int] -> String
nameChord intervals
  | length intervals == 3 =
      toFunctionality (map mkPitchClass intervals)
  | otherwise =
      toFunctionalityChord (map mkPitchClass intervals)

-- |Construct a Progression from explicit pitch-class sets.
-- This is the main function for composing/arranging workflow (not generation).
-- Takes an enharmonic spelling and a list of chord pitch-class sets,
-- returns a Progression ready for 'arrange'.
--
-- Example:
-- @
-- fromChords FlatSpelling [[0,4,7], [5,9,0], [7,11,2]]
--   --> C major → F major → G major
-- @
fromChords :: EnharmonicSpelling -> [[Int]] -> Progression
fromChords _ [] = mempty
fromChords spelling chordSets = Progression (Seq.fromList cadenceStates)
  where
    -- Get enharmonic conversion function for this spelling
    enharm = enharmonicFunc spelling

    cadenceStates = map toCadenceState chordSets

    toCadenceState :: [Int] -> CadenceState
    toCadenceState pcs =
      let sorted = sort pcs
          root = if null sorted then 0 else head sorted
          rootPC = mkPitchClass root
          intervals = toZeroForm pcs
          intervalPCs = map mkPitchClass intervals
          chordName = nameChord intervals
          -- Create Cadence with record syntax
          cadence = Cadence
            { cadenceFunctionality = chordName
            , cadenceMovement = Unison  -- Placeholder (no prior context)
            , cadenceIntervals = intervalPCs
            }
          rootNote = enharm rootPC
       in CadenceState cadence rootNote spelling

-- |Legacy alias for fromChords (matches legacy prog function)
prog :: EnharmonicSpelling -> [[Int]] -> Progression
prog = fromChords

-- |Convenience: fromChords with flat spelling
fromChordsFlat :: [[Int]] -> Progression
fromChordsFlat = fromChords FlatSpelling

-- |Convenience: fromChords with sharp spelling
fromChordsSharp :: [[Int]] -> Progression
fromChordsSharp = fromChords SharpSpelling

-------------------------------------------------------------------------------
-- Scale Source (Switch Mechanism)
-------------------------------------------------------------------------------

-- |Scale source for melody mapping.
-- Enables flexible melody construction by allowing harmony (with optional
-- overlap) to serve as the scale source instead of explicit scale definitions.
data ScaleSource
  = ExplicitScale [[Int]]           -- ^ User-defined scale per chord
  | HarmonyAsScale Progression      -- ^ Use harmony chords as scales
  | HarmonyWithOverlap Progression (Int -> Progression -> Progression)
    -- ^ Use harmony with overlap function applied

-- |Create melody state from scale source.
-- Converts a ScaleSource into a Progression suitable for melody arrangement.
melodyStateFrom :: ScaleSource -> Progression
melodyStateFrom (ExplicitScale scales) = fromChordsFlat scales
melodyStateFrom (HarmonyAsScale prog) = prog  -- Direct passthrough
melodyStateFrom (HarmonyWithOverlap prog overlapFn) = overlapFn 1 prog