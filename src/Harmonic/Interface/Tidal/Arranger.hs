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
  , grid   -- Root locked in bass, smooth compact voice leading (cyclic DP)
  , flow   -- Any inversion allowed for smoothest voice leading (cyclic DP)
  , lite   -- Literal, no transformation
  , literal -- Alias for lite
  , root   -- Root note only (root pitch class per chord)

    -- * Explicit Progression Construction
  , fromChords      -- Construct Progression from pitch-class lists
  , prog            -- Legacy alias for fromChords

    -- * Scale Source (Switch Mechanism)
  , ScaleSource(..)
  , melodyStateFrom

    -- * Starting State Construction
  , lead
  , parseLeadTokens
  , LeadToken(..)
  ) where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (><))
import Data.Foldable (toList)
import Data.List (sort, nub, sortBy)
import Data.Maybe (listToMaybe)
import Data.Char (toLower)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import System.Random.MWC (createSystemRandom, uniformRM, GenIO)

import Harmonic.Rules.Types.Progression
import Harmonic.Rules.Types.Harmony (Chord(..), Cadence(..), CadenceState(..), fromCadenceState, ChordState(..), EnharmonicSpelling(..), toFunctionality, toFunctionalityChord, Movement(..), enharmonicFunc, inferSpelling, initCadenceState)
import Harmonic.Traversal.Probabilistic (gammaIndexScaledWith)
import Harmonic.Evaluation.Scoring.Dissonance (dissonanceScore)
import Harmonic.Rules.Types.Pitch (PitchClass(..), NoteName(..), pitchClass, mkPitchClass, unPitchClass)
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

-- |Extract a single CadenceState at index (1-indexed, modulo wrap)
extract :: Int -> Progression -> CadenceState
extract n prog
  | len == 0  = error "extract: empty progression"
  | otherwise = case getCadenceState prog idx of
      Just cs -> cs
      Nothing -> error "extract: internal error"
  where
    len = progLength prog
    idx = ((n - 1) `mod` len) + 1  -- 1-indexed with modulo wrap

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
      -- Infer spelling from the new absolute pitches
      rootPC = pitchClass root
      absolutePitches = map (\i -> (fromIntegral i + unPitchClass rootPC) `mod` 12) newIntervals
      spelling = inferSpelling absolutePitches
  in CadenceState newCad root spelling

-------------------------------------------------------------------------------
-- Voicing Extractors (3 Paradigms)
-------------------------------------------------------------------------------

-- |GRID paradigm: Root locked in bass with smooth compact voice leading.
-- Uses cyclic DP to find globally optimal voicings.
-- First chord starts compact with root in bass; all subsequent chords
-- maintain root in bass with minimal voice movement.
grid :: Progression -> [[Int]]
grid prog =
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

-- |ROOT paradigm: Root note only (root pitch class per chord).
-- Extracts the root note (first element, mod 12) from each chord.
-- Returns as single-element lists in [0,11] range.
root :: Progression -> [[Int]]
root prog =
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
-- fromChords [[0,4,7], [5,9,0], [7,11,2]]
--   --> C major → F major → G major
-- @
fromChords :: [[Int]] -> Progression
fromChords [] = mempty
fromChords chordSets = Progression (Seq.fromList cadenceStates)
  where
    cadenceStates = map toCadenceState chordSets

    toCadenceState :: [Int] -> CadenceState
    toCadenceState pcs =
      let root = if null pcs then 0 else head pcs
          rootPC = mkPitchClass root
          intervals = sort $ map (\p -> (p - root) `mod` 12) pcs
          intervalPCs = map mkPitchClass intervals
          chordName = nameChord intervals
          -- Create Cadence with record syntax
          cadence = Cadence
            { cadenceFunctionality = chordName
            , cadenceMovement = Unison  -- Placeholder (no prior context)
            , cadenceIntervals = intervalPCs
            }
          -- Infer spelling from absolute pitch content
          spelling = inferSpelling (map (`mod` 12) pcs)
          rootNote = enharmonicFunc spelling rootPC
       in CadenceState cadence rootNote spelling

-- |Legacy alias for fromChords (matches legacy prog function)
prog :: [[Int]] -> Progression
prog = fromChords

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
melodyStateFrom (ExplicitScale scales) = fromChords scales
melodyStateFrom (HarmonyAsScale prog) = prog  -- Direct passthrough
melodyStateFrom (HarmonyWithOverlap prog overlapFn) = overlapFn 1 prog

-------------------------------------------------------------------------------
-- Starting State Construction
-------------------------------------------------------------------------------

-- All unique 3-note zero-form sets: [0, a, b] with 1 ≤ a < b ≤ 11 (55 total)
allTriadZeroForms :: [[Int]]
allTriadZeroForms = [[0, a, b] | a <- [1..10], b <- [a+1..11]]

-- Map from quality name → all sets producing that name, sorted by dissonance
qualityMap :: Map.Map String [[Int]]
qualityMap =
  Map.map (sortBy (comparing dissonanceScore))
  $ Map.fromListWith (++)
    [ (name, [zf])
    | zf <- allTriadZeroForms
    , let name = toFunctionality (map mkPitchClass zf)
    , not (null name)
    ]

-- User-friendly alias table: shorthand → interval set variants (most consonant first)
qualityAliases :: Map.Map String [[Int]]
qualityAliases = Map.fromList
  [ ("maj",  [[0,4,7]])
  , ("min",  [[0,3,7]])
  , ("dim",  [[0,3,6]])
  , ("aug",  [[0,4,8]])
  , ("7",    sortBy (comparing dissonanceScore) [[0,4,10], [0,7,10]])
  , ("dom7", sortBy (comparing dissonanceScore) [[0,4,10], [0,7,10]])
  , ("maj7", sortBy (comparing dissonanceScore) [[0,4,11], [0,7,11]])
  , ("min7", [[0,3,10]])
  , ("m7",   [[0,3,10]])
  , ("dim7", [[0,3,6]])
  , ("hdim", sortBy (comparing dissonanceScore) [[0,3,6], [0,3,10]])
  , ("sus2", [[0,2,7]])
  , ("sus4", [[0,5,7]])
  , ("6",    [[0,4,9]])
  , ("m6",   [[0,3,9]])
  ]

-- Note name parsing table: lowercase → canonical
noteNameTable :: [(String, String)]
noteNameTable =
  [ ("c","C"), ("db","Db"), ("c#","C#"), ("d","D")
  , ("eb","Eb"), ("d#","D#"), ("e","E"), ("f","F")
  , ("gb","Gb"), ("f#","F#"), ("g","G"), ("ab","Ab")
  , ("g#","G#"), ("a","A"), ("bb","Bb"), ("a#","A#")
  , ("b","B")
  ]

-- |Token type for 'parseLeadTokens'
data LeadToken = RootTok String | QualTok String | MoveTok Int
  deriving (Show, Eq)

-- Parse a movement token: "(N)" or "(-N)" → Just N
parseMovement :: String -> Maybe Int
parseMovement ('(':rest) =
  case Prelude.reverse rest of
    (')':inner) -> case reads (Prelude.reverse inner) :: [(Int, String)] of
      [(n, "")] -> Just n
      _ -> Nothing
    _ -> Nothing
parseMovement _ = Nothing

-- Classify a single token as root, movement, or quality
classifyToken :: String -> LeadToken
classifyToken tok
  | Just canonical <- lookup (map toLower tok) noteNameTable = RootTok canonical
  | Just n         <- parseMovement tok                       = MoveTok n
  | otherwise                                                 = QualTok tok

-- |Parse a lead string into a list of typed tokens.
-- Each space-separated token is independently classified as root, quality, or movement.
parseLeadTokens :: String -> [LeadToken]
parseLeadTokens = map classifyToken . words

-- Pick a variant from a sorted list, biased toward the most consonant
pickVariant :: GenIO -> String -> [[Int]] -> IO (String, [Int])
pickVariant gen label variants = do
  idx <- gammaIndexScaledWith gen 0.1 (length variants)
  pure (label, variants !! idx)

-- Resolve a quality string to (label, intervals), or fall through to random
resolveQuality :: GenIO -> Maybe String -> IO (String, [Int])
resolveQuality gen Nothing  = randomQuality gen
resolveQuality gen (Just q) = do
  let qLower = map toLower q
  case Map.lookup qLower qualityAliases of
    Just vs -> pickVariant gen q vs
    Nothing -> case Map.lookup qLower qualityMap of
      Just vs -> pickVariant gen q vs
      Nothing -> randomQuality gen

-- Select a random quality, biased toward consonant (low entropy gamma)
randomQuality :: GenIO -> IO (String, [Int])
randomQuality gen = do
  let entries = sortBy (comparing (\(_,vs) -> dissonanceScore (head vs))) (Map.toList qualityMap)
  idx <- gammaIndexScaledWith gen 0.2 (length entries)
  let (name, variants) = entries !! idx
  pickVariant gen name variants

-- Select a random root from the 12 chromatic notes (uniform)
randomRoot :: GenIO -> IO String
randomRoot gen = do
  let roots = ["C","C#","D","Eb","E","F","F#","G","Ab","A","Bb","B"]
  idx <- uniformRM (0, length roots - 1) gen
  pure (roots !! idx)

-- |Construct a 'CadenceState' from a human-readable string.
--
-- Parses root, quality, and movement from space-separated tokens.
-- Unspecified components fall through to randomness.
-- Prints "root quality" to the console after construction.
--
-- Examples:
-- @
-- start <- lead "E min (5)"  -- E minor, ascending 5th
-- start <- lead "E min"      -- E minor, random movement
-- start <- lead "min"        -- random root, minor quality, random movement
-- start <- lead "E"          -- E, random quality, random movement
-- start <- lead ""           -- fully random
-- start <- lead "(5)"        -- random root and quality, fixed movement 5
-- @
lead :: String -> IO CadenceState
lead input = do
  gen <- createSystemRandom
  let toks = parseLeadTokens input
      mRoot = listToMaybe [r | RootTok r <- toks]
      mQual = listToMaybe [q | QualTok q <- toks]
      mMove = listToMaybe [m | MoveTok m <- toks]
  rootStr          <- maybe (randomRoot gen) pure mRoot
  (qualLabel, ivs) <- resolveQuality gen mQual
  movement         <- maybe (uniformRM (-5, 6) gen) pure mMove
  let cs = initCadenceState movement rootStr ivs
  putStrLn $ rootStr ++ " " ++ qualLabel
  pure cs