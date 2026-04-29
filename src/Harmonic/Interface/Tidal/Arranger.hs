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
  , strataModeFlow  -- Per-voice tracking for non-triad chroma layers (key-signature semantic)

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
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import Harmonic.Rules.Types.ProgressionContext (ProgressionContext, liftPC)
import Harmonic.Rules.Types.Harmony (Chord(..), Cadence(..), CadenceState(..), fromCadenceState, ChordState(..), EnharmonicSpelling(..), toFunctionality, toFunctionalityChord, Movement(..), enharmonicFunc, inferSpelling, initCadenceState)
import Harmonic.Traversal.Probabilistic (gammaIndexScaledWith)
import Harmonic.Evaluation.Scoring.Dissonance (dissonanceScore)
import Harmonic.Rules.Types.Pitch (PitchClass(..), NoteName(..), pitchClass, mkPitchClass, unPitchClass)
import Harmonic.Evaluation.Scoring.VoiceLeading (solveRoot, solveFlow, liteVoicing, bassVoicing, normalizeByFirstRoot, initialCompact)
import Data.Function (on)
import Data.List (minimumBy)

-------------------------------------------------------------------------------
-- Position/Range Operations
-------------------------------------------------------------------------------

-- |Rotate a progression by n bars (positive = left, negative = right)
rotate :: Int -> ProgressionContext -> ProgressionContext
rotate n = liftPC (rotateProgression n)

-- |Extract bars start to end (1-indexed, inclusive)
excerpt :: Int -> Int -> ProgressionContext -> ProgressionContext
excerpt s e = liftPC (excerptProgression s e)

-- |Insert a CadenceState at position (1-indexed), replacing the existing one
insert :: CadenceState -> Int -> ProgressionContext -> ProgressionContext
insert cs pos = liftPC (insertProg cs pos)
  where
    insertProg :: CadenceState -> Int -> Progression -> Progression
    insertProg c p (Progression s)
      | Seq.null s = singleton c
      | p < 1 = Progression (c Seq.<| s)
      | p > Seq.length s = Progression (s Seq.|> c)
      | otherwise =
        let (before, rest) = Seq.splitAt (p - 1) s
        in case Seq.viewl rest of
             Seq.EmptyL -> Progression (before Seq.|> c)
             _ Seq.:< after -> Progression (before >< (c Seq.<| after))

-- |Switch two bars at positions m and n (1-indexed)
switch :: Int -> Int -> ProgressionContext -> ProgressionContext
switch m n = liftPC (switchProg m n)
  where
    switchProg :: Int -> Int -> Progression -> Progression
    switchProg a b progIn@(Progression s)
      | a == b = progIn
      | Seq.null s = progIn
      | otherwise =
        let len = Seq.length s
            m' = max 0 (min (len - 1) (a - 1))
            n' = max 0 (min (len - 1) (b - 1))
            csM = Seq.index s m'
            csN = Seq.index s n'
            s'  = Seq.update m' csN $ Seq.update n' csM s
        in Progression s'

-- |Clone bar m to position n (overwrites n with contents of m)
clone :: Int -> Int -> ProgressionContext -> ProgressionContext
clone m n = liftPC (cloneProg m n)
  where
    cloneProg :: Int -> Int -> Progression -> Progression
    cloneProg a b progIn@(Progression s)
      | a == b = progIn
      | Seq.null s = progIn
      | otherwise =
        let len = Seq.length s
            m' = max 0 (min (len - 1) (a - 1))
            n' = max 0 (min (len - 1) (b - 1))
            csM = Seq.index s m'
            s'  = Seq.update n' csM s
        in Progression s'

-- |Extract a single CadenceState at index (1-indexed, modulo wrap) from the triad layer
extract :: Int -> ProgressionContext -> CadenceState
extract n pc
  | len == 0  = error "extract: empty progression"
  | otherwise = case getCadenceState prog idx of
      Just cs -> cs
      Nothing -> error "extract: internal error"
  where
    prog = PC.triadLayer pc
    len  = progLength prog
    idx  = ((n - 1) `mod` len) + 1  -- 1-indexed with modulo wrap

-------------------------------------------------------------------------------
-- Transformation Operations
-------------------------------------------------------------------------------

-- |Transpose a progression by n semitones
transposeP :: Int -> ProgressionContext -> ProgressionContext
transposeP n = liftPC (transposeProgression n)

-- |Reverse a progression
reverse :: ProgressionContext -> ProgressionContext
reverse = liftPC (\(Progression s) -> Progression (Seq.reverse s))

-- |Fuse multiple progressions into one (concatenation)
fuse :: [ProgressionContext] -> ProgressionContext
fuse = mconcat

-- |Binary fuse for convenience in live coding
fuse2 :: ProgressionContext -> ProgressionContext -> ProgressionContext
fuse2 a b = a <> b

-- |Interleave two progressions (alternating chords)
-- Example: interleave [A,B,C] [X,Y,Z] = [A,X,B,Y,C,Z]
interleave :: ProgressionContext -> ProgressionContext -> ProgressionContext
interleave a b = PC.ProgressionContext
  { PC.triadLayer   = fuseProgression (PC.triadLayer a)  (PC.triadLayer b)
  , PC.strataLayer  = fuseProgression (PC.strataLayer a) (PC.strataLayer b)
  , PC.modeLayer    = fuseProgression (PC.modeLayer a)   (PC.modeLayer b)
  , PC.pcProvenance = Nothing
  }

-- |Expand a progression by repeating each chord n times
expandP :: Int -> ProgressionContext -> ProgressionContext
expandP n = liftPC (expandProgression n)

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

-- |STRATA-MODE-FLOW paradigm: each bar is the bar's chroma in
-- sorted-ascending compressed form rooted on its harmonic root, with the
-- whole voicing octave-shifted to minimise voice movement against the bar 0
-- anchor. Functions as a "key signature": pattern index @i@ in any bar plays
-- the i-th scale degree of that bar's strata / mode, so pattern increments
-- of 1 always ascend by one set member and decrements descend by one.
--
-- Bar 0: 'initialCompact' + 'normalizeByFirstRoot' anchors the harmonic
-- root in the standard window ([-12, -1] note range). Span ≤ 12 semitones
-- from root upward.
--
-- Bar n+1: 'initialCompact' rooted on bar n+1's harmonic root produces a
-- "natural" compressed-ascending voicing; the whole voicing is then shifted
-- by the octave (k·12 for @k ∈ [-3..3]@) that minimises total
-- @|placed_MIDI - anchor_MIDI|@ across voices, where 'anchor' is bar 0's
-- voicing. The root is allowed to migrate octaves freely if that makes the
-- line closer to the anchor. Voicing remains sorted ascending after the
-- shift (uniform shift preserves order), so "ascend by 1 with idx+1" holds.
--
-- Anchoring to bar 0 (rather than the previous bar) guarantees:
--   * No drift over long walks — every bar stays within ~6 semitones of the
--     anchor.
--   * Cyclic return to anchor at the pattern wrap (bar N-1 → bar 0).
--   * When chroma cycles back to bar 0's chroma (e.g. tristrata II-VI-X
--     repeating), the bar lands on bar 0's exact MIDI (shift = 0).
--
-- O(n × k) per bar where n = chroma cardinality and k = number of octave
-- candidates (~7). Sub-microsecond per bar; eager forcing in 'Bridge.arrange'
-- still hoists the work to REPL evaluation time.
strataModeFlow :: Progression -> [[Int]]
strataModeFlow prog =
  case toList (unProgression prog) of
    []                  -> []
    (firstCS : restCSs) ->
      let firstPCs    = cadencePCs firstCS
          firstRootPC = case firstPCs of (p:_) -> p; [] -> 0
          v0          = initialCompact firstRootPC firstPCs
          voicings    = v0 : map (shiftBar v0) restCSs
      in normalizeByFirstRoot voicings

-- |Build a bar's natural compressed-ascending voicing, then choose the
-- uniform octave shift that minimises total absolute MIDI distance to the
-- bar 0 anchor 'v0'. Each bar is independently anchored so drift is bounded.
shiftBar :: [Int] -> CadenceState -> [Int]
shiftBar v0 cs =
  let nextPCs    = cadencePCs cs
      nextRootPC = case nextPCs of (p:_) -> p; [] -> 0
      natural    = initialCompact nextRootPC nextPCs
      candidates = [ map (+ (k * 12)) natural | k <- [-3 .. 3] ]
      score v    = sum (zipWith (\a b -> abs (a - b)) v v0)
  in case candidates of
       [] -> v0
       _  -> minimumBy (compare `on` score) candidates

-- |Read a CadenceState's absolute PCs in cadence-interval order (NOT sorted).
-- For genP strata/mode layers (intervals start at 0 from harmonic root), this
-- yields [root, root+2nd, root+3rd, ...] — i.e. degree-ordered. Pattern idx 0
-- therefore tracks the root.
cadencePCs :: CadenceState -> [Int]
cadencePCs cs =
  let r    = unPitchClass (pitchClass (stateCadenceRoot cs))
      ints = map unPitchClass (cadenceIntervals (stateCadence cs))
  in [ (i + r) `mod` 12 | i <- ints ]

-- Helper to get literal voicings as Integer lists (internal use).
-- Reads cadence intervals directly so non-triad CadenceStates (5-PC strata,
-- 7-PC mode in genP-derived ProgressionContexts) survive without toTriad
-- reduction. For 3-PC triad cadences this produces the same PCs as the
-- legacy chordIntervals path.
literalVoicing' :: Progression -> [[Integer]]
literalVoicing' (Progression seq) =
  map cadenceVoicing (toList seq)
  where
    cadenceVoicing cs =
      let rootPC = unPitchClass (pitchClass (stateCadenceRoot cs))
          tones = cadenceIntervals (stateCadence cs)
          pcs   = map (\t -> (unPitchClass t + rootPC) `mod` 12) tones
      in map fromIntegral pcs

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
fromChords :: [[Int]] -> ProgressionContext
fromChords = PC.fromProgression . fromChordsRaw

fromChordsRaw :: [[Int]] -> Progression
fromChordsRaw [] = mempty
fromChordsRaw chordSets = Progression (Seq.fromList cadenceStates)
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
prog :: [[Int]] -> ProgressionContext
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
melodyStateFrom (ExplicitScale scales) = fromChordsRaw scales
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
  rng <- createSystemRandom
  let toks = parseLeadTokens input
      mRoot = listToMaybe [r | RootTok r <- toks]
      mQual = listToMaybe [q | QualTok q <- toks]
      mMove = listToMaybe [m | MoveTok m <- toks]
  rootStr          <- maybe (randomRoot rng) pure mRoot
  (qualLabel, ivs) <- resolveQuality rng mQual
  movement         <- maybe (uniformRM (-5, 6) rng) pure mMove
  let cs = initCadenceState movement rootStr ivs
  putStrLn $ rootStr ++ " " ++ qualLabel
  pure cs