{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Harmonic.Core.VoiceLeading
-- Description : Cyclic DP voice leading optimization
-- 
-- This module implements the Traversal (T) component of the Creative Systems
-- Framework. Voice leading determines HOW we move through harmonic space.
--
-- KEY DESIGN DECISIONS:
--
-- 1. Cost Function Approach
--    Rather than hard constraints, voice leading quality is measured via
--    a cost function. This allows flexible optimization strategies.
--
-- 2. Cyclic Dynamic Programming
--    Uses DP to find globally optimal voicings for the entire cyclic 
--    progression, considering wrap-around from last to first chord.
--
-- 3. Register Constraints
--    All voices constrained to [0, 36] range (C2 to C5 roughly).
--    First chord starts in compact root position.
--
-- 4. Two Paradigms:
--    * root: smooth, compact voice leading with root always in bass
--    * flow: smooth, compact voice leading allowing any inversion

module Harmonic.Core.VoiceLeading
  ( -- * Cost Functions
    voiceLeadingCost
  , totalCost
  , cyclicCost
  
    -- * Voice Movement Calculation
  , voiceMovement
  , minimalMovement
  
    -- * Candidate Generation
  , allVoicings
  , initialCompact
  
    -- * Paradigm Solvers (Cyclic DP)
  , solveRoot
  , solveFlow
  , liteVoicing
  , bassVoicing
  
    -- * Post-processing
  , normalizeByFirstRoot
  ) where

import Data.List (sort, minimumBy, nub)
import Data.Function (on)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import GHC.Generics (Generic)

import Harmonic.Core.Pitch (PitchClass(..), mkPitchClass, unPitchClass, transpose)

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

-- | Minimum allowed pitch for exploration (exploration floor)
-- Allows bass to descend up to a 5th below target octave
minPitch :: Int
minPitch = 7

-- | Maximum allowed pitch for exploration (exploration ceiling)
-- Allows upper voices to ascend up to a 7th above target octave
maxPitch :: Int
maxPitch = 41

-- | Target octave lower bound (middle C octave)
-- Initial voicings start here
targetOctaveMin :: Int
targetOctaveMin = 12

-- | Target octave upper bound
targetOctaveMax :: Int
targetOctaveMax = 23

-- | Target range for first chord's root (lower bound, G0)
targetFirstRootMin :: Int
targetFirstRootMin = 7

-- | Target range for first chord's root (upper bound, F#1)
targetFirstRootMax :: Int
targetFirstRootMax = 18

-------------------------------------------------------------------------------
-- Voice Movement Calculation
-------------------------------------------------------------------------------

-- |Calculate the movement for a single voice between two concrete pitches.
-- Movement is measured in absolute semitones (not mod 12 since we're
-- working with concrete pitches in the [0,36] range).
voiceMovement :: Int -> Int -> Int
voiceMovement from to = abs (to - from)
{-# INLINE voiceMovement #-}

-- |Calculate minimal movement between two pitch class values (mod 12)
minimalMovement :: PitchClass -> PitchClass -> Int
minimalMovement (P from) (P to) = 
  let up   = (to - from) `mod` 12
      down = (from - to) `mod` 12
  in min up down
{-# INLINE minimalMovement #-}

-------------------------------------------------------------------------------
-- Cost Functions
-------------------------------------------------------------------------------

-- |Calculate the voice leading cost between two chords.
-- Cost is the sum of minimal movements for all voices, with penalties.
--
-- Cost components:
--   * Base: sum of minimal movements for each voice pair
--   * Parallel penalty: +3 for each parallel perfect 5th/octave when voices move
--   * Large leap penalty: +2 for any voice moving > 4 semitones
--
-- This is a principled redesign of the legacy smoothBass approach.
voiceLeadingCost :: [Int] -> [Int] -> Int
voiceLeadingCost from to
  | length from /= length to = 999  -- Incompatible voicings
  | from == to = 0  -- Identical voicings have zero cost
  | otherwise = baseCost + parallelPenalty + leapPenalty
  where
    movements = zipWith voiceMovement from to
    baseCost = sum movements
    
    -- Penalty for parallel perfect intervals (P5, P8) when voices actually move
    -- Only count parallel motion if both voices involved actually moved
    parallelPenalty = 
      let pairs = zip3 (zip from (tail from)) (zip to (tail to)) (zip movements (tail movements))
          isPerfect n = n == 7 || n == 0  -- P5 or P8
          isParallelPerfect ((f1, f2), (t1, t2), (m1, m2)) =
            let fromInt = (f2 - f1) `mod` 12
                toInt = (t2 - t1) `mod` 12
            in isPerfect fromInt && fromInt == toInt && (m1 > 0 || m2 > 0)
      in 3 * length (filter isParallelPerfect pairs)
    
    -- Penalty for large leaps (>4 semitones)
    leapPenalty = 2 * length (filter (> 4) movements)

-- |Calculate intervals between adjacent voices in a chord
intervalsBetweenVoices :: [Int] -> [Int]
intervalsBetweenVoices xs = zipWith (\a b -> (b - a) `mod` 12) xs (tail xs)

-- |Calculate the span of a chord (highest - lowest pitch)
chordSpan :: [Int] -> Int
chordSpan [] = 0
chordSpan xs = maximum xs - minimum xs

-- |Calculate total voice leading cost for a sequence of chords
totalCost :: [[Int]] -> Int
totalCost [] = 0
totalCost [_] = 0
totalCost chords = sum $ zipWith voiceLeadingCost chords (tail chords)

-- |Calculate cyclic cost: total cost including wrap-around from last to first.
-- This is essential for loop-aware optimization.
--
-- From the evaluation document:
--   "Adding wrap-around cost to the voice leading solver solves the 'drift'
--    issue elegantly. It forces the algorithm to find a path that is not
--    just locally optimal, but topologically closed."
cyclicCost :: [[Int]] -> Int
cyclicCost [] = 0
cyclicCost [x] = 0
cyclicCost chords = totalCost chords + voiceLeadingCost (last chords) (head chords)

-------------------------------------------------------------------------------
-- Candidate Generation
-------------------------------------------------------------------------------

-- |Get all valid octave placements for a pitch class within [minPitch, maxPitch]
-- Generates placements at base, +12, and +24 semitones (3 octaves)
pitchPlacements :: Int -> [Int]
pitchPlacements pc = 
  let pcMod = pc `mod` 12
  in filter (\p -> p >= minPitch && p <= maxPitch) 
       [pcMod, pcMod + 12, pcMod + 24]

-- |Generate all valid voicings for a triad (3 pitch classes).
-- Each pitch class can be placed at multiple octaves within [0, 36].
-- Results are sorted low-to-high and deduplicated.
allVoicings :: [Int] -> [[Int]]
allVoicings pcs
  | length pcs /= 3 = [sort pcs]  -- Fallback for non-triads
  | otherwise = 
    let [pc0, pc1, pc2] = map (`mod` 12) pcs
        placements0 = pitchPlacements pc0
        placements1 = pitchPlacements pc1
        placements2 = pitchPlacements pc2
        -- Generate all combinations
        allCombos = [[p0, p1, p2] | p0 <- placements0, p1 <- placements1, p2 <- placements2]
        -- Sort each voicing low-to-high
        sorted = map sort allCombos
    in nub sorted

-- |Create initial compact voicing: root in bass in target octave (12-23),
-- upper voices stacked compactly above.
initialCompact :: Int -> [Int] -> [Int]
initialCompact rootPC pcs =
  let rootMod = rootPC `mod` 12
      -- Place root in target octave (12-23)
      bassPos = rootMod + targetOctaveMin
      -- Get other pitch classes
      otherPCs = filter (\p -> p `mod` 12 /= rootMod) (map (`mod` 12) pcs)
      -- Stack each above bass: find lowest placement > bass within bounds
      stackAbove bass pc = 
        let placements = pitchPlacements pc
            valid = filter (> bass) placements
        in if null valid 
           then bass + ((pc - rootMod + 12) `mod` 12)  -- Fallback
           else minimum valid
      uppers = map (stackAbove bassPos) otherPCs
  in sort (bassPos : uppers)

-------------------------------------------------------------------------------
-- Cyclic Dynamic Programming Solver
-------------------------------------------------------------------------------

-- |DP state: maps candidate index to (min cost to reach, previous index)
type DPState = Map Int (Int, Int)

-- |Solve voice leading using cyclic DP.
-- Finds globally optimal voicings minimizing total cyclic cost.
--
-- Parameters:
--   * filterCandidates: function to filter candidates (e.g., root-only for solveRoot)
--   * rootPCs: root pitch class for each chord (extracted from bass of input)
--   * chords: input chords (pitch classes)
solveCyclicDP :: (Int -> [[Int]] -> [[Int]]) -> [Int] -> [[Int]] -> [[Int]]
solveCyclicDP _ _ [] = []
solveCyclicDP _ _ [x] = [initialCompact (head x `mod` 12) x]
solveCyclicDP filterCandidates rootPCs chords =
  let n = length chords
      
      -- Generate candidates for each position
      -- Position 0 is fixed to initial compact voicing
      firstRootPC = head rootPCs
      firstVoicing = initialCompact firstRootPC (head chords)
      
      -- For positions 1..n-1, generate all candidates filtered appropriately
      -- Type: [[[Int]]] -- list of positions, each containing list of candidate voicings
      candidatesPerPos :: [[[Int]]]
      candidatesPerPos = [[firstVoicing]] ++ 
        [filterCandidates (rootPCs !! i) (allVoicings (chords !! i)) | i <- [1..n-1]]
      
      getCandidates :: Int -> [[Int]]
      getCandidates i = candidatesPerPos !! i
      
      numCandidates :: Int -> Int
      numCandidates i = length (getCandidates i)
      
      -- Initial state: position 0, only candidate 0 (the fixed first voicing)
      initialDP :: DPState
      initialDP = Map.singleton 0 (0, -1)
      
      -- Forward pass: for each position, compute min cost to reach each candidate
      forwardPass :: [DPState]
      forwardPass = scanl stepDP initialDP [1..n-1]
        where
          stepDP :: DPState -> Int -> DPState
          stepDP prevState pos =
            let prevCands = getCandidates (pos - 1)
                currCands = getCandidates pos
                -- For each current candidate, find best predecessor
                computeBest :: Int -> Maybe (Int, (Int, Int))
                computeBest currIdx =
                  let currVoicing = currCands !! currIdx
                      -- Try each predecessor
                      costs = [(prevIdx, cost + voiceLeadingCost prevVoicing currVoicing, prevIdx)
                              | (prevIdx, (cost, _)) <- Map.toList prevState
                              , let prevVoicing = prevCands !! prevIdx]
                  in if null costs 
                     then Nothing
                     else let (_, minCost, backPtr) = minimumBy (compare `on` (\(_,c,_) -> c)) costs
                          in Just (currIdx, (minCost, backPtr))
            in Map.fromList $ catMaybes [computeBest j | j <- [0 .. length currCands - 1]]
      
      -- Get final DP state
      finalState :: DPState
      finalState = last forwardPass
      
      -- Add wrap-around cost and find best ending
      lastCands = getCandidates (n - 1)
      bestEnding :: (Int, Int)  -- (candidate index, total cyclic cost)
      bestEnding = minimumBy (compare `on` snd) 
        [(lastIdx, cost + voiceLeadingCost (lastCands !! lastIdx) firstVoicing)
        | (lastIdx, (cost, _)) <- Map.toList finalState]
      
      -- Backtrack to reconstruct path
      backtrack :: [DPState] -> Int -> Int -> [Int]
      backtrack states pos candIdx
        | pos == 0 = [candIdx]
        | otherwise = 
          let (_, backPtr) = (states !! pos) Map.! candIdx
          in backtrack states (pos - 1) backPtr ++ [candIdx]
      
      path = backtrack forwardPass (n - 1) (fst bestEnding)
      
      -- Convert path to voicings
      result = [getCandidates i !! (path !! i) | i <- [0..n-1]]
      
  in result

-- |Solve with root always in bass (root paradigm).
-- Filters candidates at each position to only those where bass note mod 12 == root PC.
-- Result is normalized so first chord's root is in [7,18].
solveRoot :: [[Int]] -> [[Int]]
solveRoot [] = []
solveRoot chords =
  let rootPCs = map (\c -> head c `mod` 12) chords
      filterByRoot rootPC cands = 
        let valid = filter (\v -> head v `mod` 12 == rootPC) cands
        in if null valid then cands else valid  -- Fallback if filtering removes all
      solved = solveCyclicDP filterByRoot rootPCs chords
  in normalizeByFirstRoot solved

-------------------------------------------------------------------------------
-- Paradigm Solvers
-------------------------------------------------------------------------------

-- |Solve FLOW paradigm using cyclic DP:
-- Smoothest voice leading with any inversion allowed.
-- Voice crossings permitted for optimal smoothness.
-- First chord starts compact with root in bass.
-- Result is normalized so first chord's root is in [7,18].
solveFlow :: [[Int]] -> [[Int]]
solveFlow [] = []
solveFlow chords =
  let rootPCs = map (\c -> head c `mod` 12) chords
      noFilter _ cands = cands  -- No filtering, all inversions allowed
      solved = solveCyclicDP noFilter rootPCs chords
  in normalizeByFirstRoot solved

-- |LITE paradigm: Literal voicing with no optimization.
-- Takes raw pitch class lists and normalizes them.
-- Normalized so first chord's root is in [7,18].
-- Use this for comparing raw pitch classes against optimized voicings.
liteVoicing :: [[Int]] -> [[Int]]
liteVoicing [] = []
liteVoicing raw = normalizeByFirstRoot raw

-- |BASS paradigm: Bass note only (root pitch class per chord).
-- Extracts the root note (first element, mod 12) from each chord.
-- Returns as single-element lists in [0,11] range.
-- Use this for bass line extraction from voiced progressions.
bassVoicing :: [[Int]] -> [[Int]]
bassVoicing [] = []
bassVoicing chords = map (\chord -> [head chord `mod` 12]) chords

-------------------------------------------------------------------------------
-- Post-processing
-------------------------------------------------------------------------------

-- |Normalize a progression so first chord's root is in [7,18] (G0 to F#1).
-- This ensures consistent output ranges across all progressions regardless
-- of key or where the solver explored during optimization.
--
-- Strategy: Calculate shift needed to place first chord's root (bass note)
-- into [targetFirstRootMin, targetFirstRootMax], then apply to all pitches.
normalizeByFirstRoot :: [[Int]] -> [[Int]]
normalizeByFirstRoot [] = []
normalizeByFirstRoot voicings =
  let firstChord = head voicings
      firstRoot = head firstChord  -- Bass note of first chord
      firstRootPC = firstRoot `mod` 12
      
      -- Calculate the current octave of the first root
      currentOctave = firstRoot `div` 12
      
      -- Find target octave: we want firstRootPC + targetOctave * 12 to be in [7,18]
      -- targetFirstRootMin = 7, targetFirstRootMax = 18
      -- For pc in [0,6]: target octave 1 gives [12,18], all valid
      -- For pc in [7,11]: target octave 0 gives [7,11], all valid
      targetOctave = if firstRootPC >= 7 then 0 else 1
      targetRoot = firstRootPC + targetOctave * 12
      
      -- Shift amount: how much to add/subtract to move firstRoot to targetRoot
      shift = targetRoot - firstRoot
      
  in map (map (+ shift)) voicings
