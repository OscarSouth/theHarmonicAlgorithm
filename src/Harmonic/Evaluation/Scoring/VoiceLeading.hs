{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Harmonic.Evaluation.Scoring.VoiceLeading
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

module Harmonic.Evaluation.Scoring.VoiceLeading
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

import Data.List (sort, minimumBy)
import Data.Function (on)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import qualified Data.Vector as V
import GHC.Generics (Generic)

import Harmonic.Rules.Types.Pitch (PitchClass(..), mkPitchClass, unPitchClass, transpose)

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

-- | Target range for first chord's root (lower bound, C3)
targetFirstRootMin :: Int
targetFirstRootMin = -12

-- | Target range for first chord's root (upper bound, B3)
targetFirstRootMax :: Int
targetFirstRootMax = -1

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
--
-- Cost components:
--   * Base: sum of absolute MIDI movements per voice.
--   * Parallel penalty: +3 for each parallel perfect 5th / octave between
--     ANY voice pair (not just adjacent), when at least one voice moves.
--   * Large leap penalty: +2 per voice moving > 4 semitones.
--   * Register-exchange penalty: +4 per adjacent voice pair where both
--     voices move ≥5 semitones in opposite directions (split-leap pattern
--     producing register inversion). Note: not classical "voice crossing"
--     — sorted MIDI voicings have no voice identity to cross — but the
--     same musical effect of register-swapping leaps.
--   * Contrary motion bonus: −1 per voice pair (any pair) where both
--     voices move ≤4 semitones in opposite directions (modest divergence).
--   * Stepwise motion bonus: −1 per stepping voice (movement ∈ {1, 2})
--     when ≥2 voices step. Single-voice steps contribute 0.
--
-- Magnitudes calibrated to compose: contrary motion and register exchange
-- are deliberately disjoint by magnitude (≤4 vs ≥5 thresholds), aligning
-- with the leap-penalty trigger so the same motion is never both
-- rewarded and penalised.
voiceLeadingCost :: [Int] -> [Int] -> Int
voiceLeadingCost from to
  | length from /= length to = 999  -- Incompatible voicings
  | from == to = 0                  -- Identical voicings have zero cost
  | otherwise =
      baseCost + parallelPenalty + leapPenalty
               + registerExchangePenalty + contraryBonus + stepwiseBonus
  where
    n           = length from
    movements   = zipWith voiceMovement from to
    signedMoves = zipWith (-) to from
    allPairs    = [(i, j) | i <- [0 .. n - 2], j <- [i + 1 .. n - 1]]
    adjPairs    = [(i, i + 1) | i <- [0 .. n - 2]]

    baseCost = sum movements

    -- Parallel perfect intervals (P5 = 7, P8 = 0) between ANY voice pair.
    -- The interval is preserved across the transition AND at least one
    -- voice moves (purely held intervals don't count).
    isPerfect ivl = ivl == 7 || ivl == 0
    isParallelPerfect (i, j) =
      let fromInt = (from !! j - from !! i) `mod` 12
          toInt   = (to   !! j - to   !! i) `mod` 12
      in isPerfect fromInt && fromInt == toInt
         && (movements !! i > 0 || movements !! j > 0)
    parallelPenalty = 3 * length (filter isParallelPerfect allPairs)

    -- Per-voice penalty for movements > 4 semitones (anything above a P4).
    leapPenalty = 2 * length (filter (> 4) movements)

    -- Adjacent split-leap detection: both voices leap ≥5 in opposite directions.
    isExchange (i, j) =
      let mi = signedMoves !! i
          mj = signedMoves !! j
      in mi * mj < 0 && abs mi >= 5 && abs mj >= 5
    registerExchangePenalty = 4 * length (filter isExchange adjPairs)

    -- Contrary motion: any voice pair, both moving ≤4 in opposite directions
    -- (smooth divergence). Disjoint from register exchange by magnitude.
    isContrary (i, j) =
      let mi = signedMoves !! i
          mj = signedMoves !! j
      in mi * mj < 0 && abs mi <= 4 && abs mj <= 4
    contraryBonus = (-1) * length (filter isContrary allPairs)

    -- Stepwise: per voice with movement of 1 or 2 semitones, bonus only
    -- when ≥2 voices step (preserves >=1 floor for single-voice steps).
    stepwiseBonus =
      let stepCount = length (filter (\m -> m == 1 || m == 2) movements)
      in if stepCount >= 2 then -(stepCount - 1) else 0

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
        -- Sort each voicing low-to-high, deduplicate via Set (O(n log n) vs nub's O(n²))
        sorted = map sort allCombos
    in Set.toList (Set.fromList sorted)

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
      rootPCsV = V.fromList rootPCs
      chordsV = V.fromList chords

      -- Generate candidates for each position
      -- Position 0 is fixed to initial compact voicing
      firstRootPC = V.head rootPCsV
      firstVoicing = initialCompact firstRootPC (V.head chordsV)

      -- For positions 1..n-1, generate all candidates filtered appropriately
      -- Vector of Vectors for O(1) indexing
      candidatesPerPos :: V.Vector (V.Vector [Int])
      candidatesPerPos = V.generate n $ \i ->
        if i == 0
        then V.singleton firstVoicing
        else V.fromList $ filterCandidates (rootPCsV V.! i) (allVoicings (chordsV V.! i))

      getCandidates :: Int -> V.Vector [Int]
      getCandidates i = candidatesPerPos V.! i

      -- Initial state: position 0, only candidate 0 (the fixed first voicing)
      initialDP :: DPState
      initialDP = Map.singleton 0 (0, -1)

      -- Forward pass: for each position, compute min cost to reach each candidate
      forwardPass :: V.Vector DPState
      forwardPass = V.fromList $ scanl stepDP initialDP [1..n-1]
        where
          stepDP :: DPState -> Int -> DPState
          stepDP prevState pos =
            let prevCands = getCandidates (pos - 1)
                currCands = getCandidates pos
                -- For each current candidate, find best predecessor
                computeBest :: Int -> Maybe (Int, (Int, Int))
                computeBest currIdx =
                  let currVoicing = currCands V.! currIdx
                      -- Try each predecessor
                      costs = [(prevIdx, cost + voiceLeadingCost prevVoicing currVoicing, prevIdx)
                              | (prevIdx, (cost, _)) <- Map.toList prevState
                              , let prevVoicing = prevCands V.! prevIdx]
                  in if null costs
                     then Nothing
                     else let (_, minCost, backPtr) = minimumBy (compare `on` (\(_,c,_) -> c)) costs
                          in Just (currIdx, (minCost, backPtr))
            in Map.fromList $ catMaybes [computeBest j | j <- [0 .. V.length currCands - 1]]

      -- Get final DP state
      finalState :: DPState
      finalState = V.last forwardPass

      -- Add wrap-around cost and find best ending
      lastCands = getCandidates (n - 1)
      bestEnding :: (Int, Int)  -- (candidate index, total cyclic cost)
      bestEnding = minimumBy (compare `on` snd)
        [(lastIdx, cost + voiceLeadingCost (lastCands V.! lastIdx) firstVoicing)
        | (lastIdx, (cost, _)) <- Map.toList finalState]

      -- Backtrack to reconstruct path (accumulator-passing, O(n))
      backtrack :: Int -> Int -> [Int] -> [Int]
      backtrack pos candIdx acc
        | pos == 0 = candIdx : acc
        | otherwise =
          let (_, backPtr) = (forwardPass V.! pos) Map.! candIdx
          in backtrack (pos - 1) backPtr (candIdx : acc)

      path = V.fromList $ backtrack (n - 1) (fst bestEnding) []

      -- Convert path to voicings
      result = [getCandidates i V.! (path V.! i) | i <- [0..n-1]]

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
-- Smoothest voice leading with any inversion allowed for bars 1..n-1.
-- Bar 0 is anchored to the compact root-position voicing
-- ('initialCompact') so the progression's starting register is
-- predictable and 'normalizeByFirstRoot' has a stable anchor.
-- Voice crossings permitted in subsequent bars for optimal smoothness.
-- Result is normalized so first chord's root is in [-12, -1].
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
      
      -- Find target octave: we want firstRootPC + targetOctave * 12 to be in [-12,-1]
      -- targetFirstRootMin = -12 (C3), targetFirstRootMax = -1 (B3)
      -- targetOctave = -1 places all pitch classes in [-12,-1]
      targetOctave = (-1)
      targetRoot = firstRootPC + targetOctave * 12
      
      -- Shift amount: how much to add/subtract to move firstRoot to targetRoot
      shift = targetRoot - firstRoot
      
  in map (map (+ shift)) voicings
