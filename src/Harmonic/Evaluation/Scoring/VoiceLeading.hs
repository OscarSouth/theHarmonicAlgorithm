-- |
-- Module      : Harmonic.Evaluation.Scoring.VoiceLeading
-- Description : Cyclic DP voice leading optimization
-- 
-- Part of the Evaluation (E) component of the Creative Systems Framework:
-- voice-leading cost scores the quality of movement between sonorities. It
-- does not run in the per-step generation loop — it enters evaluation at the
-- whole-progression level ("Harmonic.Evaluation.Scoring.Progression", used
-- by @attempt@) and is applied by the Tidal Arranger when realising voicings.
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
--    Candidate pitches live in [7, 35] (pitchPlacements over [minPitch,
--    maxPitch] bounds; the effective ceiling is 35 = 11+24). First chord
--    starts in compact root position, and the whole result is shifted so
--    the first root lands in [-12, -1].
--
-- 4. Two DP Paradigms (plus extractors and the chroma engine):
--    * solveRoot (grid): smooth, compact voice leading, root always in bass
--    * solveFlow (flow): smooth, compact voice leading, any inversion

module Harmonic.Evaluation.Scoring.VoiceLeading
  ( -- * Cost Functions
    voiceLeadingCost
  , alignVoices
  , totalCost
  , cyclicCost
  
    -- * Voice Movement Calculation
  , voiceMovement
  , minimalMovement
  
    -- * Candidate Generation
  , allVoicings
  , pitchPlacements
  , initialCompact
  
    -- * Paradigm Solvers (Cyclic DP)
  , solveRoot
  , solveFlow
  , liteVoicing
  , bassVoicing
  
    -- * Post-processing
  , normalizeByFirstRoot
  ) where

import Data.List (sort, nub, minimumBy)
import qualified Data.List as List
import Data.Function (on)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Harmonic.Rules.Types.Pitch (PitchClass(..))

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

-- | Minimum allowed pitch for exploration (exploration floor)
-- Allows bass to descend up to a 5th below target octave
minPitch :: Int
minPitch = 7

-- | Maximum allowed pitch for exploration (exploration ceiling).
-- Note: never binds in practice — 'pitchPlacements' only generates
-- pc, pc+12, pc+24, so the effective ceiling is 35 (= 11 + 24).
maxPitch :: Int
maxPitch = 41

-- | Target octave lower bound: 'initialCompact' places bar 0's bass at
-- rootPC + this, i.e. in [12, 23].
targetOctaveMin :: Int
targetOctaveMin = 12

-- | Post-normalization anchor: 'normalizeByFirstRoot' shifts the whole
-- progression so bar 0's bass lands at its pitch class + this, i.e. in
-- [-12, -1].
targetFirstRootMin :: Int
targetFirstRootMin = -12

-------------------------------------------------------------------------------
-- Voice Movement Calculation
-------------------------------------------------------------------------------

-- |Calculate the movement for a single voice between two concrete pitches.
-- Movement is measured in absolute semitones (not mod 12 since we're
-- working with concrete pitches in the [0,36] range).
voiceMovement :: Int -> Int -> Int
voiceMovement from to = abs (to - from)
{-# INLINE voiceMovement #-}

-- |Calculate minimal movement between two pitch class values (mod 12).
-- Exported API; currently unused inside the engine (kept for REPL and
-- downstream use).
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
--   * Parallel penalty: +3 for each parallel perfect 5th \/ octave between
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
-- rewarded and penalised. They also differ in pair scope by design:
-- register exchange scans ADJACENT pairs only (a register swap is a
-- neighbouring-voices phenomenon), while contrary motion rewards ANY pair.
--
-- The total is floored at 1 for any actual motion (from /= to): bonuses
-- could otherwise exceed base + penalties and drive a moving transition
-- below the held-chord cost of 0 — inverting the musical preference. A
-- held chord ("available static movement") is always strictly cheapest;
-- bonuses still discriminate among positive-cost alternatives. The floor
-- applies to the TOTAL only — component bonuses stay un-clamped inside
-- the sum (e.g. [0,4,7]→[-1,2,7] = base 3 + stepwise −1 = 2).
-- Cross-cardinality transitions (mixed-set seams: lead' cues, hand-built
-- fromChords material) are costed by optimal monotone padding: the smaller
-- sorted voicing is expanded by duplicating tones per the minimal-distance
-- non-crossing alignment in which every voice of BOTH chords participates
-- ('alignVoices'), then the full cost above runs verbatim on the aligned
-- pair. An extra voice pays exactly its distance to the tone it splits
-- from (a literal unison doubling is free); no voice can appear or vanish
-- unpenalised. Historical note: this branch was a flat 999 sentinel,
-- which in the DP acted as "ignore this edge" — seam registers were
-- decided by downstream edges alone and the cyclic wrap objective was
-- silently disabled on mixed material.
voiceLeadingCost :: [Int] -> [Int] -> Int
voiceLeadingCost from to
  | null from || null to = 0        -- Empty bar: neutral edge
  | length from /= length to =
      let (from', to') = alignVoices from to
      in voiceLeadingCost from' to'
  | from == to = 0                  -- Identical voicings have zero cost
  | otherwise =
      max 1 (baseCost + parallelPenalty + leapPenalty
               + registerExchangePenalty + contraryBonus + stepwiseBonus)
  where
    movements   = zipWith voiceMovement from to
    signedMoves = zipWith (-) to from

    -- Per-voice tuples traversed once; pair scans use tails\/zip rather
    -- than repeated list indexing (the old `!!`-based loops made each
    -- call effectively O(n³) — a real cost at 36–432 DP candidates\/bar).
    voiceData   = zip3 from to signedMoves
    pairsAll    = [ (a, b) | (a : rest) <- List.tails voiceData, b <- rest ]
    pairsAdj    = zip voiceData (drop 1 voiceData)

    baseCost = sum movements

    -- Parallel perfect intervals (P5 = 7, P8 = 0) between ANY voice pair.
    -- The interval is preserved across the transition AND at least one
    -- voice moves (purely held intervals don't count).
    isPerfect ivl = ivl == 7 || ivl == 0
    isParallelPerfect ((f1, t1, s1), (f2, t2, s2)) =
      let fromInt = (f2 - f1) `mod` 12
          toInt   = (t2 - t1) `mod` 12
      in isPerfect fromInt && fromInt == toInt && (s1 /= 0 || s2 /= 0)
    parallelPenalty = 3 * length (filter isParallelPerfect pairsAll)

    -- Per-voice penalty for movements > 4 semitones (anything above a P4).
    leapPenalty = 2 * length (filter (> 4) movements)

    -- Adjacent split-leap detection: both voices leap ≥5 in opposite directions.
    isExchange ((_, _, s1), (_, _, s2)) =
      s1 * s2 < 0 && abs s1 >= 5 && abs s2 >= 5
    registerExchangePenalty = 4 * length (filter isExchange pairsAdj)

    -- Contrary motion: any voice pair, both moving ≤4 in opposite directions
    -- (smooth divergence). Disjoint from register exchange by magnitude.
    isContrary ((_, _, s1), (_, _, s2)) =
      s1 * s2 < 0 && abs s1 <= 4 && abs s2 <= 4
    contraryBonus = (-1) * length (filter isContrary pairsAll)

    -- Stepwise: per voice with movement of 1 or 2 semitones, bonus only
    -- when ≥2 voices step (preserves >=1 floor for single-voice steps).
    stepwiseBonus =
      let stepCount = length (filter (\m -> m == 1 || m == 2) movements)
      in if stepCount >= 2 then -(stepCount - 1) else 0

-- |Optimal monotone padding for cross-cardinality voice leading.
-- Given two SORTED voicings of different lengths, expands the smaller by
-- duplicating tones so both have the larger's length, choosing the
-- duplication per the minimal-total-|distance| monotone (non-crossing)
-- alignment in which every voice of BOTH chords participates: each voice
-- of the larger maps to exactly one voice of the smaller, the mapping is
-- non-decreasing, and every smaller voice is used at least once. This is
-- the "lowest \/ middle \/ highest voices of the larger lead the smaller"
-- intuition: a 5-note chord resolves into a triad through its outer and
-- inner voices, and the doubled tones pay exactly their split distance.
-- O(m·n) DP (≤ 49 cells at max cardinality 7). Symmetric in its result
-- (roles of from\/to only decide which side gets padded). Equal-length
-- input is returned unchanged.
alignVoices :: [Int] -> [Int] -> ([Int], [Int])
alignVoices from to
  | length from == length to = (from, to)
  | length from <  length to = (padTo from to, to)
  | otherwise                = (from, padTo to from)
  where
    -- Expand @small@ (length m) to @big@'s length n by optimal monotone
    -- assignment big!!j -> small!!(a j), a non-decreasing surjection.
    padTo small big =
      let m    = length small
          n    = length big
          s    = V.fromList small
          b    = V.fromList big
          huge = 10 ^ (9 :: Int)
          dist j i = abs (b V.! j - s V.! i)
          -- rows !! j: at each small-index i, (min cost with big j -> small i,
          -- predecessor small-index at big (j-1))
          row0 = V.generate m (\i -> if i == 0 then (dist 0 0, 0) else (huge, i))
          step prev j = V.generate m $ \i ->
            let stayC = fst (prev V.! i)
                diagC = if i > 0 then fst (prev V.! (i - 1)) else huge
                best  = min stayC diagC
                prevI = if diagC < stayC then i - 1 else i
            in if best >= huge then (huge, i) else (dist j i + best, prevI)
          rows = scanl step row0 [1 .. n - 1]
          backtrack j i acc
            | j == 0    = i : acc
            | otherwise = let (_, prevI) = (rows !! j) V.! i
                          in backtrack (j - 1) prevI (i : acc)
          assignment = backtrack (n - 1) (m - 1) []
      in map (s V.!) assignment

-- |Calculate total voice leading cost for a sequence of chords
totalCost :: [[Int]] -> Int
totalCost [] = 0
totalCost [_] = 0
totalCost chords = sum $ zipWith voiceLeadingCost chords (tail chords)

-- |Calculate cyclic cost: total cost including wrap-around from last to first.
-- This is essential for loop-aware optimization.
--
-- From the evaluation document:
--   "Adding wrap-around cost to the voice leading solver solves the @drift@
--    issue elegantly. It forces the algorithm to find a path that is not
--    just locally optimal, but topologically closed."
cyclicCost :: [[Int]] -> Int
cyclicCost [] = 0
cyclicCost [_] = 0
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

-- |Generate all valid voicings for a pitch-class set of any cardinality.
-- Each pitch class is placed at its 'pitchPlacements' octaves (PCs 0-6
-- get 2 placements in [12, 30]; PCs 7-11 get 3 in [7, 35]) — candidate
-- count is the per-PC placement product (2^a·3^b: typically 12 for a
-- triad, 36 for a tetrad, 72 for a 5-PC set), NOT 3^N. The key-dependent
-- window asymmetry is a known calibration; widening it changes every
-- solved voicing globally and is deferred behind a before\/after
-- listening protocol.
-- Results are sorted low-to-high and deduplicated.
--
-- Historical note: this was hard-coded to 3 notes, with non-triads falling
-- back to the single candidate @[sort pcs]@ pinned in octave [0,11] — which
-- collapsed bars 2..n of any multi-bar 4+-note progression ~2 octaves below
-- bar 1 under 'Harmonic.Interface.Tidal.Arranger.flow'/'Harmonic.Interface.Tidal.Arranger.grid' (bar 1 goes through 'initialCompact', the rest
-- through here, then 'normalizeByFirstRoot' applies one uniform shift).
allVoicings :: [Int] -> [[Int]]
allVoicings pcs
  | null pcs = [[]]
  | otherwise =
    let placements = map (pitchPlacements . (`mod` 12)) pcs
        -- Cartesian product of per-note octave placements
        allCombos = sequence placements
        -- Sort each voicing low-to-high, deduplicate via Set (O(n log n) vs nub's O(n²))
        sorted = map sort allCombos
    in Set.toList (Set.fromList sorted)

-- |Create initial compact voicing: root in bass in target octave (12-23),
-- upper voices stacked compactly above.
initialCompact :: Int -> [Int] -> [Int]
initialCompact _ [] = []
initialCompact rootPC pcs =
  let rootMod = rootPC `mod` 12
      -- Place root in target octave (12-23)
      bassPos = rootMod + targetOctaveMin
      -- Get other pitch classes
      otherPCs = filter (\p -> p `mod` 12 /= rootMod) (map (`mod` 12) pcs)
      -- Stack each above bass: find lowest placement > bass.
      -- `filter (> bass)` is never empty: bass <= 23 < 24 <= pc+24, and
      -- pc+24 is always in pitchPlacements' output.
      stackAbove bass pc = minimum (filter (> bass) (pitchPlacements pc))
      uppers = map (stackAbove bassPos) otherPCs
  in sort (bassPos : uppers)

-------------------------------------------------------------------------------
-- Cyclic Dynamic Programming Solver
-------------------------------------------------------------------------------

-- |DP state: maps candidate index to (min cost to reach, previous index)
type DPState = Map Int (Int, Int)

-- |Solve voice leading using cyclic DP.
-- Finds globally optimal voicings minimizing total cyclic cost — the
-- wrap edge (last bar -> bar 0) is part of the objective, so loops
-- close smoothly.
--
-- Bar 0 is DELIBERATELY pinned to 'initialCompact' (compact root
-- position): the published contract is a predictable starting register,
-- so the optimum is conditional on that anchor rather than searched
-- over bar-0 voicings.
--
-- Tie-breaking: 'minimumBy' keeps the first minimum and candidates are
-- in ascending (lowest-register-first) order, so exact ties resolve to
-- the lowest-register choice.
--
-- Parameters:
--   * filterCandidates: function to filter candidates (e.g., root-only for solveRoot)
--   * rootPCs: root pitch class for each chord (extracted from bass of input)
--   * chords: input chords (pitch classes)
solveCyclicDP :: (Int -> [[Int]] -> [[Int]]) -> [Int] -> [[Int]] -> [[Int]]
solveCyclicDP _ _ [] = []
solveCyclicDP _ _ [x] = [initialCompact (bassPC x) (dedupPCs x)]
solveCyclicDP filterCandidates rootPCs rawChords =
  let -- Canonical dedup: pitch-class sets carry no duplicates. Keeps the
      -- first occurrence so the root stays at head, and keeps
      -- 'initialCompact' (which drops duplicate roots) and 'allVoicings'
      -- (which would keep them) at the same cardinality — a duplicated
      -- root PC previously guaranteed a cross-cardinality edge out of
      -- bar 0.
      chords = map dedupPCs rawChords
      n = length chords
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
-- Result is normalized so first chord's root is in [-12,-1].
solveRoot :: [[Int]] -> [[Int]]
solveRoot [] = []
solveRoot chords =
  let rootPCs = map bassPC chords
      -- The fallback below is provably unreachable for non-empty chords: a
      -- root-in-bass candidate always exists, because every non-root PC
      -- has a placement at pc+24 >= 24, above any minimum root placement
      -- (<= 23). Kept as free defense rather than a crash surface.
      filterByRoot rootPC cands =
        let valid = filter (\v -> bassPC v == rootPC) cands
        in if null valid then cands else valid
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
  let rootPCs = map bassPC chords
      noFilter _ cands = cands  -- No filtering, all inversions allowed
      solved = solveCyclicDP noFilter rootPCs chords
  in normalizeByFirstRoot solved

-- |LITE paradigm: Literal voicing with no optimization.
-- Takes raw pitch class lists and normalizes them.
-- Normalized so first chord's root is in [-12,-1].
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
bassVoicing chords = map (\chord -> if null chord then [] else [bassPC chord]) chords

-------------------------------------------------------------------------------
-- Post-processing
-------------------------------------------------------------------------------

-- |Normalize a progression by a single uniform shift placing the first
-- chord's root (bass note) at @firstRootPC + targetFirstRootMin@ — i.e.
-- into [-12, -1]. One constant transposition for the whole progression:
-- consistent output register regardless of key or where the solver
-- explored. (In practice the shift is always -24 after the DP solvers,
-- whose bar 0 is 'initialCompact' in [12, 23], and -12 for
-- 'liteVoicing', whose raw input has its root in [0, 11].)
normalizeByFirstRoot :: [[Int]] -> [[Int]]
normalizeByFirstRoot [] = []
normalizeByFirstRoot voicings@(firstChord : _)
  | null firstChord = voicings  -- empty first bar: nothing to anchor on
  | otherwise =
      let firstRoot   = head firstChord
          firstRootPC = firstRoot `mod` 12
          targetRoot  = firstRootPC + targetFirstRootMin
          shift       = targetRoot - firstRoot
      in map (map (+ shift)) voicings

-- |Bass pitch class of a voicing (0 for an empty bar — degrades, never crashes).
bassPC :: [Int] -> Int
bassPC []      = 0
bassPC (p : _) = p `mod` 12

-- |Duplicate-free pitch classes, first occurrence kept (root stays at head).
dedupPCs :: [Int] -> [Int]
dedupPCs = nub . map (`mod` 12)
