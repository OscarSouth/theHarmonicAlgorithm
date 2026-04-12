{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Framework.Builder.Core
-- Description : Core generation engine for harmonic progressions
--
-- Internal chain building, candidate pool construction, R-constraint filtering,
-- consonance fallback generation, state advancement, and progression conversion.
-- These functions run inside the Bolt action monad for Neo4j access.

module Harmonic.Framework.Builder.Core
  ( -- * Chain Building (online, requires Neo4j)
    buildChain
  , buildChainWithDiag
  , buildChainWithDiagV

    -- * Chain Building (offline, no Neo4j required)
  , buildChainOffline
  , buildChainOfflineWithDiag
  , buildChainOfflineWithDiagV

    -- * Conversion
  , chainToProgression
  , extractCadence

    -- * Filtering (exposed for testing)
  , matchesContext
  , matchesContextWithTarget
  , applyDriftFilter
  ) where

import qualified Database.Bolt as Bolt
import qualified Data.Text as T
import qualified Data.IntSet as IntSet
import           Control.Monad (foldM)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (sortBy)
import           Data.Function (on)
import           Data.Ord (Down(..))
import           System.Random.MWC (GenIO, createSystemRandom)
import qualified System.Random.MWC.Distributions as Dist

import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as P
import qualified Harmonic.Rules.Types.Progression as Prog
import           Harmonic.Evaluation.Database.Query (ComposerWeights, fetchTransitions)
import qualified Harmonic.Evaluation.Database.Query as Q
import           Harmonic.Traversal.Probabilistic (gammaIndexScaledWith)
import           Harmonic.Rules.Constraints.Filter (parseOvertones', parseKey, isWildcard, resolveRoots,
                                                    nthAbove, nthBelow)
import           Harmonic.Rules.Constraints.Overtone (overtoneSets)
import           Harmonic.Evaluation.Scoring.Dissonance (dissonanceScore)
import qualified Harmonic.Evaluation.Scoring.Dissonance as D

import           Harmonic.Framework.Builder.Types
import           Harmonic.Framework.Builder.Diagnostics (computeChordTrace)

-------------------------------------------------------------------------------
-- Chain Building (Inside Bolt Action)
-------------------------------------------------------------------------------

-- |Build the cadence chain step by step.
--
-- Simplified algorithm:
--   1. Start from initial CadenceState
--   2. For each step: build candidate pool, gamma-select next
--   3. Pool = filtered graph transitions + consonanceFallback (up to poolSize)
buildChain :: GeneratorConfig
           -> GenIO            -- ^ Shared random generator
           -> Double           -- ^ Entropy [0,1]
           -> HarmonicContext
           -> ParsedContext    -- ^ Pre-parsed context for O(1) lookups
           -> ComposerWeights  -- ^ Composer blend weights
           -> H.CadenceState   -- ^ Starting state
           -> Int              -- ^ Number of steps to generate
           -> Bolt.BoltActionT IO [H.CadenceState]
buildChain config gen ent context pctx composerWeights start totalSteps = do
  let initCounter = if H.isInversion (H.stateCadence start) then 0 else 1
  ((_current, revChain, _counter), _noDiags) <-
    foldM (stepChainCore config gen Nothing ent context pctx composerWeights)
          ((start, [start], initCounter), [])
          [1..totalSteps]
  pure $ reverse revChain

-- |Core body for a single chain-building step (plain IO, no Bolt dependency).
--
-- Takes pre-fetched transitions and executes the full filtering/scoring/selection logic.
-- Used by both the online Bolt wrapper ('stepChainCore') and offline path ('stepChainOffline').
-- When transitions is empty (offline mode), generation relies entirely on consonanceFallback.
stepChainBody :: GeneratorConfig
              -> GenIO
              -> Maybe Int        -- ^ Nothing = no diagnostics, Just n = verbosity level
              -> Double           -- ^ Entropy [0,1]
              -> HarmonicContext
              -> ParsedContext
              -> ComposerWeights
              -> ((H.CadenceState, [H.CadenceState], Int), [StepDiagnostic])
              -> Int
              -> [(H.Cadence, ComposerWeights)]   -- ^ Pre-fetched transitions (empty for offline)
              -> IO ((H.CadenceState, [H.CadenceState], Int), [StepDiagnostic])
stepChainBody _config gen mVerbosity ent _context pctx composerWeights ((current, revChain, nonInvCount), revDiags) stepNum transitions = do
  -- Compute bass direction target (if rise/fall is active)
  let prevBassPC = P.unPitchClass (P.pitchClass (H.stateCadenceRoot current))
      bassTarget = case pcBassDirection pctx of
        Nothing       -> Nothing
        Just (Rise n) -> Just $ nthAbove n prevBassPC (pcAllowedBassNotes pctx)
        Just (Fall n) -> Just $ nthBelow n prevBassPC (pcAllowedBassNotes pctx)

  -- Apply R constraints (pure filter by ParsedContext)
  let filtered = applyRConstraintsWithTarget bassTarget pctx current transitions

  -- Score by composer blend
  -- Filter to confidence > 0 and sort highest first
  let scored = scoreByConfidence composerWeights filtered
      graphCandidates = scored  -- No pool limit: use all candidates
      graphCount = length graphCandidates

  -- Build candidate pool: graph candidates + consonanceFallback
  -- NO POOL SIZE LIMIT - use full 660-candidate fallback generation
  fallbackAll <- consonanceFallbackParsed gen current pctx
  -- Apply R constraints to fallback candidates (same as graph candidates)
  let unfilteredFallback = [(cad, score) | (cad, score, _, _, _) <- fallbackAll]
      filteredFallback = filter (\(cad, _) -> matchesContextWithTarget bassTarget pctx current cad) unfilteredFallback
      fallbackCount = length filteredFallback
      -- Create unified pool with (Cadence, score) format
      -- Graph candidates first (preserves database priority), then filtered fallback
      pool = graphCandidates ++ filteredFallback

      -- Apply dissonance drift filter
      driftedPool = applyDriftFilter (pcDrift pctx) current pool

      -- Apply inversion spacing constraint
      invSpacing = pcInversionSpacing pctx
      inversionAllowed = nonInvCount >= invSpacing
      spacedPool = if inversionAllowed
                   then driftedPool
                   else filter (not . H.isInversion . fst) driftedPool
      prepedalPool = if null spacedPool then driftedPool else spacedPool
      finalPool = applyPedalFilter pctx current prepedalPool

  -- Select next cadence using gamma sampling
  if null finalPool
    then do
      -- Absorbing state
      let diags = case mVerbosity of
            Nothing -> revDiags
            Just _  ->
              let diag = StepDiagnostic
                    { sdStepNumber = stepNum
                    , sdPriorCadence = show (extractCadence current)
                    , sdPriorRoot = show (H.stateCadenceRoot current)
                    , sdPriorRootPC = P.unPitchClass (P.pitchClass (H.stateCadenceRoot current))
                    , sdSelectedDbIntervals = "N/A"
                    , sdSelectedDbMovement = "N/A"
                    , sdSelectedDbFunctionality = "N/A"
                    , sdGraphCount = 0
                    , sdGraphTop6 = []
                    , sdFallbackCount = 0
                    , sdFallbackTop6 = []
                    , sdPoolSize = 0
                    , sdEntropyUsed = ent
                    , sdGammaIndex = -1
                    , sdSelectedFrom = "none (absorbing)"
                    , sdPosteriorRoot = show (H.stateCadenceRoot current)
                    , sdPosteriorRootPC = P.unPitchClass (P.pitchClass (H.stateCadenceRoot current))
                    , sdRenderedChord = Nothing
                    , sdTransformTrace = Nothing
                    , sdAdvanceTrace = Nothing
                    }
              in diag : revDiags
      pure ((current, current : revChain, nonInvCount), diags)
    else do
      idx <- gammaIndexScaledWith gen ent (length finalPool)
      let nextCadence = fst (finalPool !! idx)
          (newState, advTrace) = advanceStateTraced current nextCadence
          newCounter = if H.isInversion nextCadence then 0 else nonInvCount + 1

          -- Build diagnostics only when requested
          diags = case mVerbosity of
            Nothing -> revDiags
            Just verbosity ->
              let selectedFrom = if idx < graphCount then "graph" else "fallback"
                  graphTop6 = take 6 [(show cad, conf) | (cad, conf) <- graphCandidates]
                  fallbackTop6' = take 6 [(show cad, score, cd, md, gd) | (cad, score, cd, md, gd) <- fallbackAll]
                  (renderedChord, transformTrace) = computeChordTrace verbosity newState
                  priorRoot = H.stateCadenceRoot current
                  priorRootPC = P.unPitchClass (P.pitchClass priorRoot)
                  posteriorRoot = H.stateCadenceRoot newState
                  posteriorRootPC = P.unPitchClass (P.pitchClass posteriorRoot)
                  diag = StepDiagnostic
                    { sdStepNumber = stepNum
                    , sdPriorCadence = show (extractCadence current)
                    , sdPriorRoot = show priorRoot
                    , sdPriorRootPC = priorRootPC
                    , sdSelectedDbIntervals = show (H.cadenceIntervals nextCadence)
                    , sdSelectedDbMovement = show (H.cadenceMovement nextCadence)
                    , sdSelectedDbFunctionality = H.cadenceFunctionality nextCadence
                    , sdGraphCount = graphCount
                    , sdGraphTop6 = graphTop6
                    , sdFallbackCount = fallbackCount
                    , sdFallbackTop6 = fallbackTop6'
                    , sdPoolSize = length finalPool
                    , sdEntropyUsed = ent
                    , sdGammaIndex = idx
                    , sdSelectedFrom = selectedFrom
                    , sdPosteriorRoot = show posteriorRoot
                    , sdPosteriorRootPC = posteriorRootPC
                    , sdRenderedChord = renderedChord
                    , sdTransformTrace = transformTrace
                    , sdAdvanceTrace = if verbosity >= 2 then Just advTrace else Nothing
                    }
              in diag : revDiags

      pure ((newState, newState : revChain, newCounter), diags)

-- |Unified single step for chain building (online, requires Neo4j).
--
-- Fetches graph transitions via Bolt then delegates all logic to 'stepChainBody'.
-- When verbosity is Nothing, skips diagnostic construction entirely.
-- When verbosity is Just n, collects diagnostics at level n:
--   Just 1 = standard diagnostics (rendered chord populated)
--   Just 2 = maximum diagnostics (full TransformTrace and AdvanceTrace)
stepChainCore :: GeneratorConfig
              -> GenIO
              -> Maybe Int
              -> Double
              -> HarmonicContext
              -> ParsedContext
              -> ComposerWeights
              -> ((H.CadenceState, [H.CadenceState], Int), [StepDiagnostic])
              -> Int
              -> Bolt.BoltActionT IO ((H.CadenceState, [H.CadenceState], Int), [StepDiagnostic])
stepChainCore config gen mVerbosity ent context pctx composerWeights acc@((current, _, _), _) stepNum = do
  let currentShow = T.pack $ show (extractCadence current)
  transitions <- fetchTransitions currentShow
  liftIO $ stepChainBody config gen mVerbosity ent context pctx composerWeights acc stepNum transitions

-- |Offline single step for chain building (no Neo4j required).
--
-- Passes empty transitions to 'stepChainBody', so generation relies entirely
-- on the consonanceFallback mechanism (~660 candidates shaped by context filters).
stepChainOffline :: GeneratorConfig
                 -> GenIO
                 -> Maybe Int
                 -> Double
                 -> HarmonicContext
                 -> ParsedContext
                 -> ((H.CadenceState, [H.CadenceState], Int), [StepDiagnostic])
                 -> Int
                 -> IO ((H.CadenceState, [H.CadenceState], Int), [StepDiagnostic])
stepChainOffline config gen mVerbosity ent context pctx acc stepNum =
  stepChainBody config gen mVerbosity ent context pctx mempty acc stepNum []

-------------------------------------------------------------------------------
-- Chain Building with Diagnostics
-------------------------------------------------------------------------------

-- |Build cadence chain with diagnostic collection (verbosity level 1)
buildChainWithDiag :: GeneratorConfig
                   -> GenIO            -- ^ Shared random generator
                   -> Double           -- ^ Entropy [0,1]
                   -> HarmonicContext
                   -> ParsedContext    -- ^ Pre-parsed context for O(1) lookups
                   -> ComposerWeights  -- ^ Composer blend weights
                   -> H.CadenceState   -- ^ Starting state
                   -> Int              -- ^ Number of steps to generate
                   -> Bolt.BoltActionT IO ([H.CadenceState], [StepDiagnostic])
buildChainWithDiag config gen ent context pctx composerWeights start totalSteps =
  buildChainWithDiagV config gen 1 ent context pctx composerWeights start totalSteps

-- |Build cadence chain with diagnostic collection (configurable verbosity)
-- Verbosity levels:
--   1 = standard diagnostics (sdRenderedChord populated)
--   2 = maximum diagnostics (full TransformTrace and AdvanceTrace)
buildChainWithDiagV :: GeneratorConfig
                    -> GenIO           -- ^ Shared random generator
                    -> Int             -- ^ Verbosity level (1 or 2)
                    -> Double          -- ^ Entropy [0,1]
                    -> HarmonicContext
                    -> ParsedContext    -- ^ Pre-parsed context for O(1) lookups
                    -> ComposerWeights -- ^ Composer blend weights
                    -> H.CadenceState  -- ^ Starting state
                    -> Int             -- ^ Number of steps to generate
                    -> Bolt.BoltActionT IO ([H.CadenceState], [StepDiagnostic])
buildChainWithDiagV config gen verbosity ent context pctx composerWeights start totalSteps = do
  let initCounter = if H.isInversion (H.stateCadence start) then 0 else 1
  ((_current, revChain, _counter), revDiags) <-
    foldM (stepChainCore config gen (Just verbosity) ent context pctx composerWeights)
          ((start, [start], initCounter), [])
          [1..totalSteps]
  pure (reverse revChain, reverse revDiags)

-------------------------------------------------------------------------------
-- Offline Chain Building (plain IO, no Bolt/Neo4j)
-------------------------------------------------------------------------------

-- |Build cadence chain offline (no Neo4j required).
--
-- Uses only the consonanceFallback mechanism — no graph traversal.
-- Progressions are shaped by context filters (overtones, key, roots, drift,
-- inversion spacing) and entropy. Fully musical without corpus-trained style.
buildChainOffline :: GeneratorConfig
                  -> GenIO
                  -> Double
                  -> HarmonicContext
                  -> ParsedContext
                  -> H.CadenceState
                  -> Int
                  -> IO [H.CadenceState]
buildChainOffline config gen ent context pctx start totalSteps = do
  let initCounter = if H.isInversion (H.stateCadence start) then 0 else 1
  ((_current, revChain, _counter), _noDiags) <-
    foldM (stepChainOffline config gen Nothing ent context pctx)
          ((start, [start], initCounter), [])
          [1..totalSteps]
  pure $ reverse revChain

-- |Build cadence chain offline with standard diagnostic collection.
buildChainOfflineWithDiag :: GeneratorConfig
                           -> GenIO
                           -> Double
                           -> HarmonicContext
                           -> ParsedContext
                           -> H.CadenceState
                           -> Int
                           -> IO ([H.CadenceState], [StepDiagnostic])
buildChainOfflineWithDiag config gen ent context pctx start totalSteps = do
  let initCounter = if H.isInversion (H.stateCadence start) then 0 else 1
  ((_current, revChain, _counter), revDiags) <-
    foldM (stepChainOffline config gen (Just 1) ent context pctx)
          ((start, [start], initCounter), [])
          [1..totalSteps]
  pure (reverse revChain, reverse revDiags)

-- |Build cadence chain offline with configurable verbosity diagnostics.
buildChainOfflineWithDiagV :: GeneratorConfig
                            -> GenIO
                            -> Int
                            -> Double
                            -> HarmonicContext
                            -> ParsedContext
                            -> H.CadenceState
                            -> Int
                            -> IO ([H.CadenceState], [StepDiagnostic])
buildChainOfflineWithDiagV config gen verbosity ent context pctx start totalSteps = do
  let initCounter = if H.isInversion (H.stateCadence start) then 0 else 1
  ((_current, revChain, _counter), revDiags) <-
    foldM (stepChainOffline config gen (Just verbosity) ent context pctx)
          ((start, [start], initCounter), [])
          [1..totalSteps]
  pure (reverse revChain, reverse revDiags)

-------------------------------------------------------------------------------
-- Scoring and Selection
-------------------------------------------------------------------------------

-- |Score transitions by applying composer blend to edge weights.
-- Filters to confidence > 0 and sorts highest first.
-- Uses resolveWeights internally to multiply user blend by edge weights,
-- then filters out zero-score candidates.
scoreByConfidence :: ComposerWeights -> [(H.Cadence, ComposerWeights)] -> [(H.Cadence, Double)]
scoreByConfidence blend transitions = Q.applyComposerBlend blend transitions

-------------------------------------------------------------------------------
-- Consonance Fallback
-------------------------------------------------------------------------------

-- |Generate fallback candidates from HarmonicContext filters.
--
-- This implements the legacy "constructive generation" pattern:
--   1. Get effective overtone palette (tuning filtered by key)
--   2. Get allowed roots (via resolveRoots which handles "key"/"tones" options)
--   3. Generate all valid triads from roots × overtones (660 structures with wildcard)
--   4. Compute actual movement from current state to each candidate
--   5. Score with multiplicative formula: (rootMotionDiss × structureDiss × (gammaDraw+1))
--   6. Sort by score (lower badness = higher score)
--
-- Movement computation matches legacy getCadenceOptions which uses:
--   toCadence (transposeCadence enharm rootPC prev, nxt)
-- to derive proper movements from current position to each candidate.
-- This ensures fallback cadences have real movements, enabling subsequent
-- iterations to find graph matches and traverse freely.
--
-- The gamma draw adds organic randomness to scoring, preventing identical
-- scores for structurally similar triads with the same movement type.
-- Returns IO [(Cadence, score, chordDiss, motionDiss, gammaDraw)]
consonanceFallback :: H.CadenceState -> HarmonicContext -> IO [(H.Cadence, Double, Double, Double, Double)]
consonanceFallback currentState context = do
  rng <- createSystemRandom
  consonanceFallbackWith rng currentState context

-- |Like 'consonanceFallback' but uses a shared random generator.
consonanceFallbackWith :: GenIO -> H.CadenceState -> HarmonicContext -> IO [(H.Cadence, Double, Double, Double, Double)]
consonanceFallbackWith gen currentState context =
  let -- Get current root pitch class for movement computation
      currentRoot = P.pitchClass (H.stateCadenceRoot currentState)

      -- Get overtone palette (4 partials per fundamental)
      overtones = parseOvertones' 4 (_hcOvertones context)

      -- Apply key filter to overtones
      keyPcs = parseKey (_hcKey context)
      effectiveOvertones = if isWildcard (_hcKey context)
                           then overtones
                           else filter (`elem` keyPcs) overtones

      -- Generate all valid triads: all ROOT+PAIR combinations
      -- For complete coverage: generate from effectiveOvertones (key-filtered overtone palette)
      -- Each root gets all possible 2-note pairs from the remaining pitches
      -- This preserves inversion distinctions: [0,4,7], [4,0,7], [7,0,4] are three unique structures
      -- NOTE: hcRoots is for BASS filtering (applied at line 1486), NOT for root generation!
      -- Always generate from all effective overtones, let the filter handle bass note constraints
      triads = let allRoots = effectiveOvertones
               in concatMap (\r -> overtoneSets 3 [r] effectiveOvertones) allRoots

      -- No normalization deduplication: preserve ROOT+PAIR distinction for inversions
      -- Each triad is already distinct by its root position
      uniqueTriads = triads
  in do
      -- Compute multiplicative badness score with gamma randomness for each triad
      -- Returns IO (score, chordDiss, motionDiss, gammaDraw) for each candidate
      results <- mapM (\t -> do
                         let cad = triadToCadenceFrom currentRoot t
                         (score, cd, md, gd) <- computeFallbackScoreWith gen currentRoot cad t
                         pure (cad, score, cd, md, gd)
                       ) uniqueTriads

      -- Sort by score (highest first = lowest badness = best combination)
      pure $ sortBy (compare `on` (\(_, s, _, _, _) -> Down s)) results

-- |Like 'consonanceFallbackWith' but uses pre-parsed context for efficiency.
consonanceFallbackParsed :: GenIO -> H.CadenceState -> ParsedContext -> IO [(H.Cadence, Double, Double, Double, Double)]
consonanceFallbackParsed gen currentState pctx =
  let currentRoot = P.pitchClass (H.stateCadenceRoot currentState)
      effectiveOvertones = IntSet.toList (pcEffectiveOvertones pctx)
      triads = concatMap (\r -> overtoneSets 3 [r] effectiveOvertones) effectiveOvertones
      uniqueTriads = triads
  in do
      results <- mapM (\t -> do
                         let cad = triadToCadenceFrom currentRoot t
                         (score, cd, md, gd) <- computeFallbackScoreWith gen currentRoot cad t
                         pure (cad, score, cd, md, gd)
                       ) uniqueTriads
      pure $ sortBy (compare `on` (\(_, s, _, _, _) -> Down s)) results

-- |Convert a triad (list of pitch classes) to a Cadence with movement from current root.
-- Movement is computed from currentRoot to the triad's root (head of sorted triad).
-- This matches legacy getCadenceOptions which uses toCadence to derive proper movements.
-- |Convert fallback triad (absolute pitch classes) to Cadence with zero-form normalization.
-- Applies H.zeroFormPC to ensure all fallback-generated cadences store relative intervals,
-- matching database format. This guarantees naming consistency across all cadence sources.
-- IMPORTANT: overtoneSets generates [root, note1, note2] with root FIRST.
-- We must use the first element as root, not the minimum!
triadToCadenceFrom :: P.PitchClass -> [Int] -> H.Cadence
triadToCadenceFrom currentRoot pitches =
  let triadRoot = P.mkPitchClass (head pitches)  -- First element from overtoneSets is the root
      movement = H.toMovement currentRoot triadRoot
      -- Zero-form normalization: [P 4,P 7,P 11] → [P 0,P 3,P 7]
      -- zeroFormPC subtracts first element and sorts, so don't pre-sort!
      pcs = H.zeroFormPC (map P.mkPitchClass pitches)
      functionality = H.toFunctionality pcs
  in H.Cadence functionality movement pcs

-------------------------------------------------------------------------------
-- Fallback Scoring
-------------------------------------------------------------------------------

-- |Compute multiplicative fallback score with entropy-based randomness.
-- Formula: badness = rootMotionDiss × structureDiss × (gammaDraw + 1)
--          score = 10000 - badness
--
-- Chord dissonance range: 6 (major/minor triad) to ~50 (dense cluster)
-- Root motion range: 1 (P5/P4) to 6 (tritone)
-- Gamma draw range: ~0.0-5.0 (minimum entropy, shape=1.01)
--
-- The multiplicative formula spreads scores organically based on:
--   * Root motion quality (smooth vs rough)
--   * Vertical consonance (simple vs complex)
--   * Stochastic perturbation (via gamma draw)
--
-- This prevents score clustering and eliminates the need for pool size limits.
-- Returns IO (finalScore, chordDiss, motionDiss, gammaDraw)
computeFallbackScoreWithComponents :: P.PitchClass -> H.Cadence -> [Int] -> IO (Double, Double, Double, Double)
computeFallbackScoreWithComponents currentRoot cad triad = do
  rng <- createSystemRandom
  computeFallbackScoreWith rng currentRoot cad triad

-- |Like 'computeFallbackScoreWithComponents' but uses a shared random generator.
computeFallbackScoreWith :: GenIO -> P.PitchClass -> H.Cadence -> [Int] -> IO (Double, Double, Double, Double)
computeFallbackScoreWith gen _currentRoot cad triad = do
  -- Chord vertical dissonance (raw Hindemith score)
  let chordDiss = fromIntegral (dissonanceScore triad) :: Double

      -- Root motion dissonance (extract interval from Movement)
      interval = extractMovementInterval (H.cadenceMovement cad)
      motionDiss = fromIntegral (D.rootMotionScore interval) :: Double

  -- Draw gamma sample for entropy (minimum entropy: shape=1.01)
  gammaDraw <- Dist.gamma 1.01 1.0 gen

  -- Multiplicative badness: all three factors contribute
  let badness = chordDiss * motionDiss * (gammaDraw + 1.0)

      -- Final score: 10000 - badness (higher is better)
      finalScore = 10000.0 - badness

  pure (finalScore, chordDiss, motionDiss, gammaDraw)

-- Convenience wrapper returning only the score (now in IO)
computeFallbackScore :: P.PitchClass -> H.Cadence -> [Int] -> IO Double
computeFallbackScore root cad triad = do
  (score, _, _, _) <- computeFallbackScoreWithComponents root cad triad
  pure score

-- |Extract interval class (0-6) from Movement type.
-- Maps Movement to interval class for rootMotionScore input.
-- Interval class folds intervals larger than tritone to their complement.
extractMovementInterval :: H.Movement -> Int
extractMovementInterval movement = case movement of
  H.Asc pc   -> intervalClassFromPC (P.unPitchClass pc)
  H.Desc pc  -> intervalClassFromPC (P.unPitchClass pc)
  H.Unison   -> 0
  H.Tritone  -> 6
  where
    intervalClassFromPC semitones =
      let m = semitones `mod` 12
      in if m <= 6 then m else 12 - m

-------------------------------------------------------------------------------
-- Dissonance Drift Filter
-------------------------------------------------------------------------------

-- |Filter the candidate pool by dissonance drift direction.
--
-- * @Dissonant@: keep only candidates with dissonance >= current state's dissonance
-- * @Consonant@: keep only candidates with dissonance <= current state's dissonance
-- * @Free@: no filtering (return pool unchanged)
--
-- Safety fallback: if filtering empties the pool, returns the original
-- unfiltered pool so generation never fails.
applyDriftFilter :: Drift -> H.CadenceState -> [(H.Cadence, Double)] -> [(H.Cadence, Double)]
applyDriftFilter Free _ pool = pool
applyDriftFilter direction currentState pool =
  let currentDiss = dissonanceScore
        (map P.unPitchClass (H.cadenceIntervals (H.stateCadence currentState)))
      candidateDiss cad = dissonanceScore (map P.unPitchClass (H.cadenceIntervals cad))
      predicate = case direction of
        Dissonant -> \(cad, _) -> candidateDiss cad >= currentDiss
        Consonant -> \(cad, _) -> candidateDiss cad <= currentDiss
      filtered = filter predicate pool
  in if null filtered then pool else filtered

-------------------------------------------------------------------------------
-- Pedal Tone Filter
-------------------------------------------------------------------------------

-- |Filter the candidate pool by pedal tone constraints.
--
-- Required tones must be present in every candidate chord (as absolute pitch
-- classes, anywhere in the chord — root or upper voices).
-- Preferred tones (@?@ suffix in input) are applied when doing so leaves at
-- least 'minPedalPool' candidates; otherwise they are relaxed and only required
-- tones are enforced. Safety fallback: never returns an empty pool.
applyPedalFilter :: ParsedContext -> H.CadenceState -> [(H.Cadence, Double)] -> [(H.Cadence, Double)]
applyPedalFilter pctx currentState pool
  | IntSet.null req && IntSet.null pref = pool
  | otherwise =
      let cadenceAbsPCs cadence =
            let (movement, chord) = H.deconstructCadence cadence
                prevRoot = P.pitchClass (H.stateCadenceRoot currentState)
                newRoot = case movement of
                  H.Unison   -> prevRoot
                  H.Tritone  -> P.transpose 6 prevRoot
                  H.Asc pc   -> P.transpose (P.unPitchClass pc) prevRoot
                  H.Desc pc  -> P.transpose (negate $ P.unPitchClass pc) prevRoot
                  H.Empty    -> prevRoot
                rootInt   = fromIntegral (P.unPitchClass newRoot)
                chordInts = map (fromIntegral . P.unPitchClass) chord
            in IntSet.fromList $ map (\i -> (i + rootInt) `mod` 12) chordInts
          combined     = IntSet.union req pref
          reqFiltered  = filter (IntSet.isSubsetOf req     . cadenceAbsPCs . fst) pool
          combFiltered = filter (IntSet.isSubsetOf combined . cadenceAbsPCs . fst) pool
          result
            | IntSet.null pref                     = reqFiltered
            | length combFiltered >= minPedalPool  = combFiltered
            | not (null reqFiltered)               = reqFiltered
            | otherwise                            = pool   -- safety
      in result
  where
    req  = pcPedalRequired pctx
    pref = pcPedalPreferred pctx

-- |Minimum candidate pool size when applying preferred pedal tones.
-- If fewer candidates remain after applying required+preferred tones,
-- the preferred constraint is relaxed to required-only.
minPedalPool :: Int
minPedalPool = 10

-------------------------------------------------------------------------------
-- R Constraint Filtering
-------------------------------------------------------------------------------

-- |Apply R constraints to filter transitions
applyRConstraints :: HarmonicContext
                  -> H.CadenceState
                  -> [(H.Cadence, ComposerWeights)]
                  -> [(H.Cadence, ComposerWeights)]
applyRConstraints context currentState = filter (matchesContext context currentState . fst)

-- |Check if a cadence matches the harmonic context filters.
--
-- Filter logic (matching legacy behavior):
--   1. Compute effective overtones: key-filtered overtone palette
--   2. All chord pitches must be in effective overtones
--   3. Root must be in resolved roots (handles "key"/"tones" options)
matchesContext :: HarmonicContext -> H.CadenceState -> H.Cadence -> Bool
matchesContext context currentState cadence =
  let (movement, chord) = H.deconstructCadence cadence

      -- Get effective overtone palette (key-filtered)
      rawOvertones = parseOvertones' 4 (_hcOvertones context)
      keyPcs = parseKey (_hcKey context)
      effectiveOvertones = if isWildcard (_hcKey context)
                           then rawOvertones
                           else filter (`elem` keyPcs) rawOvertones

      -- Get allowed bass notes (the "roots" parameter actually filters bass notes, not harmonic roots)
      allowedBassNotes = resolveRoots (_hcOvertones context) (_hcKey context) (_hcRoots context)

      -- Compute current root from previous state + movement
      prevRoot = P.pitchClass (H.stateCadenceRoot currentState)
      currentRoot = case movement of
        H.Unison -> prevRoot
        H.Tritone -> P.transpose 6 prevRoot
        H.Asc pc -> P.transpose (P.unPitchClass pc) prevRoot
        H.Desc pc -> P.transpose (negate $ P.unPitchClass pc) prevRoot
        H.Empty -> prevRoot

      -- Convert chord intervals to Int for transposition
      chordInts = map (fromIntegral . P.unPitchClass) chord
      currentRootInt = fromIntegral (P.unPitchClass currentRoot)

      -- Transpose relative intervals (zero-form) to absolute pitches
      -- Example: [0,4,7] + root 4 (E) = [4,8,11] mod 12
      absolutePitches = map (\interval -> (interval + currentRootInt) `mod` 12) chordInts

      -- Bass note is the FIRST interval (fundamental), not minimum!
      -- This matches how bassNotes and toTriad compute bass.
      bassInt = if null absolutePitches then 0 else head absolutePitches

      -- All absolute chord pitches must be in effective overtones
      -- (effectiveOvertones already handles wildcard cases correctly)
      overtonesMatch = all (`elem` effectiveOvertones) absolutePitches

      -- Bass note must be in allowed bass notes (or wildcard)
      bassMatch = isWildcard (_hcRoots context)
                  || bassInt `elem` allowedBassNotes

  in overtonesMatch && bassMatch

-- |Like 'applyRConstraints' but uses pre-parsed context for O(1) lookups.
applyRConstraintsParsed :: ParsedContext
                        -> H.CadenceState
                        -> [(H.Cadence, ComposerWeights)]
                        -> [(H.Cadence, ComposerWeights)]
applyRConstraintsParsed pctx currentState = filter (matchesContextParsed pctx currentState . fst)

-- |Like 'applyRConstraintsParsed' but with an optional bass target override.
-- When bassTarget is Just, only candidates whose bass matches the target pass.
applyRConstraintsWithTarget :: Maybe Int
                            -> ParsedContext
                            -> H.CadenceState
                            -> [(H.Cadence, ComposerWeights)]
                            -> [(H.Cadence, ComposerWeights)]
applyRConstraintsWithTarget bassTarget pctx currentState =
  filter (matchesContextWithTarget bassTarget pctx currentState . fst)

-- |Like 'matchesContext' but uses pre-parsed IntSet lookups instead of reparsing text.
matchesContextParsed :: ParsedContext -> H.CadenceState -> H.Cadence -> Bool
matchesContextParsed = matchesContextWithTarget Nothing

-- |Core filter with optional bass target override from rise/fall direction.
-- When bassTarget is Just, the bass note must equal the target exactly.
-- When Nothing, falls back to the standard set-membership check.
matchesContextWithTarget :: Maybe Int -> ParsedContext -> H.CadenceState -> H.Cadence -> Bool
matchesContextWithTarget bassTarget pctx currentState cadence =
  let (movement, chord) = H.deconstructCadence cadence

      -- Compute current root from previous state + movement
      prevRoot = P.pitchClass (H.stateCadenceRoot currentState)
      currentRoot = case movement of
        H.Unison -> prevRoot
        H.Tritone -> P.transpose 6 prevRoot
        H.Asc pc -> P.transpose (P.unPitchClass pc) prevRoot
        H.Desc pc -> P.transpose (negate $ P.unPitchClass pc) prevRoot
        H.Empty -> prevRoot

      -- Convert chord intervals to Int for transposition
      chordInts = map (fromIntegral . P.unPitchClass) chord
      currentRootInt = fromIntegral (P.unPitchClass currentRoot)

      -- Transpose relative intervals (zero-form) to absolute pitches
      absolutePitches = map (\interval -> (interval + currentRootInt) `mod` 12) chordInts

      -- Bass note is the FIRST interval (fundamental), not minimum!
      bassInt = if null absolutePitches then 0 else head absolutePitches

      -- All absolute chord pitches must be in effective overtones (IntSet lookup).
      -- When bass direction targets a specific note, exempt that pitch class
      -- from the overtone check — allows chromatic passing bass notes
      -- (e.g. D# in a G major context) while still constraining upper voices.
      pitchesToCheck = case bassTarget of
        Just target -> filter (/= target) absolutePitches
        Nothing     -> absolutePitches
      overtonesMatch = all (`IntSet.member` pcEffectiveOvertones pctx) pitchesToCheck

      -- Bass note check: exact target if rise/fall active, otherwise set membership
      bassMatch = case bassTarget of
        Just target -> bassInt == target
        Nothing     -> pcIsRootsWild pctx
                       || bassInt `IntSet.member` pcAllowedBassNotes pctx

  in overtonesMatch && bassMatch

-------------------------------------------------------------------------------
-- State Advancement
-------------------------------------------------------------------------------

-- |Advance the CadenceState based on movement to a new cadence
advanceState :: H.CadenceState -> H.Cadence -> H.CadenceState
advanceState currentState newCadence =
  fst $ advanceStateTraced currentState newCadence

-- |Advance the CadenceState with full trace of intermediate values
-- Used for maximum verbosity diagnostics (gen'')
-- Enharmonic spelling is inferred from the new chord's absolute pitch content
-- using the 3-layer inferSpelling system (3-set match → 2-set match → root fallback).
advanceStateTraced :: H.CadenceState -> H.Cadence -> (H.CadenceState, AdvanceTrace)
advanceStateTraced currentState newCadence =
  let currentRoot = H.stateCadenceRoot currentState
      currentRootPC = P.pitchClass currentRoot
      movement = H.cadenceMovement newCadence
      movementInterval = H.fromMovement movement
      newRootPC = currentRootPC + movementInterval
      -- Infer spelling from the new chord's absolute pitches
      tones = map P.unPitchClass $ H.cadenceIntervals newCadence
      absolutePitches = map (\t -> (t + P.unPitchClass newRootPC) `mod` 12) tones
      inferredSpelling = H.inferSpelling absolutePitches
      -- Ambiguous patterns (e.g. min7o3 at roots where maj/min disagree) adopt prior spelling
      newSpelling = if H.isAmbiguousPattern absolutePitches
                    then H.stateSpelling currentState
                    else inferredSpelling
      newRoot = H.enharmonicFunc newSpelling newRootPC
      newState = H.CadenceState newCadence newRoot newSpelling

      -- Build trace
      enharmName = case newSpelling of
        H.FlatSpelling -> "flat"
        H.SharpSpelling -> "sharp"
      trace = AdvanceTrace
        { atCurrentRoot = show currentRoot
        , atCurrentRootPC = P.unPitchClass currentRootPC
        , atMovement = show movement
        , atMovementInterval = P.unPitchClass movementInterval
        , atNewRootPC = P.unPitchClass newRootPC
        , atEnharmFunc = enharmName
        , atNewRoot = show newRoot
        }
  in (newState, trace)

-------------------------------------------------------------------------------
-- Extraction and Conversion
-------------------------------------------------------------------------------

-- |Extract Cadence from CadenceState
extractCadence :: H.CadenceState -> H.Cadence
extractCadence = H.stateCadence

-- |Convert a chain of CadenceStates to a Progression
chainToProgression :: [H.CadenceState] -> Prog.Progression
chainToProgression = Prog.fromCadenceStates
