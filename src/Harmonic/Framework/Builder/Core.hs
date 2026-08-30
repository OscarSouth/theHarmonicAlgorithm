-- |
-- Module      : Harmonic.Framework.Builder.Core
-- Description : Core generation engine for harmonic progressions
--
-- Internal chain building, candidate pool construction, R-constraint filtering,
-- consonance fallback generation, state advancement, and progression conversion.
-- Online vs offline is a 'TransitionSource' argument, not a code path:
-- one chain builder serves every family and both modes.

module Harmonic.Framework.Builder.Core
  ( -- * The transition seam
    TransitionSource
  , offlineSource
  , classicalSource
  , sourceFor

    -- * Chain building (one builder, any source)
  , buildChainWith
  , stepChainWith

    -- * Conversion
  , chainToProgression
  , extractCadence

    -- * Filtering (exposed for testing)
  , matchesContextWithTarget
  , applyDriftFilter

    -- * Context-aware starting cue
  , tonalStartCue
  , rootPositionCue
  ) where

import qualified Data.Text as T
import qualified Data.IntSet as IntSet
import           Control.Monad (foldM)
import           Data.List (sortBy, sort, nub, isInfixOf)
import           Data.Function (on)
import           Data.Ord (Down(..))
import           System.Random.MWC (GenIO, uniform, uniformR, createSystemRandom)
import qualified System.Random.MWC.Distributions as Dist

import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as P
import qualified Harmonic.Rules.Types.Progression as Prog
import qualified Data.Map.Strict as Map
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Set as Set
import           Harmonic.Database (DbConn, runDb, connectNeo4j)
import           Harmonic.Evaluation.Database.Query (ComposerWeights)
import qualified Harmonic.Evaluation.Database.Query as Q
import           Harmonic.Traversal.Probabilistic (gammaIndexScaledWith)
import           Harmonic.Rules.Constraints.Filter (nthAbove, nthBelow)
import           Harmonic.Rules.Constraints.Overtone (overtoneSets, possibleTriads)
import           Harmonic.Evaluation.Scoring.Dissonance (dissonanceScore)
import qualified Harmonic.Evaluation.Scoring.Dissonance as D

import           Harmonic.Framework.Builder.Types
import           Harmonic.Framework.Builder.Diagnostics (computeChordTrace)

-------------------------------------------------------------------------------
-- Chain Building (Inside DbActionT)
-------------------------------------------------------------------------------

-- |How a walk key resolves to scored transitions: THE seam between the
-- step engine and any particular graph (or none). Sources return
-- positive scores sorted highest first — exactly the contract
-- @stepChainBody@ documents for its @transitions@ argument.
type TransitionSource = T.Text -> IO [(H.Cadence, Double)]

-- |The no-graph source: every step's pool is the consonance fallback
-- alone. This IS offline mode — nothing else distinguishes it.
offlineSource :: TransitionSource
offlineSource _ = pure []

-- |The classical @Cadence@ graph under a composer blend: the aggregate
-- fast path for the wildcard (empty) blend, the full weights fetch and
-- blend resolution otherwise.
classicalSource :: DbConn -> ComposerWeights -> TransitionSource
classicalSource conn weights key
  | Map.null weights = runDb conn (Q.fetchTransitionsAggregate key)
  | otherwise        = runDb conn (Q.applyComposerBlend weights <$> Q.fetchTransitions key)

-- |Resolve a seek string to its 'TransitionSource': @"none"@ (case-
-- insensitive) is 'offlineSource'; anything else opens the database and
-- blends per 'Q.parseComposerWeights'.
--
-- A named blend is additionally spell-checked against the graph as the
-- walk runs: a name that has not appeared on any fetched edge after a
-- few steps is reported once (a typo'd composer otherwise zeroes its
-- share silently and the walk quietly degrades toward fallback).
sourceFor :: T.Text -> IO TransitionSource
sourceFor seekStr
  | T.toLower (T.strip seekStr) == "none" = pure offlineSource
  | otherwise = do
      conn <- connectNeo4j
      let blend = Q.parseComposerWeights seekStr
      if Map.null blend
        then pure (classicalSource conn blend)
        else do
          ref <- newIORef (Map.keysSet blend, 0 :: Int)
          pure (spellcheckedSource ref conn blend)

-- Wraps 'classicalSource' with the unseen-name tracker: each fetch
-- crosses off blend names present on any returned edge; after
-- 'spellcheckFetches' fetches, survivors are reported once (weights are
-- sparse per edge, so a single node proves nothing — a few steps of
-- absence is strong evidence of a typo, and the message says which).
spellcheckedSource :: IORef (Set.Set T.Text, Int) -> DbConn -> ComposerWeights -> TransitionSource
spellcheckedSource ref conn blend key = do
  rows <- runDb conn (Q.fetchTransitions key)
  (unseen, n) <- readIORef ref
  if Set.null unseen
    then pure ()
    else do
      let seen    = Set.unions [ Map.keysSet w | (_, w) <- rows ]
          unseen' = unseen `Set.difference` seen
          n'      = n + 1
      if n' >= spellcheckFetches && not (Set.null unseen')
        then do
          putStrLn $ "⚠ seek: " ++ show (map T.unpack (Set.toList unseen'))
                   ++ " matched no composer on any edge of the first "
                   ++ show n' ++ " steps — check the spelling against"
                   ++ " documents/COMPOSERS.md (walk continues; their"
                   ++ " share contributes nothing)"
          writeIORef ref (Set.empty, n')
        else writeIORef ref (unseen', n')
  pure (Q.applyComposerBlend blend rows)

-- Fetches to observe before calling a never-seen blend name a typo.
spellcheckFetches :: Int
spellcheckFetches = 3

-- |Build the cadence chain step by step against any 'TransitionSource'.
--
-- The one chain builder: online\/offline is the source argument, fixed
-- vs per-bar R-context is the supplier argument (@const pctx@ for the
-- plain families; 'Harmonic.Framework.Builder.genP' narrows overtones
-- per bar), diagnostics collection is the verbosity argument (@Nothing@
-- skips construction entirely; @Just 1@ standard, @Just 2@ maximum).
--
-- Algorithm per step: fetch the walk key's transitions from the source
-- (the key is the current state's most-consonant rooted embedded triad —
-- identity for triads, so the walk never silently goes offline on
-- cardinality), then delegate the full R-filter\/scoring\/selection
-- logic to @stepChainBody@ (pool = graph candidates + unlimited
-- consonance fallback).
buildChainWith :: TransitionSource
               -> GenIO                    -- ^ Shared random generator
               -> Maybe Int                -- ^ Diagnostics verbosity (Nothing \/ Just 1 \/ Just 2)
               -> Double                   -- ^ Entropy (>= 0; rank-target dial)
               -> HarmonicContext
               -> (Int -> ParsedContext)   -- ^ Bar index (1-based) -> R context for that bar
               -> H.CadenceState           -- ^ Starting state
               -> Int                      -- ^ Number of steps to generate
               -> IO ([H.CadenceState], [StepDiagnostic])
buildChainWith source gen mVerbosity ent context pctxAt start totalSteps = do
  let initCounter = if H.isInversion (H.stateCadence start) then 0 else 1
  ((_current, revChain, _counter), revDiags) <-
    foldM (\acc i -> stepChainWith source gen mVerbosity ent context (pctxAt i) acc i)
          ((start, [start], initCounter), [])
          [1..totalSteps]
  pure (reverse revChain, reverse revDiags)

-- |One step against a 'TransitionSource': fetch, then @stepChainBody@.
stepChainWith :: TransitionSource
              -> GenIO
              -> Maybe Int
              -> Double
              -> HarmonicContext
              -> ParsedContext
              -> ((H.CadenceState, [H.CadenceState], Int), [StepDiagnostic])
              -> Int
              -> IO ((H.CadenceState, [H.CadenceState], Int), [StepDiagnostic])
stepChainWith source gen mVerbosity ent context pctx acc@((current, _, _), _) stepNum = do
  scored <- source (T.pack $ show (extractCadence (H.walkTriadState current)))
  stepChainBody gen mVerbosity ent context pctx acc stepNum scored


-- |Resolve a 'BassDirectionSpec' into a concrete 'BassDirection' for a
-- single generation step. Returns 'Nothing' when no spec is active, or
-- when the spec's optional @?@ flag caused the coin flip to come up tails.
--
-- Rotation (@BDRotate@) cycles through the choices by @stepNum@ (1-based).
-- Random pick (@BDRandomPick@) samples uniformly from the choices.
resolveBassDirection
  :: GenIO -> Int -> Maybe BassDirectionSpec -> IO (Maybe BassDirection)
resolveBassDirection _   _       Nothing     = pure Nothing
resolveBassDirection gen stepNum (Just spec) = do
  active <- if bdsOptional spec
              then do
                r <- uniform gen :: IO Double
                pure (r < 0.5)
              else pure True
  if not active
    then pure Nothing
    else do
      let cs = bdsChoices spec
      n <- case bdsSelector spec of
        -- bdsChoices is non-empty by parser construction; 1 = the bare
        -- rise/fall default, unreachable here.
        BDFixed      -> pure (case cs of { (c : _) -> c; [] -> 1 })
        BDRotate     -> pure (cs !! ((stepNum - 1) `mod` length cs))
        BDRandomPick -> do
          i <- uniformR (0, length cs - 1) gen
          pure (cs !! i)
      pure $ Just $ case bdsKind spec of
        RiseK -> Rise n
        FallK -> Fall n

-- |Core body for a single chain-building step (plain IO, no database dependency).
--
-- Takes pre-fetched transitions and executes the full filtering\/scoring\/selection logic.
-- Used by 'stepChainWith' for every family and mode (the source argument
-- decides online vs offline).
-- When transitions is empty (offline mode), generation relies entirely on the consonance fallback.
stepChainBody :: GenIO
              -> Maybe Int        -- ^ Nothing = no diagnostics, Just n = verbosity level
              -> Double           -- ^ Entropy [0,1]
              -> HarmonicContext
              -> ParsedContext
              -> ((H.CadenceState, [H.CadenceState], Int), [StepDiagnostic])
              -> Int
              -> [(H.Cadence, Double)]   -- ^ Pre-scored transitions, sorted desc (empty for offline)
              -> IO ((H.CadenceState, [H.CadenceState], Int), [StepDiagnostic])
stepChainBody gen mVerbosity ent _context pctx ((current, revChain, nonInvCount), revDiags) stepNum transitions = do
  -- Walk shadow: all stage-1 machinery runs against the current state's
  -- most-consonant rooted embedded triad, so drift comparisons stay
  -- triad-vs-triad and graph keys stay corpus-shaped. Identity for every
  -- <=3-interval state — it bites only when a >3-note bar seeds a walk
  -- (regen of hand-built extended material via 'genFrom').
  let walkCur = H.walkTriadState current

  -- Resolve bass direction for this step (may consume randomness for
  -- optional @?@ tokens and for BDRandomPick comma-list selectors)
  mDir <- resolveBassDirection gen stepNum (pcBassDirectionSpec pctx)
  let prevBassPC = P.unPitchClass (P.pitchClass (H.stateCadenceRoot current))
      bassTarget = case mDir of
        Nothing       -> Nothing
        Just (Rise n) -> Just $ nthAbove n prevBassPC (pcAllowedBassNotes pctx)
        Just (Fall n) -> Just $ nthBelow n prevBassPC (pcAllowedBassNotes pctx)

  -- Apply R constraints (pure filter by ParsedContext)
  let filtered = applyRConstraintsWithTarget bassTarget pctx walkCur transitions

  -- Transitions arrive pre-scored by the caller (composer blend or the
  -- r.confidence aggregate), positive and sorted highest first; the R-filter
  -- above preserves that order.
  let -- Apply per-bar soft-boost (inverted sense: graph "higher is better",
      -- so dividing a score by a sub-unit boost raises it — matching the
      -- fallback-side effect of lowering @badness@ via the same boost).
      boost = pcSoftBoost pctx
      graphCandidates
        | boost == 1.0 = filtered
        | otherwise    =
            let boosted = [(c, s / boost) | (c, s) <- filtered]
            in sortBy (compare `on` (Down . snd)) boosted
      graphCount = length graphCandidates

  -- Build candidate pool: graph candidates + consonance fallback
  -- NO POOL SIZE LIMIT - use full 660-candidate fallback generation
  --
  -- The fallback is computed UNCONDITIONALLY every step, by design: that is
  -- what guarantees backfill is always present when graphCount is small
  -- (conditional computation was tried and proved fragile). Online, with a
  -- typical graphCount of 20-60, the fallback segment of the stacked
  -- ranking is effectively unreachable by the gamma draw — the cost is a
  -- ~660-candidate score-and-sort per step, negligible next to the per-step
  -- Neo4j round-trip. Offline the fallback IS the pool. Keep unconditional.
  fallbackAll <- consonanceFallbackParsed gen walkCur pctx
  -- Apply R constraints to fallback candidates (same as graph candidates)
  let unfilteredFallback = [(cad, score) | (cad, score, _, _, _) <- fallbackAll]
      filteredFallback = filter (\(cad, _) -> matchesContextWithTarget bassTarget pctx walkCur cad) unfilteredFallback
      fallbackCount = length filteredFallback
      -- Create unified pool with (Cadence, score) format
      -- Graph candidates first (preserves database priority), then filtered fallback
      pool = graphCandidates ++ filteredFallback

      -- Apply dissonance drift filter
      driftedPool = applyDriftFilter (pcDrift pctx) walkCur pool

      -- Apply inversion spacing constraint
      invSpacing = pcInversionSpacing pctx
      inversionAllowed = nonInvCount >= invSpacing
      spacedPool = if inversionAllowed
                   then driftedPool
                   else filter (not . H.isInversion . fst) driftedPool
      prepedalPool = if null spacedPool then driftedPool else spacedPool
      finalPool = applyPedalFilter pctx walkCur prepedalPool

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
                    , sdTristrataIdx = Nothing
                    , sdTristrata = Nothing
                    , sdStrataLabel = Nothing
                    , sdMode = Nothing
                    , sdStrataChroma = Nothing
                    , sdModeChroma = Nothing
                    , sdSoftBoost = Nothing
                    , sdHarmonicRootPC = Nothing
                    , sdParentKey = Nothing
                    , sdModeResult = Nothing
                    , sdBarSpelling = Nothing
                    , sdPoly = Nothing
                    }
              in diag : revDiags
      pure ((current, current : revChain, nonInvCount), diags)
    else do
      idx <- gammaIndexScaledWith gen ent (length finalPool)
      let nextCadence = fst (finalPool !! idx)
          (newState, advTrace) = advanceStateTraced (pcKeySpelling pctx) walkCur nextCadence
          newCounter = if H.isInversion nextCadence then 0 else nonInvCount + 1

      let emitState = newState

          -- Build diagnostics only when requested
      let diags = case mVerbosity of
            Nothing -> revDiags
            Just verbosity ->
              -- Candidates display: only chords actually present in finalPool
              -- (the list the gamma index sampled), so the trace never shows
              -- a chord that R or the advisory filters excluded this step.
              -- Membership by rendered form — Cadence has no Eq, and equal
              -- shows denote the same sonority. Provenance likewise: a
              -- selection is "graph" iff the chord exists among the graph
              -- candidates (indexing against the pre-filter graphCount
              -- mislabels whenever a soft filter shrank the graph segment).
              let finalShows = map (show . fst) finalPool
                  graphShows = map (show . fst) graphCandidates
                  selectedFrom = if show nextCadence `elem` graphShows
                                   then "graph" else "fallback"
                  graphTop6 = take 6 [(s', conf) | (cad, conf) <- graphCandidates
                                                 , let s' = show cad, s' `elem` finalShows]
                  fallbackTop6' = take 6 [(s', score, cd, md, gd) | (cad, score, cd, md, gd) <- fallbackAll
                                                                  , let s' = show cad, s' `elem` finalShows]
                  (renderedChord, transformTrace) = computeChordTrace verbosity emitState
                  priorRoot = H.stateCadenceRoot current
                  priorRootPC = P.unPitchClass (P.pitchClass priorRoot)
                  posteriorRoot = H.stateCadenceRoot emitState
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
                    , sdTristrataIdx = Nothing
                    , sdTristrata = Nothing
                    , sdStrataLabel = Nothing
                    , sdMode = Nothing
                    , sdStrataChroma = Nothing
                    , sdModeChroma = Nothing
                    , sdSoftBoost = Nothing
                    , sdHarmonicRootPC = Nothing
                    , sdParentKey = Nothing
                    , sdModeResult = Nothing
                    , sdBarSpelling = Nothing
                    , sdPoly = Nothing
                    }
              in diag : revDiags

      pure ((emitState, emitState : revChain, newCounter), diags)

-------------------------------------------------------------------------------
-- Scoring and Selection
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Consonance Fallback
-------------------------------------------------------------------------------

-- |Generate fallback candidates from the pre-parsed context filters.
--
-- This implements the legacy "constructive generation" pattern:
--   1. Take the effective overtone palette (tuning filtered by key)
--   2. Generate all valid triads from roots × overtones (660 structures with wildcard)
--   3. Compute actual movement from current state to each candidate
--   4. Score with multiplicative formula: (rootMotionDiss × structureDiss × (gammaDraw+1))
--   5. Sort by score (lower badness = higher score)
--
-- Movement computation matches legacy getCadenceOptions which uses:
--   toCadence (transposeCadence enharm rootPC prev, nxt)
-- to derive proper movements from current position to each candidate.
-- This ensures fallback cadences have real movements, enabling subsequent
-- iterations to find graph matches and traverse freely.
--
-- Reads 'pcSoftBoost' from the context and applies it multiplicatively to
-- @badness@ inside 'computeFallbackScoreWithBoost'. Values < 1.0 favour the
-- candidates (lower badness, higher score); = 1.0 is the no-op default.
--
-- Returns IO [(Cadence, score, chordDiss, motionDiss, gammaDraw)]
consonanceFallbackParsed :: GenIO -> H.CadenceState -> ParsedContext -> IO [(H.Cadence, Double, Double, Double, Double)]
consonanceFallbackParsed gen currentState pctx =
  let currentRoot = P.pitchClass (H.stateCadenceRoot currentState)
      effectiveOvertones = IntSet.toList (pcEffectiveOvertones pctx)
      -- No dedup needed: overtoneSets emits distinct nCr combinations per
      -- root and each triad's head is its root, so the list is duplicate-free
      -- by construction.
      triads = concatMap (\r -> overtoneSets 3 [r] effectiveOvertones) effectiveOvertones
      boost = pcSoftBoost pctx
  in do
      results <- mapM (\t -> do
                         let cad = triadToCadenceFrom currentRoot t
                         (score, cd, md, gd) <- computeFallbackScoreWithBoost gen currentRoot cad t boost
                         pure (cad, score, cd, md, gd)
                       ) triads
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
  let triadRoot = P.mkPitchClass (case pitches of { (p : _) -> p; [] -> 0 })  -- First element from overtoneSets is the root
      movement = H.toMovement currentRoot triadRoot
      -- Zero-form normalization: [P 4,P 7,P 11] → [P 0,P 3,P 7]
      -- zeroFormPC subtracts first element and sorts, so don't pre-sort!
      pcs = H.zeroFormPC (map P.mkPitchClass pitches)
      functionality = H.toFunctionality pcs
  in H.Cadence functionality movement pcs

-------------------------------------------------------------------------------
-- Fallback Scoring
-------------------------------------------------------------------------------

-- |Compute multiplicative fallback score with stochastic perturbation.
-- Formula: badness = rootMotionDiss × structureDiss × (gammaDraw + 1)
--          score = 10000 - badness
--
-- Chord dissonance range: 6 (major\/minor triad) to ~50 (dense cluster)
-- Root motion range: 1 (P5\/P4) to 6 (tritone)
-- Gamma draw range: mostly ~0-3, occasionally larger (fixed shape=1.01,
-- near-exponential)
--
-- The multiplicative formula spreads scores organically based on:
--   * Root motion quality (smooth vs rough)
--   * Vertical consonance (simple vs complex)
--   * Stochastic perturbation (via gamma draw)
--
-- The gamma draw is DELIBERATE tie-breaking noise and is independent of
-- '_gcEntropy' by design: the entropy dial acts at selection time
-- ('gammaIndexScaledWith' over the ranked pool), not at scoring time. A
-- consequence to know about: the fallback ranking is stochastic per step
-- even at entropy 0.0, so offline generation retains run-to-run variety at
-- the deterministic end of the dial.
--
-- This prevents score clustering and eliminates the need for pool size limits.
-- Returns IO (finalScore, chordDiss, motionDiss, gammaDraw)
--
-- @boost@ is a multiplicative soft-boost on @badness@: values below 1.0
-- favour the candidate (lower badness → higher score); 1.0 is the no-op;
-- values above 1.0 disfavour. Used by 'Harmonic.Framework.Builder.genP' to
-- bias candidates toward strata\/tristrata continuity via 'pcSoftBoost'.
computeFallbackScoreWithBoost :: GenIO -> P.PitchClass -> H.Cadence -> [Int] -> Double -> IO (Double, Double, Double, Double)
computeFallbackScoreWithBoost gen _currentRoot cad triad boost = do
  -- Chord vertical dissonance (raw Hindemith score)
  let chordDiss = fromIntegral (dissonanceScore triad) :: Double

      -- Root motion dissonance (extract interval from Movement)
      interval = extractMovementInterval (H.cadenceMovement cad)
      motionDiss = fromIntegral (D.rootMotionScore interval) :: Double

  -- Draw gamma sample for entropy (minimum entropy: shape=1.01)
  gammaDraw <- Dist.gamma 1.01 1.0 gen

  -- Multiplicative badness: all three factors contribute, soft-boost
  -- applied as an additional multiplicative term.
  let badness = chordDiss * motionDiss * (gammaDraw + 1.0) * boost

      -- Final score: 10000 - badness (higher is better)
      finalScore = 10000.0 - badness

  pure (finalScore, chordDiss, motionDiss, gammaDraw)

-- |Extract interval class (0-6) from Movement type.
-- Maps Movement to interval class for rootMotionScore input.
-- Interval class folds intervals larger than tritone to their complement.
extractMovementInterval :: H.Movement -> Int
extractMovementInterval movement = case movement of
  H.Asc pc   -> intervalClassFromPC (P.unPitchClass pc)
  H.Desc pc  -> intervalClassFromPC (P.unPitchClass pc)
  H.Unison   -> 0
  H.Tritone  -> 6
  -- Empty is the missing-movement placeholder; no motion to score.
  H.Empty    -> 0
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

-- |Apply R constraints, with an optional bass target override.
-- When bassTarget is Just, only candidates whose bass matches the target pass.
applyRConstraintsWithTarget :: Maybe Int
                            -> ParsedContext
                            -> H.CadenceState
                            -> [(H.Cadence, a)]
                            -> [(H.Cadence, a)]
applyRConstraintsWithTarget bassTarget pctx currentState =
  filter (matchesContextWithTarget bassTarget pctx currentState . fst)

-- |Core filter with optional bass target override from rise\/fall direction.
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
      bassInt = case absolutePitches of { [] -> 0; (p : _) -> p }

      -- All absolute chord pitches must be in effective overtones (IntSet lookup).
      -- When bass direction targets a specific note, exempt that pitch class
      -- from the overtone check — allows chromatic passing bass notes
      -- (e.g. D# in a G major context) while still constraining upper voices.
      -- 'pcStrictContainment' (set by 'Harmonic.Framework.Builder.genP') disables the bass exemption so
      -- the candidate's bass must also lie in the narrowed overtone set.
      pitchesToCheck
        | pcStrictContainment pctx = absolutePitches
        | otherwise = case bassTarget of
            Just target -> filter (/= target) absolutePitches
            Nothing     -> absolutePitches
      overtonesMatch = all (`IntSet.member` pcEffectiveOvertones pctx) pitchesToCheck

      -- Bass note check: exact target if rise\/fall active, otherwise set membership
      bassMatch = case bassTarget of
        Just target -> bassInt == target
        Nothing     -> pcIsRootsWild pctx
                       || bassInt `IntSet.member` pcAllowedBassNotes pctx

  in overtonesMatch && bassMatch

-------------------------------------------------------------------------------
-- State Advancement
-------------------------------------------------------------------------------

-- |Advance the CadenceState with full trace of intermediate values
-- Used for maximum verbosity diagnostics (gen'')
-- Enharmonic spelling is inferred from the new chord's absolute pitch content
-- using the 3-layer inferSpelling system (3-set match → 2-set match → root fallback).
advanceStateTraced :: Maybe H.EnharmonicSpelling -> H.CadenceState -> H.Cadence -> (H.CadenceState, AdvanceTrace)
advanceStateTraced keyBias currentState newCadence =
  let currentRoot = H.stateCadenceRoot currentState
      currentRootPC = P.pitchClass currentRoot
      movement = H.cadenceMovement newCadence
      movementInterval = H.fromMovement movement
      newRootPC = currentRootPC + movementInterval
      -- Infer spelling from the new chord's absolute pitches
      tones = map P.unPitchClass $ H.cadenceIntervals newCadence
      absolutePitches = map (\t -> (t + P.unPitchClass newRootPC) `mod` 12) tones
      inferredSpelling = H.inferSpelling absolutePitches
      -- Spelling precedence:
      --   1. A declared key signature fixes the enharmonic side for the
      --      whole walk — a flat-side key never spells sharp.
      --   2. While the root pitch class stands still, the spelling stands
      --      still: per-bar re-inference alone can flip side when an upper
      --      tone changes over a stationary root.
      --   3. Enharmonically ambiguous patterns adopt the prior spelling.
      --   4. Otherwise, infer from absolute pitch content.
      newSpelling = case keyBias of
        Just ks -> ks
        Nothing
          | newRootPC == currentRootPC       -> H.stateSpelling currentState
          | H.isAmbiguousPattern absolutePitches -> H.stateSpelling currentState
          | otherwise                        -> inferredSpelling
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


-------------------------------------------------------------------------------
-- Context-aware starting cue
-------------------------------------------------------------------------------

-- |True when a state names as a root-position structure — its bass IS
-- its named root. Triad-sized states re-name through the display seam's
-- own chord builder ('H.fromCadenceState'), whose functionality carries
-- an @Inv@ tag for every recognised inversion shape (maj\/min\/dim first
-- and second inversions, the sus4 inversions) — the same tags that make
-- 'Harmonic.Rules.Types.Progression.showTriad' render a slash chord. A
-- bare @\'\/\'@ test would misfire: @sus2\/4@ names carry the character
-- without being slash chords. Extended states pass — no triad-inversion
-- vocabulary applies; the jazz cue filters its slash structures by node
-- key instead. Every random-cue pool filters on this: an uncued bar 1
-- never opens on a slash chord, and inversions enter a progression only
-- by the walk's choice or the caller's explicit cue.
rootPositionCue :: H.CadenceState -> Bool
rootPositionCue cs
  | length (H.cadenceIntervals (H.stateCadence cs)) > 3 = True
  | otherwise = case H.fromCadenceState cs of
      H.Chord _ fn _ -> not ("Inv" `isInfixOf` fn)

-- |Draw a random starting chord INSIDE the caller's tonal context: the
-- root from the resolved roots filter, the tones from the key-filtered
-- overtone set (via 'possibleTriads', the same pool builder the strata
-- cue uses), required pedal tones honoured where the pool allows —
-- and filtered to root-position structures via 'rootPositionCue'. A
-- fully wildcard context defers to the caller's '_gcCue' — plain
-- @seek "*" $ gen@ is unchanged. The gen \/ genE \/ grid counterpart of
-- 'Harmonic.Framework.Builder.StrataGen.strataStartCue': without it an
-- uncued walk under @hcKey "3b"@ opens out of key three draws in four
-- (only three major triads fit a seven-tone key set) and snaps into key
-- from bar 2 — the R constraints filter transitions, never bar 1.
--
-- The root keeps the walk's own bass exemption (@pcStrictContainment@
-- 'False'): a root named outside the key still cues, voiced over key
-- tones. An unsatisfiable context (empty pool even after the pedal
-- filter relaxes) falls back to the legacy draw rather than erroring —
-- bar 1 is the human aberration channel, never a wall.
tonalStartCue :: GenConfig -> IO H.CadenceState
tonalStartCue gc
  | pcIsKeyWild pctx && pcIsOvertonesWild pctx && pcIsRootsWild pctx
      && IntSet.null (pcPedalRequired pctx) = _gcCue gc
  | otherwise = do
      rng <- createSystemRandom
      let allowed = IntSet.toList (pcEffectiveOvertones pctx)
          roots | pcIsRootsWild pctx = allowed
                | otherwise          = IntSet.toList (pcAllowedBassNotes pctx)
          probe (r, ivs) =
            let nm = H.enharmonicFunc H.FlatSpelling (P.mkPitchClass r)
            in H.initCadenceState 0 (show nm) ivs
          pool0 = [ cand
                  | r <- roots
                  , pcs <- possibleTriads (r, nub (sort (r : allowed)))
                  , let cand = (r, [ (p - r) `mod` 12 | p <- pcs ])
                  , rootPositionCue (probe cand) ]
          pedalOk (r, ivs) =
            all (\p -> (p - r) `mod` 12 `elem` ivs)
                (IntSet.toList (pcPedalRequired pctx))
          pool = case filter pedalOk pool0 of
                   [] -> pool0
                   ps -> ps
      case pool of
        [] -> _gcCue gc
        _  -> do
          i <- uniformR (0, length pool - 1) rng
          let (rootPC, ivs) = pool !! i
              spelling = maybe H.FlatSpelling id (pcKeySpelling pctx)
              rootName = H.enharmonicFunc spelling (P.mkPitchClass rootPC)
          pure (H.initCadenceState 0 (show rootName) ivs)
  where
    pctx = parseContextOnce (_gcTonal gc)
