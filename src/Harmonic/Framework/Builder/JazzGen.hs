-- |
-- Module      : Harmonic.Framework.Builder.JazzGen
-- Description : genJ — walk generation over the jazz (Change) graph
--
-- The jazz counterpart of the triadic walk, decomposed R→E→T with every
-- shared concern imported from its home module rather than re-implemented:
--
-- * __R__ — the @Change@ graph's adjacency (no consonance fallback: the
--   corpus measures rich enough that none is needed — 0 dead ends,
--   ~219-candidate typical pools) intersected with the caller's
--   'HarmonicContext' via the same
--   'Harmonic.Framework.Builder.Core.matchesContextWithTarget' filter the
--   classical walk applies (it is arity-agnostic and anchors exactly as
--   jazz movement does). A step whose R-filter empties the pool relaxes
--   the filter for that step with a notice — there is no fallback pool
--   to fill from, and continuity beats a hard stop.
-- * __E__ — corpus frequency under the seek spec
--   ('Harmonic.Evaluation.Database.Query.resolveWeights'), plus the
--   classical steer boost when the spec names classical composers.
-- * __T__ — the shared gamma entropy dial
--   ('Harmonic.Traversal.Probabilistic.gammaIndexScaledWith').
--
-- == Seek semantics (genJ)
--
-- One spec drives both corpora, split by
-- 'Harmonic.Evaluation.Database.Query.splitSeekByCorpus':
--
-- * @"*"@ — aggregate corpus-frequency walk over the whole jazz graph.
-- * Jazz names (@"monk:60 coltrane:40"@) — composer-blend scoring over
--   jazz edge weights (substring-matched against jazz keys).
-- * Classical names (@"debussy"@) — the jazz walk runs on @"*"@ and each
--   step is steered: the current chord's most consonant embedded triad
--   queries the classical graph under that blend, and jazz candidates
--   containing one of the top recommended triads (arriving by the same
--   movement) are boosted by the 'Harmonic.Framework.Builder.steer'
--   strength.
-- * Mixed (@"monk debussy"@) — blend and steer together.
-- * @"none"@ — refused: the jazz graph IS the generator; no offline mode.
module Harmonic.Framework.Builder.JazzGen (
    runJazzGen,
    runJazzGenFrom,
    jazzGuardSeek,
    jazzStartCue,
) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Data.Char (toLower)
import           Data.List (sortBy)
import           Data.Ord (Down(..), comparing)
import           Control.Monad (when)
import           Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import           System.Random.MWC (GenIO, createSystemRandom, uniformRM)
import qualified Data.IntSet as IntSet

import           Harmonic.Database (DbConn, runDb)
import qualified Harmonic.Evaluation.Database.Query as Q
import qualified Harmonic.Rules.Import.Jazz as J
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as P
import qualified Harmonic.Rules.Types.Progression as Prog
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import           Harmonic.Rules.Import.Graph (connectNeo4j)
import           Harmonic.Framework.Builder.Types
import           Harmonic.Evaluation.Analysis.KeyArea (chordscale)
import           Harmonic.Framework.Builder.Core (matchesContextWithTarget, rootPositionCue)
import           Harmonic.Traversal.Probabilistic (gammaIndexScaledWith)

-- Everything one walk step needs, resolved once per generation.
data JazzEnv = JazzEnv
  { jeConn     :: DbConn
  , jeGen      :: GenIO
  , jeBlend    :: Q.ComposerWeights  -- ^ Jazz half of the seek spec.
  , jeSteer    :: Q.ComposerWeights  -- ^ Classical-steer half.
  , jeStrength :: Double             -- ^ Steer boost strength ('_gcSteer').
  , jeEntropy  :: Double
  , jePctx     :: ParsedContext      -- ^ Parsed 'HarmonicContext' (R filter).
  , jeVerbose  :: Bool
  , jeSingle   :: Bool               -- ^ Single-pass run (no attempt loop): warnings may print immediately.
  , jeTrace    :: IORef [String]     -- ^ Walk-trace accumulator (reversed); drained into 'gdJazzTrace' so only the attempt winner's trace is emitted.
  }

-- Collect one trace line; emitFinalised prints the winner's collected
-- lines exactly once, restoring the emit-once contract under attempt.
trace' :: JazzEnv -> String -> IO ()
trace' env l = modifyIORef' (jeTrace env) (l :)

-- |Refuse @seek "none"@ before any Neo4j contact: the jazz graph IS the
-- generator — there is no offline fallback. Exported so the attempt loop
-- can guard before opening its scoring connection.
jazzGuardSeek :: GenConfig -> IO ()
jazzGuardSeek gc =
  when (map toLower (_gcSeek gc) == "none") $
    error "genJ: seek \"none\" has no meaning here — the jazz graph IS the generator (no offline fallback exists). Use seek \"*\" or a composer spec."

-- | Execute a 'JazzMode' 'GenConfig': fresh jazz generation. The cue is
-- honoured as bar 1 (the human aberration channel, as in every family)
-- and mapped onto a jazz start node; the walk fills the remaining bars.
-- Output is stamped 'PC.FJazz'.
runJazzGen :: GenConfig -> IO (PC.ProgressionContext, GenerationDiagnostics)
runJazzGen gc = do
  env  <- mkEnv gc
  cue0 <- if _gcCueExplicit gc then _gcCue gc else jazzStartCue gc
  startKey <- resolveStart env cue0
  states <- jazzWalk env startKey cue0 (max 1 (_gcLen gc) - 1)
  traceLines <- drainTrace env
  let allStates = cue0 : states
      prog      = Prog.fromCadenceStates allStates
      pc        = chordscale ((PC.fromProgression prog) { PC.pcFamily = PC.FJazz })
  pure (pc, jazzDiag gc cue0 prog traceLines)

-- | Regenerate bars @s..e@ (1-indexed, wrap-aware; @len@ expands the
-- range like the classical 'Harmonic.Framework.Builder.genFrom') of a
-- jazz-family source in place. The cue is the bar before @s@ (or the
-- caller's override); regenerated bars are spliced back via
-- 'PC.pcSplice', whose triad-layer path applies the standard seam
-- movement-fix — the bar after the range keeps its chord but its
-- movement metadata is corrected to the actual arrival interval, exactly
-- the legacy-family regen contract.
runJazzGenFrom :: PC.ProgressionContext -> Int -> Int -> GenConfig
               -> IO (PC.ProgressionContext, GenerationDiagnostics)
runJazzGenFrom srcPC s _e gc = do
  env  <- mkEnv gc
  cue0 <- _gcCue gc
  startKey <- resolveStart env cue0
  let n     = PC.pcLength srcPC
      rSize = max 1 (_gcLen gc)
      effE  = ((s - 1 + rSize - 1) `mod` n) + 1
  newBars <- jazzWalk env startKey cue0 rSize
  traceLines <- drainTrace env
  let insertPC = (PC.fromProgression (Prog.fromCadenceStates newBars))
                   { PC.pcFamily = PC.FJazz }
      -- Rebuild from the movement-fixed triad splice (pcSplice would leave
      -- stale movement metadata on the duplicated layers), then re-derive
      -- the chordscale S/M layers over the WHOLE spliced progression — key
      -- boundaries are a global property, so a regen recomputes them.
      spliced  = chordscale ((PC.fromProgression
                    (PC.triadLayer (PC.pcSplice srcPC s effE insertPC)))
                   { PC.pcFamily = PC.FJazz })
  pure (spliced, jazzDiag gc cue0 (PC.triadLayer spliced) traceLines)

-- Reverse-accumulated trace back into emission order.
drainTrace :: JazzEnv -> IO [String]
drainTrace env = Prelude.reverse <$> readIORef (jeTrace env)

-- Shared construction of the per-generation environment: connection,
-- RNG, seek split (with the Verbose resolution report), parsed tonal
-- context.
mkEnv :: GenConfig -> IO JazzEnv
mkEnv gc = do
  jazzGuardSeek gc
  conn <- connectNeo4j
  gen  <- createSystemRandom
  traceRef <- newIORef []
  let blend   = Q.parseComposerWeights (T.pack (_gcSeek gc))
      verbose = _gcVerbosity gc /= Silent
  (jazzBlend, steerBlend) <-
    if Map.null blend
      then pure (Map.empty, Map.empty)
      else do
        keys <- runDb conn Q.fetchJazzComposers
        pure (Q.splitSeekByCorpus keys blend)
  let env = JazzEnv
        { jeConn = conn, jeGen = gen
        , jeBlend = jazzBlend, jeSteer = steerBlend
        , jeStrength = _gcSteer gc
        , jeEntropy  = _gcEntropy gc
        , jePctx     = parseContextOnce (_gcTonal gc)
        , jeVerbose  = verbose
        , jeSingle   = _gcMaxAttempts gc <= 1
        , jeTrace    = traceRef
        }
  when (verbose && not (Map.null blend)) $ do
    trace' env $ "genJ seek resolution: jazz blend " ++ showBlend jazzBlend
               ++ " | classical steer " ++ showBlend steerBlend
    when (Map.null jazzBlend) $
      trace' env "  (no jazz composers named — jazz walk runs on \"*\", classical names steer)"
  pure env
  where
    showBlend m
      | Map.null m = "(none)"
      | otherwise  = show [ (T.unpack k, v) | (k, v) <- Map.toList m ]

-- Map a cue state onto a jazz start node: exact (movement, name) key if
-- the graph has it, else best node for the bare functionality, else the
-- corpus's workhorse ( pedal -> m7 ).
resolveStart :: JazzEnv -> H.CadenceState -> IO T.Text
resolveStart env cue0 = do
  let conn    = jeConn env
      cueSet  = map P.unPitchClass (H.cadenceIntervals (H.stateCadence cue0))
      cueMv   = H.cadenceMovement (H.stateCadence cue0)
      exactKey nm = "( " <> T.pack (show cueMv) <> " -> " <> nm <> " )"
  case J.jazzFunctionality cueSet of
    Nothing -> do
      let notice = "genJ: cue chord is outside the jazz vocabulary — starting from ( pedal -> m7 )"
      if jeSingle env then putStrLn notice else trace' env notice
      orDefault conn (pure Nothing)
    Just nm -> do
      probe <- runDb conn (Q.fetchChangeAggregate (exactKey nm))
      if not (null probe)
        then pure (exactKey nm)
        else do
          when (jeVerbose env) $
            trace' env $ "genJ: no jazz node " ++ T.unpack (exactKey nm)
                       ++ " — resolving by functionality"
          orDefault conn (runDb conn (Q.resolveChangeCue nm))
  where
    orDefault conn action = do
      found <- action
      case found of
        Just k  -> pure k
        Nothing -> do
          fallback <- runDb conn (Q.resolveChangeCue "m7")
          maybe (error "genJ: jazz graph appears empty — run `stack run -- jazz` to ingest") pure fallback

-- The walk: n steps of fetch → blend → R-filter → steer → gamma pick.
jazzWalk :: JazzEnv -> T.Text -> H.CadenceState -> Int
         -> IO [H.CadenceState]
jazzWalk env = go
  where
    conn = jeConn env
    verbose = jeVerbose env
    go _ _ 0 = pure []
    go key prevState remaining = do
      -- E: candidates under the jazz blend (aggregate for wildcard).
      pool0 <- if Map.null (jeBlend env)
        then runDb conn (Q.fetchChangeAggregate key)
        else do
          cands <- runDb conn (Q.fetchChangeTransitions key)
          pure (filter ((> 0) . snd) (Q.resolveWeights (jeBlend env) cands))
      pool1 <- if not (null pool0) then pure pool0 else do
        when verbose $ trace' env $ "  genJ: composer subgraph dead-ends at "
                                  ++ T.unpack key ++ " — widening to \"*\" for this step"
        runDb conn (Q.fetchChangeAggregate key)
      when (null pool1) $
        error ("genJ: no outgoing transitions from " ++ T.unpack key)
      -- R: the caller's tonal context, applied by the classical filter
      -- (arity-agnostic; roots resolve from the previous anchor exactly
      -- as jazz movement semantics require). No fallback exists, so an
      -- emptied pool relaxes the filter for the step, with a notice.
      let asCadence c = H.Cadence "" (Q.ccMovement c) (map P.mkPitchClass (Q.ccSet c))
          rPassed = [ p | p@(c, _) <- pool1
                        , matchesContextWithTarget Nothing (jePctx env) prevState (asCadence c) ]
      pool2 <- if not (null rPassed) then pure rPassed else do
        when verbose $ trace' env $ "  genJ: tonal constraints exclude every candidate at "
                                  ++ T.unpack key ++ " — relaxed for this step"
        pure pool1
      -- E: classical steer.
      pool <- if Map.null (jeSteer env) then pure pool2 else do
        let triadKey = T.pack (show (H.stateCadence (H.walkTriadState prevState)))
        recs0 <- runDb conn (Q.fetchTransitions triadKey)
        -- Steer on the blend's strongest recommendations only: the top
        -- of the classical ranking is "what this composer would most
        -- likely do next"; matching their entire vocabulary would boost
        -- nearly everything and discriminate nothing.
        let recs = take 12 (filter ((> 0) . snd) (Q.resolveWeights (jeSteer env) recs0))
            maxW = maximum (1 : map snd recs)
            matches cand (rc, _) =
              fst (H.deconstructCadence rc) == Q.ccMovement cand
              && let triad = map P.unPitchClass (snd (H.deconstructCadence rc))
                 in length (filter (`elem` Q.ccSet cand) triad) == length triad
            boost (cand, sc) =
              (cand, sc * (1 + jeStrength env * sum [ w / maxW | r@(_, w) <- recs, matches cand r ]))
            boosted = map boost pool2
            nBoosted = length [ () | ((_, b), (_, o)) <- zip boosted pool2, b > o ]
        when verbose $
          trace' env $ "  steer: classical recs " ++ show (length recs)
                   ++ ", boosted " ++ show nBoosted ++ "/" ++ show (length pool2) ++ " candidates"
        pure (sortBy (comparing (Down . snd)) boosted)
      -- T: the shared entropy dial.
      idx <- gammaIndexScaledWith (jeGen env) (jeEntropy env) (length pool)
      let (cand, score) = pool !! idx
          prevRootPC = P.pitchClass (H.stateCadenceRoot prevState)
          nextRootPC = prevRootPC + H.fromMovement (Q.ccMovement cand)
          nextRoot   = P.flat nextRootPC
          nextState0 = H.mkCadenceStatePCs nextRoot (Q.ccMovement cand) (Q.ccSet cand)
          -- Rename with the jazz namer so grids show corpus vocabulary
          -- ("13sus4") rather than the generic chord namer's spelling.
          nextState  = case J.jazzFunctionality (Q.ccSet cand) of
            Just nm -> nextState0
              { H.stateCadence = (H.stateCadence nextState0)
                  { H.cadenceFunctionality = T.unpack nm } }
            Nothing -> nextState0
      when verbose $
        trace' env $ "  " ++ T.unpack key ++ " -> " ++ T.unpack (Q.ccShow cand)
                 ++ "  [pick " ++ show (idx + 1) ++ "/" ++ show (length pool)
                 ++ ", score " ++ show (fromIntegral (round (score * 100) :: Int) / 100 :: Double) ++ "]"
      rest <- go (Q.ccShow cand) nextState (remaining - 1 :: Int)
      pure (nextState : rest)

-- Minimal diagnostics record: the jazz walk renders its trace during
-- generation into 'gdJazzTrace'; gdSteps stays empty and emitFinalised
-- prints header + collected trace + grid — once, for the attempt winner.
jazzDiag :: GenConfig -> H.CadenceState -> Prog.Progression -> [String] -> GenerationDiagnostics
jazzDiag gc cue0 prog traceLines = GenerationDiagnostics
  { gdStartCadence = show (H.stateCadence cue0)
  , gdStartRoot    = show (H.stateCadenceRoot cue0)
  , gdRequestedLen = _gcLen gc
  , gdActualLen    = Prog.progLength prog
  , gdEntropy      = _gcEntropy gc
  , gdSteps        = []
  , gdProgression  = prog
  , gdJazzTrace    = traceLines
  }


-- |Draw an uncued 'Harmonic.Framework.Builder.genJ' starting chord from
-- the jazz graph itself: the STRUCTURE is sampled from the Change nodes
-- proportionally to outgoing corpus mass (the corpus's own sense of a
-- common departure point), and the ROOT comes from the caller's tonal
-- context — Change nodes are zero-form, so the two are independent.
-- Structure-in-key containment is preferred but relaxes when the
-- context admits no node whole, mirroring the walk's own per-step
-- relaxation; roots stay hard-filtered. Replaces the whole-corpus
-- random major triad, which is not jazz vocabulary at all — and which
-- @resolveStart@ then flagged as outside it on every uncued run.
jazzStartCue :: GenConfig -> IO H.CadenceState
jazzStartCue gc = do
  jazzGuardSeek gc
  conn <- connectNeo4j
  rng  <- createSystemRandom
  starts0 <- runDb conn Q.fetchChangeStarts
  when (null starts0) $
    error "genJ: jazz graph appears empty — run `stack run -- jazz` to ingest"
  -- Never open on a slash structure: chord-over-degree nodes (a "/" in
  -- the node key — 7/3, 69/4 …) stay walkable but are not departure
  -- points, and a triad-sized node that would re-name as an inversion
  -- is rejected the same way the classical pools reject it.
  let notSlash (cand, _) =
        not ("/" `T.isInfixOf` Q.ccShow cand)
        && rootPositionCue (H.mkCadenceStatePCs P.C (Q.ccMovement cand) (Q.ccSet cand))
      starts = case filter notSlash starts0 of
                 [] -> starts0
                 ss -> ss
      pctx = parseContextOnce (_gcTonal gc)
      keyConstrained = not (pcIsKeyWild pctx && pcIsOvertonesWild pctx)
      allowed = pcEffectiveOvertones pctx
      roots | not (pcIsRootsWild pctx) = IntSet.toList (pcAllowedBassNotes pctx)
            | keyConstrained           = IntSet.toList allowed
            | otherwise                = [0 .. 11]
      inCtx r cand =
        all (\i -> let p = (i + r) `mod` 12
                   in p == r || p `IntSet.member` allowed)
            (Q.ccSet cand)
      pool0 = [ ((cand, r), w) | (cand, w) <- starts, r <- roots ]
      pool  | not keyConstrained = pool0
            | otherwise = case [ p | p@((cand, r), _) <- pool0, inCtx r cand ] of
                            [] -> pool0
                            ps -> ps
      total = sum (map snd pool)
  case pool of
    [] -> error "genJ: no viable start (roots filter resolved to nothing?)"
    (p0 : _) -> do
      u <- uniformRM (0, total) rng
      let walkTo _ []  = fst p0
          walkTo _ [x] = fst x
          walkTo acc (x@(_, w) : xs)
            | acc + w >= u = fst x
            | otherwise    = walkTo (acc + w) xs
          (cand, rootPC) = walkTo 0 pool
          spelling = maybe H.FlatSpelling id (pcKeySpelling pctx)
          rootName = H.enharmonicFunc spelling (P.mkPitchClass rootPC)
      pure (H.mkCadenceStatePCs rootName (Q.ccMovement cand) (Q.ccSet cand))
