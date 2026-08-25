-- |
-- Module      : Harmonic.Framework.Builder
-- Description : Generative engine for harmonic progressions with unified diagnostics interface
--
-- This module implements the main generation loop that connects:
--
--   * R (Rules): HarmonicContext constraints via Filter module
--   * E (Evaluation): Database-derived composer probabilities + dissonance
--   * T (Traversal): gamma-distributed sampling over the graph walk
--
-- == Academic Lineage
--
-- /Data Science In The Creative Process/ (South, 2018): Wiggins' Creative
-- Systems Framework \<R,T,E\> as the architectural blueprint. The Builder
-- orchestrates the R→E→T pipeline where R constrains the search space,
-- E scores candidates via database-derived probabilities, and T selects
-- via gamma-distributed probabilistic traversal.
--
-- == Unified Generation Interface
--
-- The module provides three public generation functions with /identical type signatures/:
--
-- @
--   genSilent    :: CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression
--   genStandard  :: CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression
--   genVerbose   :: CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression
-- @
--
-- All three functions:
--   * Return @IO Progression@ (NOT tuples)
--   * Print diagnostics as /side effects/ based on verbosity level
--   * Enable seamless switching between verbosity levels without code changes
--
-- === Verbosity Levels
--
-- [0 - Silent] @genSilent@: No diagnostic output. Use when you only want the progression.
--
-- [1 - Standard] @genStandard@: Prints per-step diagnostics including:
--   * Prior and posterior cadence states
--   * Candidate pool composition (graph candidates, fallback candidates)
--   * Top candidates with scores
--   * Selected candidate source (graph or fallback)
--   * Rendered chord names
--
-- [2 - Verbose] @genVerbose@: Prints everything from Standard plus:
--   * TRANSFORM TRACE: Complete render pipeline (intervals, transposition, zero-form, naming)
--   * ADVANCE TRACE: Root motion computation with pitch class arithmetic
--   * Verification: DB stored name vs computed name
--
-- === Legacy Diagnostic Functions
--
-- For backward compatibility, the module still exports:
--   * @generate', gen', genWith'@ - Returns @(Progression, GenerationDiagnostics)@ tuple
--   * @generate'', gen'', genWith''@ - Returns @(Progression, GenerationDiagnostics)@ tuple with max diagnostics
--
-- Use these when you need to programmatically extract diagnostics rather than printing them.
--
-- == Score Composition Details
--
-- Fallback candidates are scored using the formula:
--
-- @
--   chordDiss     = Hindemith vertical dissonance (6-50 range)
--   motionDiss    = Root motion dissonance (1-6 range, vector-based)
--   gammaDraw     = Entropy-based random perturbation (~0-5, shape=1.01)
--
--   badness = chordDiss × motionDiss × (gammaDraw + 1)
--   score = 10000 - badness
-- @
--
-- The multiplicative formula spreads scores organically without artificial limits.
-- Full 660-candidate pool (12 roots × C(11,2) pairs) ensures maximum variety.
--
-- The database is treated as abstract\/pitch-agnostic. Root notes are
-- computed at runtime based on movement intervals from a user-defined
-- starting CadenceState.
--
-- == Filter Notation (from original README)
--
-- === Overtones\/Pitch Set Filter
-- * Fundamental pitches (derives overtones): @"E A D G"@ (bass tuning)
-- * Individual pitches with prime: @"E'"@ @"A'"@ @"A#'"@
-- * Combined: @"G E' A' A#'"@ (G overtones + E, A, A# pitches)
-- * Wildcard: @"*"@ (all pitches)
--
-- === Tonality (Key) Filter
-- * Key signature: @"bb"@, @"###"@, @"4b"@, @"0#"@
-- * Named key: @"C"@, @"G"@, @"F#"@, @"Bb"@
-- * Wildcard: @"*"@ (no key filtering)
--
-- === Root Notes Filter
-- * Pitches: @"E F# G"@
-- * Key signature: @"1b"@, @"#"@, @"##"@ (D major)
-- * Wildcard: @"*"@ (all roots)

module Harmonic.Framework.Builder
  ( -- * Modifier-Based Generation API
    gen
  , gen'
  , gen''
  , genGrid
  , genE
  , genE'
  , genE''
  , quad
  , genFrom
  , genFrom'
  , genFrom''

    -- * genP Paradigm (strata-first)
  , genP
  , genP'
  , genP''
  , genI,   genII,   genIII,   genIV,   genV,   genVI,   genVII,   genVIII,   genIX,   genX,   genXI
  , genI',  genII',  genIII',  genIV',  genV',  genVI',  genVII',  genVIII',  genIX',  genX',  genXI'
  , genI'', genII'', genIII'', genIV'', genV'', genVI'', genVII'', genVIII'', genIX'', genX'', genXI''

    -- * Generation Modifiers
  , cue
  , len
  , seek
  , entropy
  , tonal
  , relStrata
  , absStrata
  , sameBoost
  , flipBoost
  , triBoost
  , attempt
  , viability

    -- * Generation Configuration
  , GenConfig(..)
  , GenMode(..)
  , Verbosity(..)
  , defaultGenConfig
  , execGenConfig
  , execGenConfigPC
  , genJ, genJ', genJ''
  , steer

    -- * Positional Generation (legacy\/internal)
  , generate
  , generateWith
  , genWith

    -- * Positional Generation with Diagnostics
  , generate'
  , genWith'
  , generate''
  , genWith''

    -- * Positional Generation with Print Output
  , genPrint
  , genPrint'
  , genPrint''

    -- * Unified Positional Interface
  , genSilent
  , genSilent'
  , genStandard
  , genStandard'
  , genVerbose
  , genVerbose'
  , printDiagnostics

    -- * Diagnostics Types
  , StepDiagnostic(..)
  , GenerationDiagnostics(..)
  , TransformTrace(..)
  , AdvanceTrace(..)

    -- * Harmonic Context (R constraints)
  , HarmonicContext(..)
  , harmonicContext
  , hContext

    -- * Context Modifiers
  , Drift(..)
  , hcOvertones
  , hcKey
  , hcRoots
  , dissonant
  , consonant
  , invSkip
  , hcPedal
  , hcTristrata

    -- * Configuration
  , GeneratorConfig(..)
  , defaultConfig

    -- * Internal functions (exposed for testing)
  , parseComposersWithOrder
  , makePortmanteau
  , extractByPosition
  , takeFromBeginning
  , takeFromEnd
  , takeFromMiddle
  , printHeader
  ) where

import           Harmonic.Database (DbActionT, runDb)
import qualified Data.Text as T
import           Data.Text (Text)
import           Control.Monad (forM_, when)
import           Data.Char (toLower)
import           Data.List (intercalate)
import           System.Random.MWC (createSystemRandom)

import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as P
import qualified Harmonic.Rules.Types.Progression as Prog
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import           Harmonic.Rules.Import.Graph (connectNeo4j)
import qualified Harmonic.Evaluation.Scoring.Progression as PS
import           Control.Monad.IO.Class (liftIO)
import           Harmonic.Rules.Constraints.Filter (parseTuningNamed)
import           Harmonic.Rules.Constraints.Overtone (formatOvertoneAnnotationPipe)
import           Data.Foldable (toList)
import           Data.List (nub)
import qualified Data.IntSet as IntSet
import qualified Data.Sequence as Seq

-- Sub-module imports
import           Harmonic.Framework.Builder.Types
import           Harmonic.Framework.Builder.Portmanteau
import           Harmonic.Framework.Builder.Diagnostics
import           Harmonic.Framework.Builder.Core
import           Harmonic.Framework.Builder.Modifiers
import           Harmonic.Framework.Builder.StrataGen (runStrataGen, runStrataGenFrom)
import           Harmonic.Framework.Builder.JazzGen (runJazzGen, runJazzGenFrom)

-------------------------------------------------------------------------------
-- Mode Display
-------------------------------------------------------------------------------

-- |Format the generation mode line for diagnostic output headers.
-- Shows offline status or the distinct composer names used for online generation.
composerModeStr :: String -> String
composerModeStr s
  | map toLower s == "none" = "Mode: offline (fallback only — no graph)"
  | s == "*"                = "Mode: online (composers: all)"
  | otherwise               = "Mode: online (composers: " ++ names ++ ")"
  where
    names = intercalate ", " (map (T.unpack . fst) (parseComposersWithOrder (T.pack s)))

-------------------------------------------------------------------------------
-- Main Generation Function
-------------------------------------------------------------------------------

-- |Generate a harmonic progression from a starting state.
--
-- Arguments:
--   * start: Initial CadenceState (defines starting root and quality)
--   * len: Number of chords to generate
--   * composerStr: Composer blend string ("bach:70 debussy:30")
--   * entropy: Gamma shape parameter (higher = more unusual choices)
--   * context: HarmonicContext filters (R constraints)
--
-- Algorithm:
--   1. Parse composer weights
--   2. For each step: query graph, apply R filter, apply E weights, gamma select
--   3. Apply voice leading optimization to the complete chain
--
-- Returns: Progression type (Phase B)
generate :: H.CadenceState       -- ^ Starting state (root + quality)
         -> Int                  -- ^ Number of chords
         -> Text                 -- ^ Composer blend string
         -> Double               -- ^ Entropy (gamma shape)
         -> HarmonicContext      -- ^ R constraints
         -> IO Prog.Progression
generate start nBars composerStr ent context =
  generateWith defaultConfig start nBars composerStr ent context

-------------------------------------------------------------------------------
-- String-Friendly Generation (TidalCycles Interface)
-------------------------------------------------------------------------------

-- |Positional generate with header + grid output (internal).
genPrint :: H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genPrint start nBars composerStr ent ctx = do
  (prog, _diag) <- generate' start nBars composerStr ent ctx
  putStrLn ""
  printHeader (T.pack composerStr) ent ctx
  print prog
  putStrLn ""
  pure prog

-- |String-friendly generateWith for TidalCycles live coding.
genWith :: GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genWith config start nBars composerStr ent ctx = generateWith config start nBars (T.pack composerStr) ent ctx

-- |Generate with custom configuration
--
-- Simplified algorithm:
--   1. Start with user-provided CadenceState
--   2. For each step: build candidate pool, gamma-select next cadence
--   3. Candidate pool = graph transitions (filtered) + consonanceFallback
--      (unlimited — the pool is never truncated)
generateWith :: GeneratorConfig
             -> H.CadenceState
             -> Int
             -> Text
             -> Double
             -> HarmonicContext
             -> IO Prog.Progression
generateWith config start nBars composerStr ent context = do
  let pctx = parseContextOnce context
  rng <- createSystemRandom
  source <- sourceFor composerStr
  (chain, _) <- buildChainWith source config rng Nothing ent context (const pctx) start (nBars - 1)
  pure $ chainToProgression chain

-------------------------------------------------------------------------------
-- Generation with Diagnostics (Verbosity 1)
-------------------------------------------------------------------------------

-- |Generate a progression returning both result and diagnostics (internal).
--
-- This is the core internal function that generates progressions and collects
-- standard-level diagnostics (per-step candidate pools, selections, rendered chords).
--
-- For most users, use the unified interface instead:
--   * 'genSilent' - for silent generation
--   * 'genStandard' - for standard diagnostics
--   * 'genVerbose' - for verbose diagnostics
generate' :: H.CadenceState -> Int -> String -> Double -> HarmonicContext
          -> IO (Prog.Progression, GenerationDiagnostics)
generate' start nBars composerStr ent ctx =
  genWith' defaultConfig start nBars composerStr ent ctx

-- |Positional generate with compact musical summary (internal).
genPrint' :: H.CadenceState -> Int -> String -> Double -> HarmonicContext
          -> IO Prog.Progression
genPrint' start nBars composerStr ent ctx = do
  (prog, diag) <- generate' start nBars composerStr ent ctx
  renderStandardSteps show composerStr ctx diag
  putStrLn ""
  printHeader (T.pack composerStr) ent ctx
  print prog
  putStrLn ""
  pure prog

-- |Per-step Standard renderer extracted from 'genPrint''. Emits the
-- compact summary + per-step lines, terminated by the trailing ━ rule.
-- Does NOT print the final header + grid; callers do that.
-- The first argument maps a trace position (1 = the starting\/seed row,
-- 2.. = walked bars) to its displayed bar number — identity ('show') for
-- fresh generation; regen paths map positions onto SOURCE bar numbers so
-- the trace rows line up with the printed full grid.
renderStandardSteps :: (Int -> String) -> String -> HarmonicContext -> GenerationDiagnostics -> IO ()
renderStandardSteps barLabel composerStr ctx diag = do
  let tuningNames = parseTuningNamed (_hcOvertones ctx)
      hasAnnotation = not (null tuningNames)
      allStates = toList (Prog.unProgression (gdProgression diag))
      annotateState cs =
        let rootPC = P.unPitchClass (P.pitchClass (H.stateCadenceRoot cs))
            intervals = map P.unPitchClass (H.cadenceIntervals (H.stateCadence cs))
            absPitches = map (\i -> (i + rootPC) `mod` 12) intervals
            spelling = H.stateSpelling cs
            pcName pc = show (H.enharmonicFunc spelling (P.mkPitchClass pc))
        in formatOvertoneAnnotationPipe tuningNames absPitches pcName

  putStrLn ""
  putStrLn $ "Generation: " ++ gdStartRoot diag ++ " " ++ gdStartCadence diag
             ++ " → " ++ show (gdActualLen diag) ++ " chords (ent " ++ show (gdEntropy diag) ++ ")"
  putStrLn $ composerModeStr composerStr
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  let bar1Suffix = case allStates of
        (a0 : _) | hasAnnotation ->
          let ann = annotateState a0
          in if null ann then "" else "  " ++ ann
        _ -> ""
  putStrLn $ "  " ++ barLabel 1 ++ ": " ++ gdStartRoot diag ++ " " ++ gdStartCadence diag
             ++ " [starting state]" ++ bar1Suffix
  putStrLn ""

  forM_ (gdSteps diag) $ \step -> do
    let barNum = sdStepNumber step + 1
        stateInfo = sdPriorRoot step ++ " → " ++ sdPosteriorRoot step
        poolInfo = "[" ++ show (sdGraphCount step) ++ "G/"
                   ++ show (sdFallbackCount step) ++ "F]"
        mvmt = sdSelectedDbMovement step
        chord = case sdRenderedChord step of
                  Just c -> c
                  Nothing -> sdPosteriorRoot step
        src = "[" ++ sdSelectedFrom step ++ "]"
        selIdx = "γ=" ++ show (sdGammaIndex step)

    let overtoneSuffix =
          if hasAnnotation
          then let stateIdx = barNum - 1
               in if stateIdx >= 0 && stateIdx < length allStates
                  then let ann = annotateState (allStates !! stateIdx)
                       in if null ann then "" else "  " ++ ann
                  else ""
          else ""

    putStrLn $ "  " ++ barLabel barNum ++ ": " ++ stateInfo ++ "  " ++ poolInfo
               ++ "  " ++ mvmt ++ " → " ++ chord ++ "  " ++ src ++ " " ++ selIdx
               ++ overtoneSuffix

    let posteriorRootPC = sdPosteriorRootPC step
        renderCandidateName name =
          case parseCadenceFromString name posteriorRootPC of
            Just renderedName -> renderedName
            Nothing -> name

    let topCands = if sdSelectedFrom step == "graph"
                   then take 6 (sdGraphTop6 step)
                   else take 6 [(n, s) | (n, s, _, _, _) <- sdFallbackTop6 step]

    when (not (null topCands)) $ do
      let candNames = [renderCandidateName name | (name, _) <- topCands]
          candStr = intercalate " | " candNames
      putStrLn $ "     Candidates: " ++ candStr

    -- genE: one additive line describing the added-tone draw. The
    -- Candidates line above stays triad-stage only (final-pool members).
    case sdFusion step of
      Just fd -> do
        let spelling = case toList (Prog.unProgression (gdProgression diag)) of
              css | barNum - 1 < length css -> H.stateSpelling (css !! (barNum - 1))
              _ -> H.FlatSpelling
            toneName = show (H.enharmonicFunc spelling (P.mkPitchClass (fdAddedPC fd)))
        putStrLn $ "     fused: +" ++ toneName
                   ++ " → " ++ sdPosteriorRoot step ++ " " ++ fdFusedName fd
                   ++ "  [rank " ++ show (fdGammaIdx fd + 1) ++ "/" ++ show (fdPoolK fd) ++ "]"
      Nothing -> pure ()

    putStrLn ""

  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

-- |Generate with custom configuration, returning diagnostics tuple (internal).
genWith' :: GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext
         -> IO (Prog.Progression, GenerationDiagnostics)
genWith' = genWithV (Just 1)

-- |Shared body of the diagnostic-collecting generators, with the level as
-- an argument: @Nothing@ collects no per-step diagnostics at all (the
-- Silent path — skips the per-step trace construction entirely),
-- @Just 1@ Standard, @Just 2@ Verbose (full transform\/advance traces).
genWithV :: Maybe Int -> GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext
         -> IO (Prog.Progression, GenerationDiagnostics)
genWithV mLevel config start nBars composerStr ent context = do
  let pctx = parseContextOnce context
  rng <- createSystemRandom
  source <- sourceFor (T.pack composerStr)
  (chain, stepDiags) <- buildChainWith source config rng mLevel ent context (const pctx) start (nBars - 1)
  let prog = chainToProgression chain
      diag = GenerationDiagnostics
        { gdStartCadence = show (extractCadence start)
        , gdStartRoot = show (H.stateCadenceRoot start)
        , gdRequestedLen = nBars
        , gdActualLen = Prog.progLength prog
        , gdEntropy = ent
        , gdSteps = stepDiags
        , gdProgression = prog
        }
  pure (prog, diag)

-------------------------------------------------------------------------------
-- Generation with Maximum Diagnostics (Verbosity 2)
-------------------------------------------------------------------------------

-- |Generate a progression with maximum diagnostic traces (internal).
--
-- Like 'generate\'' but populates full transform and advance traces for debugging.
-- This function collects:
--   * All standard diagnostics (per-step candidate pools, selections)
--   * Full transform traces (DB intervals, transposition, normalization, zero-form)
--   * Full advance traces (root motion PC arithmetic, enharmonic spelling)
--
-- This is SLOWER than 'generate\'' due to extra tracing computation.
-- Use only for debugging chord name discrepancies or voice leading issues.
generate'' :: H.CadenceState -> Int -> String -> Double -> HarmonicContext
           -> IO (Prog.Progression, GenerationDiagnostics)
generate'' start nBars composerStr ent ctx =
  genWith'' defaultConfig start nBars composerStr ent ctx

-- |Positional generate with verbose traces (internal).
genPrint'' :: H.CadenceState -> Int -> String -> Double -> HarmonicContext
           -> IO Prog.Progression
genPrint'' start nBars composerStr ent ctx = do
  (prog, diag) <- generate'' start nBars composerStr ent ctx
  renderVerboseSteps show composerStr diag
  putStrLn ""
  printHeader (T.pack composerStr) ent ctx
  print prog
  putStrLn ""
  pure prog

-- |Per-step Verbose renderer extracted from @genPrint'''@. Emits the
-- verbose summary + per-step trace, terminated by the trailing ━ rule.
-- Does NOT print the final header + grid; callers do that.
-- First argument as in 'renderStandardSteps': trace position → displayed
-- bar number.
renderVerboseSteps :: (Int -> String) -> String -> GenerationDiagnostics -> IO ()
renderVerboseSteps barLabel composerStr diag = do
  putStrLn ""
  putStrLn $ "Verbose Generation: " ++ gdStartRoot diag ++ " " ++ gdStartCadence diag
             ++ " → " ++ show (gdActualLen diag) ++ " chords (ent " ++ show (gdEntropy diag) ++ ")"
  putStrLn $ composerModeStr composerStr
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  putStrLn $ "STEP " ++ barLabel 1 ++ ": " ++ gdStartRoot diag ++ " " ++ gdStartCadence diag ++ " [starting state]"
  putStrLn ""

  forM_ (gdSteps diag) $ \step -> do
    let barNum = sdStepNumber step + 1
        mvmt = sdSelectedDbMovement step
        chord = case sdRenderedChord step of
                  Just c -> c
                  Nothing -> sdPosteriorRoot step
        src = "[" ++ sdSelectedFrom step ++ "]"
        selIdx = "(γ=" ++ show (sdGammaIndex step) ++ "/" ++ show (sdPoolSize step) ++ ")"

    putStrLn $ "STEP " ++ barLabel barNum ++ ": " ++ sdPriorRoot step ++ " → "
               ++ sdPosteriorRoot step ++ "  " ++ mvmt ++ " → " ++ chord ++ " " ++ src ++ " " ++ selIdx

    putStrLn $ "  Pool: " ++ show (sdGraphCount step) ++ " graph, "
               ++ show (sdFallbackCount step) ++ " fallback"

    when (sdSelectedFrom step == "graph" && not (null (sdGraphTop6 step))) $ do
      putStrLn "  Top graph:"
      forM_ (take 6 (sdGraphTop6 step)) $ \(name, conf) -> do
        putStrLn $ "    " ++ name ++ " (" ++ show conf ++ ")"

    when (sdSelectedFrom step == "fallback" && not (null (sdFallbackTop6 step))) $ do
      putStrLn "  Top fallback:"
      forM_ (take 6 (sdFallbackTop6 step)) $ \(name, score, chordD, motionD, gammaD) -> do
        putStrLn $ "    " ++ name ++ " (" ++ show score
                   ++ ", c=" ++ show chordD
                   ++ ", m=" ++ show motionD
                   ++ ", γ=" ++ show gammaD ++ ")"
    case sdAdvanceTrace step of
      Just at -> do
        putStrLn $ "  Advance: " ++ atCurrentRoot at ++ " (" ++ show (atCurrentRootPC at) ++ ")"
                   ++ " + " ++ show (atMovementInterval at) ++ " → "
                   ++ atNewRoot at ++ " (" ++ show (atNewRootPC at) ++ ")"
      Nothing -> return ()

    putStrLn ""

  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

-- |Generate with custom configuration and maximum diagnostics (internal).
genWith'' :: GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext
          -> IO (Prog.Progression, GenerationDiagnostics)
genWith'' = genWithV (Just 2)

-------------------------------------------------------------------------------
-- Unified Interface
-------------------------------------------------------------------------------

-- |Generate a progression with NO diagnostic output (verbosity 0 - silent mode).
genSilent :: H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genSilent start nBars composerStr ent ctx = do
  (prog, _diag) <- generate' start nBars composerStr ent ctx
  pure prog

-- |Generate a progression with STANDARD diagnostic output (verbosity 1).
genStandard :: H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genStandard start nBars composerStr ent ctx = do
  (prog, diag) <- generate' start nBars composerStr ent ctx
  printDiagnostics 1 diag
  pure prog

-- |Generate a progression with VERBOSE diagnostic output (verbosity 2).
genVerbose :: H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genVerbose start nBars composerStr ent ctx = do
  (prog, diag) <- generate'' start nBars composerStr ent ctx
  printDiagnostics 2 diag
  pure prog

-- |Silent mode with custom 'GeneratorConfig'.
genSilent' :: GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genSilent' config start nBars composerStr ent ctx = do
  (prog, _diag) <- genWithV Nothing config start nBars composerStr ent ctx
  pure prog

-- |Standard diagnostics with custom 'GeneratorConfig'.
genStandard' :: GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genStandard' config start nBars composerStr ent ctx = do
  (prog, diag) <- genWith' config start nBars composerStr ent ctx
  printDiagnostics 1 diag
  pure prog

-- |Verbose diagnostics with custom 'GeneratorConfig'.
genVerbose' :: GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genVerbose' config start nBars composerStr ent ctx = do
  (prog, diag) <- genWith'' config start nBars composerStr ent ctx
  printDiagnostics 2 diag
  pure prog

-------------------------------------------------------------------------------
-- Modifier-Based Generation API
-------------------------------------------------------------------------------

-- |Per-step diagnostic collection level for a 'Verbosity': Silent
-- collects nothing (nothing would render it), Standard the step trace,
-- Verbose the full transform\/advance traces.
diagLevelOf :: Verbosity -> Maybe Int
diagLevelOf Silent   = Nothing
diagLevelOf Standard = Just 1
diagLevelOf Verbose  = Just 2

-- |Execute a 'GenConfig', producing a progression.
--
-- Thin wrapper that calls @execGenConfigWithDiag@ (pure compute) and then
-- emits the appropriate diagnostics + header + grid via @emitFinalised@.
-- Single-pass callers see byte-identical output to today.
execGenConfig :: GenConfig -> IO Prog.Progression
execGenConfig gc = do
  (prog, diag) <- execGenConfigWithDiag gc
  emitFinalised gc (PC.fromProgression prog, diag)
  pure prog

-- |Compute-only variant of 'execGenConfig'. Returns the progression and
-- its diagnostics without printing anything. Used by 'singlePassExecPCWithDiag'
-- and by @generateBest@ inside the K-attempt loop so per-attempt output
-- can be suppressed and only the winner's emitted.
execGenConfigWithDiag :: GenConfig -> IO (Prog.Progression, GenerationDiagnostics)
execGenConfigWithDiag gc = do
  start0 <- _gcCue gc
  -- genE: fuse a triad cue once so the output is uniformly 4-note from
  -- bar 1 (user decision 2026-08-19). A 4-note lead' cue passes through
  -- untouched; sub-triad cues are left alone.
  start <- if _gcQuad gc
                && length (H.cadenceIntervals (H.stateCadence start0)) == 3
             then do
               rng <- createSystemRandom
               let pctx = parseContextOnce (_gcTonal gc)
               (fused, _) <- fuseState rng (_gcEntropy gc) pctx Nothing start0
               pure fused
             else pure start0
  let cfg = defaultConfig { gcQuad = _gcQuad gc }
  case _gcMode gc of
    JazzMode -> error "unreachable: JazzMode dispatches to runJazzGen before this case"
    FromProgJ {} -> error "unreachable: FromProgJ dispatches to runJazzGenFrom before this case"

    Fresh ->
      genWithV (diagLevelOf (_gcVerbosity gc)) cfg start (_gcLen gc)
               (_gcSeek gc) (_gcEntropy gc) (_gcTonal gc)

    GridMode -> do
      let grid = Prog.fromCadenceStates (replicate (_gcLen gc) start)
          diag = GenerationDiagnostics
            { gdStartCadence = show (H.stateCadence start)
            , gdStartRoot    = show (H.stateCadenceRoot start)
            , gdRequestedLen = _gcLen gc
            , gdActualLen    = Prog.progLength grid
            , gdEntropy      = _gcEntropy gc
            , gdSteps        = []
            , gdProgression  = grid
            }
      pure (grid, diag)

    FromProg srcProg s e -> do
      -- Family uniformity: regeneration produces states of the family the
      -- source already is. genFrom auto-detects (uniform 4-note source →
      -- _gcQuad); an EXPLICIT quad on a non-4-note source would create the
      -- family mixing regeneration must never produce → fail fast.
      let srcSizes = [ length (H.cadenceIntervals (H.stateCadence cs))
                     | cs <- toList (Prog.unProgression srcProg) ]
          srcQuad  = not (null srcSizes) && all (== 4) srcSizes
          srcMixed = length (nub srcSizes) > 1
      when (_gcQuad gc && not srcQuad) $
        error "genFrom is family-aware: this source is not a uniform 4-note (genE) progression — regenerate with plain genFrom (quad is inferred from the source)"
      when srcMixed $
        putStrLn "genFrom: hand-mixed source cardinalities — regenerating as plain triads (regen never amplifies mixing)"
      -- Generate _gcLen+1 chords (cue + new), then drop cue, splice into source.
      (fullProg, regenDiag) <- genWithV (diagLevelOf (_gcVerbosity gc)) cfg start (_gcLen gc + 1)
                                 (_gcSeek gc) (_gcEntropy gc) (_gcTonal gc)
      let newChords = drop 1 $ toList $ Prog.unProgression fullProg
          result    = Prog.spliceProgression srcProg s e newChords
      pure (result, regenDiag)

    -- Strata modes are handled by the PC-returning path; they should not
    -- reach this function. Defensive fallback retains the old Fresh
    -- behaviour rather than crashing.
    StrataMode _    -> generate' start (_gcLen gc) (_gcSeek gc) (_gcEntropy gc) (_gcTonal gc)
    FromProgPC {}   -> generate' start (_gcLen gc) (_gcSeek gc) (_gcEntropy gc) (_gcTonal gc)

-- |Set composer blend and execute. Terminal modifier — produces 'IO'
-- 'PC.ProgressionContext'. For legacy 'gen'-family configs, all three layers
-- duplicate the generated triad progression and @pcProvenance@ is 'Nothing';
-- the 'genP' paradigm produces distinct strata\/mode layers with 'Just'
-- provenance.
--
-- @s <- seek "*" $ gen@
-- @s <- seek "bach:70 debussy:30" $ cue start $ len 4 $ gen@
-- @s <- seek "none" $ cue start $ len 6 $ genVI@
seek :: String -> GenConfig -> IO PC.ProgressionContext
seek s gc = execGenConfigPC gc { _gcSeek = s }

-- |Terminal executor producing a 'PC.ProgressionContext'.
--
-- When @_gcMaxAttempts > 1@, dispatches through @generateBest@ for
-- rank-and-select multi-attempt generation; otherwise runs a single pass.
--
-- The single-pass dispatch reads '_gcMode': 'StrataMode' runs the strata-
-- first traversal producing distinct layers; all other modes fall through
-- to 'execGenConfig' and wrap the resulting triad 'Prog.Progression' via
-- 'PC.fromProgression'.
execGenConfigPC :: GenConfig -> IO PC.ProgressionContext
execGenConfigPC gc
  | _gcMaxAttempts gc > 1 = generateBest gc
  | otherwise             = singlePassExecPC gc

-- |Emit the final progression block — per-step diagnostics (if any) +
-- header + chord grid — under the caller's 'Verbosity' and 'GenMode'.
-- This is the single source of user-visible output for both the single-
-- pass path and the multi-attempt winner.
--
-- For strata modes ('StrataMode', 'FromProgPC') Standard\/Verbose use the
-- 'printStrataDiagnostics' renderer (legacy 'printDiagnostics' would mis-
-- render the strata trace). For legacy modes ('Fresh', 'GridMode',
-- 'FromProg') Standard uses 'renderStandardSteps' and Verbose uses
-- 'renderVerboseSteps' — matching the byte-for-byte output of the old
-- 'genPrint''/@genPrint''''@ wrappers.
--
-- The header + grid always reflect the full 'PC.triadLayer pc'. For
-- 'FromProgPC' \/ 'FromProg' that's the spliced result (full source
-- progression with regen bars inserted), not the regen segment alone.
emitFinalised :: GenConfig -> (PC.ProgressionContext, GenerationDiagnostics) -> IO ()
emitFinalised gc (pc, diag) = do
  -- Cue-escapes-R notice (non-fatal). The cue is the human aberration
  -- channel by design and is always honoured; this makes an escape visible
  -- at the moment it happens. Emitted here because emitFinalised runs
  -- exactly once per user invocation (single-pass, or the attempt winner),
  -- and the emitted progression's first state IS the cue actually used —
  -- resolving _gcCue again would re-draw the random default cue. Scope:
  -- Fresh\/GridMode only; the regen modes infer their cue from existing
  -- material, and the strata path has its own containment check.
  case _gcMode gc of
    Fresh    -> emitCueNotice
    GridMode -> emitCueNotice
    _        -> pure ()
  -- Typo'd filter tokens are named once per invocation — the parsers'
  -- silent-[] convention otherwise turns "hcRoots \"* -Bb'\"" with a bad
  -- token into an unconstrained walk with no signal. Printed at every
  -- verbosity: a wrong constraint is wrong even under Silent.
  mapM_ putStrLn (pcWarnings (parseContextOnce (_gcTonal gc)))
  let isStrata = case _gcMode gc of
        StrataMode _    -> True
        FromProgPC {}   -> True
        _               -> False
      isJazz = case _gcMode gc of
        JazzMode     -> True
        FromProgJ {} -> True
        _            -> False
  -- Regen traces describe only the regenerated segment (seed + new bars)
  -- while the grid below shows the FULL spliced progression. Mapping
  -- trace positions onto source bar numbers (wrap-aware) lets the reader
  -- line trace rows up with grid bars; position 1 is the seed bar.
  let nBars = Prog.progLength (PC.triadLayer pc)
      regenLabel s k = show (((s - 2 + k - 1) `mod` max 1 nBars) + 1)
      barLabel = case _gcMode gc of
        FromProg _ s _   -> regenLabel s
        FromProgPC _ s _ -> regenLabel s
        _                -> show
      regenNote = case _gcMode gc of
        FromProg _ s _ | _gcVerbosity gc /= Silent ->
          putStrLn (regenNoteStr s)
        FromProgPC _ s _ | _gcVerbosity gc /= Silent ->
          putStrLn (regenNoteStr s)
        _ -> pure ()
      regenNoteStr s =
        let rN   = max 0 (gdActualLen diag - 1)
            eEff = ((s - 2 + rN) `mod` max 1 nBars) + 1
        in "regenerated bars " ++ show s ++ ".." ++ show eEff ++ " of "
           ++ show nBars ++ " (trace numbers are source bars; first row is the seed)"
  regenNote
  case _gcVerbosity gc of
    _ | isJazz -> pure ()  -- genJ prints its own walk trace inline
    Silent   -> pure ()
    Standard -> if isStrata
                  then printStrataDiagnostics barLabel 1 diag
                  else renderStandardSteps barLabel (_gcSeek gc) (_gcTonal gc) diag
    Verbose  -> if isStrata
                  then printStrataDiagnostics barLabel 2 diag
                  else renderVerboseSteps barLabel (_gcSeek gc) diag
  putStrLn ""
  printHeader (T.pack (_gcSeek gc)) (_gcEntropy gc) (_gcTonal gc)
  print (PC.triadLayer pc)
  putStrLn ""
  where
    emitCueNotice =
      case Seq.viewl (Prog.unProgression (PC.triadLayer pc)) of
        firstState Seq.:< _ -> printCueEscapeNotice (_gcTonal gc) firstState
        Seq.EmptyL          -> pure ()

-- |Single-pass body of 'execGenConfigPC' — used directly when no multi-
-- attempt selection is requested. Thin wrapper that performs pure
-- generation via 'singlePassExecPCWithDiag' then emits the appropriate
-- diagnostics + header + grid via @emitFinalised@.
singlePassExecPC :: GenConfig -> IO PC.ProgressionContext
singlePassExecPC gc = do
  (pc, diag) <- singlePassExecPCWithDiag gc
  emitFinalised gc (pc, diag)
  pure pc

-- |Pure-compute variant of 'singlePassExecPC'. Returns the
-- 'Harmonic.Rules.Types.ProgressionContext.ProgressionContext' and its 'GenerationDiagnostics' without printing
-- anything. Used by the K-attempt loop inside @generateBest@ so per-
-- attempt output is suppressed and only the winner's emitted.
singlePassExecPCWithDiag :: GenConfig -> IO (PC.ProgressionContext, GenerationDiagnostics)
singlePassExecPCWithDiag gc = case _gcMode gc of
  -- Family separation: genE (quad) and the strata family (genP \/
  -- strata-aware genFrom) never mix — strata progressions stay 3-5-7.
  StrataMode _ | _gcQuad gc ->
    error "quad/genE applies to the gen family only — genP (strata) stays 3-5-7"
  FromProgPC {} | _gcQuad gc ->
    error "quad/genE applies to the gen family only — this source is strata-aware (genP provenance); regenerate it with plain genFrom (family is inferred from the source)"
  JazzMode | _gcQuad gc ->
    error "quad/genE applies to the gen family only — genJ (jazz) chords carry their own arity"
  FromProgJ {} | _gcQuad gc ->
    error "quad/genE applies to the gen family only — this source is jazz-family (regenerate with plain genFrom; family is inferred from the source)"
  JazzMode             -> runJazzGen gc
  FromProgJ srcPC s e  -> runJazzGenFrom srcPC s e gc
  StrataMode sStart    -> runStrataGen sStart gc
  FromProgPC srcPC s e -> runStrataGenFrom srcPC s e gc
  _                    -> do
    (prog, diag) <- execGenConfigWithDiag gc
    pure (PC.fromProgression prog, diag)

-- |Generate up to @_gcMaxAttempts@ progressions and return the single
-- highest-scoring one. An attempt is /viable/ iff
-- @psModeValidity >= 1.0@ (structural invariant — walk-generated
-- progressions always pass) AND @totalScore >= _gcViabilityFloor@. The
-- loop stops early once @_gcViableTarget@ viable attempts have been
-- collected, then returns the highest-scoring attempt across the full
-- accumulator (so when zero clear the floor, the best non-viable is
-- still returned).
--
-- When @_gcSeek != "none"@, scoring runs against Neo4j: one shared
-- database connection is opened for the entire K-attempt loop and @psCadenceFav@
-- is populated via 'PS.scoreProgressionOnline' under the user's composer
-- blend. The online-weighted total ('PS.defaultWeights') is then used —
-- cadence-favourability is the dominant axis (0.4).
--
-- When @_gcSeek == "none"@, scoring is fully pure and uses
-- 'PS.defaultWeightsOffline' (cadence-fav weight zeroed, the other three
-- renormalised).
generateBest :: GenConfig -> IO PC.ProgressionContext
generateBest gc0 = do
  -- Immediate user feedback before the K-attempt loop blocks. Tidal's
  -- GHCi stdout is line-buffered, so the newline flushes right away.
  putStrLn "composing .."
  -- One cue draw for the whole loop: the K attempts rank K walks from ONE
  -- starting chord. Without this, a random default cue re-draws per
  -- attempt and the scoreboard compares progressions in different keys.
  -- genE's cue fusion still runs per attempt, so the added tone keeps its
  -- per-attempt variety.
  start0 <- _gcCue gc0
  let gc = gc0 { _gcCue = pure start0 }
      online = map toLower (_gcSeek gc) /= "none"
      isJazzGc = case _gcMode gc of
        JazzMode        -> True
        FromProgJ {}    -> True
        _               -> False
  (winnerPC, winnerDiag, diags) <-
    if isJazzGc then runJazzBest gc
      else if online then runOnline gc else runOffline gc
  -- All per-attempt printing was suppressed inside the loop (Phase 11
  -- moved every emission into @emitFinalised@). Emit the winner exactly
  -- once, at the caller's verbosity.
  emitFinalised gc (winnerPC, winnerDiag)
  -- Verbose + multi-attempt: surface the full scoreboard.
  when (_gcVerbosity gc == Verbose && _gcMaxAttempts gc > 1) $
    printAttemptScoreboard (_gcViabilityFloor gc) diags
  pure winnerPC

-- |Offline arm of @generateBest@. Pure scoring with
-- 'PS.defaultWeightsOffline'.
--
-- The loop receives the caller's 'GenConfig' as-is — per-attempt
-- diagnostics are collected at the caller's verbosity, which the
-- winner's @emitFinalised@ then renders. No printing happens inside the
-- loop (Phase 11 lifted every emission out of 'singlePassExecPCWithDiag').
runOffline :: GenConfig
           -> IO (PC.ProgressionContext, GenerationDiagnostics, [AttemptDiagnostic])
runOffline gc = do
  let maxN   = max 1 (_gcMaxAttempts gc)
      target = max 1 (_gcViableTarget gc)
      floorT = _gcViabilityFloor gc
  scored <- offlineLoop gc maxN target floorT
  finaliseScored gc scored

-- |Online arm of @generateBest@. SCORING shares one connection and one
-- 'PS.TransitionCache' across the entire K-attempt loop (attempts 2..K
-- score almost query-free); each attempt's GENERATION still opens its own
-- connection inside the walk (an HTTP manager — cheap, and the walk owns
-- its lifecycle). Scores via 'PS.scoreProgressionOnline' under @_gcSeek@
-- as the composer blend; ranks via 'PS.defaultWeights'.
--
-- If Neo4j is unreachable, 'connectNeo4j' will surface the error directly
-- — matching the existing generation pipeline's behaviour for the same
-- condition. Users who want to bypass Neo4j entirely opt in via
-- @seek "none"@.
runOnline :: GenConfig
          -> IO (PC.ProgressionContext, GenerationDiagnostics, [AttemptDiagnostic])
runOnline gc = do
  let maxN    = max 1 (_gcMaxAttempts gc)
      target  = max 1 (_gcViableTarget gc)
      floorT  = _gcViabilityFloor gc
      seekTxt = T.pack (_gcSeek gc)
  pipe <- connectNeo4j
  cache <- PS.newTransitionCache
  scored <- runDb pipe (onlineLoop cache seekTxt gc maxN target floorT)
  finaliseScored gc scored

-- |Jazz arm of @generateBest@: identical loop shape to the online arm,
-- scored via 'PS.scoreProgressionJazz' — the same pure components, with
-- cadence favourability taken from the @Change@ graph under the seek
-- spec's jazz half. Ranked with 'PS.defaultWeights' (the jazz graph is
-- always online; @seek "none"@ was already refused by the walk).
runJazzBest :: GenConfig
            -> IO (PC.ProgressionContext, GenerationDiagnostics, [AttemptDiagnostic])
runJazzBest gc = do
  let maxN    = max 1 (_gcMaxAttempts gc)
      target  = max 1 (_gcViableTarget gc)
      floorT  = _gcViabilityFloor gc
      seekTxt = T.pack (_gcSeek gc)
  pipe <- connectNeo4j
  cache <- PS.newTransitionCache
  scored <- runDb pipe (jazzLoop cache seekTxt gc maxN target floorT)
  finaliseScored gc scored

-- |Inner loop for the jazz arm, run under 'runDb'.
jazzLoop
  :: PS.TransitionCache -> T.Text -> GenConfig -> Int -> Int -> Double
  -> DbActionT [ScoredAttempt]
jazzLoop cache seekTxt gc maxN target floorT = do
  jazzBlend <- PS.resolveJazzBlend seekTxt
  let go _ acc 0 = pure (reverse acc)
      go viableSoFar acc remaining
        | viableSoFar >= target = pure (reverse acc)
        | otherwise = do
            (pc, diag) <- liftIO (singlePassExecPCWithDiag gc)
            ps <- PS.scoreProgressionJazz cache jazzBlend pc
            let tot   = PS.totalScore PS.defaultWeights ps
                isOk  = viableAttempt pc ps && tot >= floorT
                acc'  = (pc, ps, tot, isOk, diag) : acc
                viable' = if isOk then viableSoFar + 1 else viableSoFar
            go viable' acc' (remaining - 1)
  go (0 :: Int) [] maxN

-- |Structural half of the viability gate: the mode invariant holds AND
-- the attempt actually produced bars. An invalid 'genP' cue yields an
-- empty progression whose pure axes all score their n<2 default of 1.0 —
-- without the length check it would rank as a perfect attempt.
viableAttempt :: PC.ProgressionContext -> PS.ProgressionScore -> Bool
viableAttempt pc ps =
  PS.psModeValidity ps >= 1.0 && Prog.progLength (PC.triadLayer pc) > 0

-- |Inner-loop record: per-attempt (progression, score, totalScore,
-- viability flag, diagnostics). The diagnostics are carried so the
-- winner's per-step trace can be re-emitted at the caller's verbosity
-- without re-running generation. The accumulator is kept in generation
-- order; index is assigned in 'finaliseScored' so the scoreboard
-- reflects the actual trial sequence.
type ScoredAttempt = (PC.ProgressionContext, PS.ProgressionScore, Double, Bool, GenerationDiagnostics)

-- |Inner loop for the offline arm. Calls 'singlePassExecPCWithDiag'
-- (no printing) so per-attempt output is fully suppressed; diagnostics
-- are collected at the caller's verbosity for the winner's later render.
offlineLoop
  :: GenConfig            -- ^ caller's config (printing already lifted out)
  -> Int                  -- ^ maxAttempts
  -> Int                  -- ^ viableTarget
  -> Double               -- ^ viabilityFloor
  -> IO [ScoredAttempt]
offlineLoop gc maxN target floorT = go 0 [] maxN
  where
    go _ acc 0 = pure (reverse acc)
    go viableSoFar acc remaining
      | viableSoFar >= target = pure (reverse acc)
      | otherwise = do
          (pc, diag) <- singlePassExecPCWithDiag gc
          let ps    = PS.scoreProgression pc
              tot   = PS.totalScore PS.defaultWeightsOffline ps
              isOk  = viableAttempt pc ps && tot >= floorT
              acc'  = (pc, ps, tot, isOk, diag) : acc
              viable' = if isOk then viableSoFar + 1 else viableSoFar
          go viable' acc' (remaining - 1)

-- |Inner loop for the online arm, run under 'runDb'.
onlineLoop
  :: PS.TransitionCache   -- ^ cross-attempt fetch cache
  -> T.Text               -- ^ seek string (composer blend)
  -> GenConfig            -- ^ caller's config (printing already lifted out)
  -> Int                  -- ^ maxAttempts
  -> Int                  -- ^ viableTarget
  -> Double               -- ^ viabilityFloor
  -> DbActionT [ScoredAttempt]
onlineLoop cache seekTxt gc maxN target floorT = go 0 [] maxN
  where
    go _ acc 0 = pure (reverse acc)
    go viableSoFar acc remaining
      | viableSoFar >= target = pure (reverse acc)
      | otherwise = do
          (pc, diag) <- liftIO (singlePassExecPCWithDiag gc)
          ps <- PS.scoreProgressionOnline cache seekTxt pc
          let tot   = PS.totalScore PS.defaultWeights ps
              isOk  = viableAttempt pc ps && tot >= floorT
              acc'  = (pc, ps, tot, isOk, diag) : acc
              viable' = if isOk then viableSoFar + 1 else viableSoFar
          go viable' acc' (remaining - 1)

-- |Shared post-loop: builds 'AttemptDiagnostic' values with index +
-- picked flag set on the maximum-totalScore attempt, and returns the
-- picked 'Harmonic.Rules.Types.ProgressionContext.ProgressionContext', its 'GenerationDiagnostics' (for the
-- caller to emit via @emitFinalised@), and the per-attempt diagnostic
-- list (for the scoreboard).
--
-- The empty-scored defensive branch falls back to a non-silenced
-- 'singlePassExecPCWithDiag', mirroring the prior behaviour where the
-- fallback would print under the caller's verbosity.
finaliseScored
  :: GenConfig
  -> [ScoredAttempt]
  -> IO (PC.ProgressionContext, GenerationDiagnostics, [AttemptDiagnostic])
finaliseScored gc scored = case scored of
  [] -> do
    (pc, diag) <- singlePassExecPCWithDiag gc
    pure (pc, diag, [])
  xs -> do
    let indexed = zip [1..] xs
        (winnerIdx, (winnerPC, _, _, _, winnerDiag)) =
          maximumByKey (\(_, (_, _, tot, _, _)) -> tot) indexed
        diags = [ AttemptDiagnostic
                    { adIndex  = i
                    , adScore  = ps
                    , adTotal  = tot
                    , adViable = ok
                    , adPicked = i == winnerIdx
                    , adChords = chordNamesOf (PC.triadLayer pc)
                    }
                | (i, (pc, ps, tot, ok, _)) <- indexed
                ]
    pure (winnerPC, winnerDiag, diags)
  where
    maximumByKey :: Ord b => (a -> b) -> [a] -> a
    maximumByKey f = foldr1 (\x y -> if f x >= f y then x else y)

-- |Extract a chord-name sequence from a triad-layer 'Harmonic.Rules.Types.Progression.Progression' for
-- the scoreboard's diff column. Mirrors what 'Show Progression'
-- produces per cell, but as a plain list rather than a grid string.
chordNamesOf :: Prog.Progression -> [String]
chordNamesOf prog =
  let cads = toList (Prog.unProgression prog)
      enharms = map (H.enharmonicFunc . H.stateSpelling) cads
  in zipWith Prog.showHarmony enharms cads


-- |Non-fatal notice when the starting state escapes the active R context
-- (key\/overtone containment, allowed roots). The cue is always honoured —
-- 'cue' and the random default cue are the human aberration channel by
-- design — this only makes the escape visible. Prints nothing when the
-- state sits inside R or when the relevant filters are wildcards. Mirrors
-- 'matchesContextWithTarget' with no bass target and no strict containment
-- (its default treatment), so it never flags a state the step filter
-- itself would accept.
printCueEscapeNotice :: HarmonicContext -> H.CadenceState -> IO ()
printCueEscapeNotice ctx start = do
  let pctx      = parseContextOnce ctx
      rootPC    = P.unPitchClass (P.pitchClass (H.stateCadenceRoot start))
      intervals = map P.unPitchClass (H.cadenceIntervals (H.stateCadence start))
      absPCs    = [ (i + rootPC) `mod` 12 | i <- intervals ]
      toneOff   = if pcIsKeyWild pctx && pcIsOvertonesWild pctx
                    then []
                    else [ p | p <- nub absPCs
                             , not (IntSet.member p (pcEffectiveOvertones pctx)) ]
      rootOff   = not (pcIsRootsWild pctx)
                  && not (IntSet.member rootPC (pcAllowedBassNotes pctx))
      spelling  = H.inferSpelling absPCs
      enharm    = H.enharmonicFunc spelling
      spellPC p = show (enharm (P.mkPitchClass p))
      parts     = [ "contains " ++ unwords (map spellPC toneOff)
                    ++ " outside the key/overtone set" | not (null toneOff) ]
             ++ [ "root " ++ spellPC rootPC ++ " outside the allowed roots"
                | rootOff ]
  when (not (null parts)) $
    putStrLn $ "⚠ cue escapes R: " ++ intercalate "; " parts
             ++ "  (" ++ show ctx ++ ")"
