{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Framework.Builder
-- Description : Generative engine for harmonic progressions with unified diagnostics interface
--
-- This module implements the main generation loop that connects:
--
--   * R (Rules): HarmonicContext constraints via Filter module
--   * E (Evaluation): Database-derived composer probabilities
--   * T (Traversal): Voice leading optimization
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
-- The database is treated as abstract/pitch-agnostic. Root notes are
-- computed at runtime based on movement intervals from a user-defined
-- starting CadenceState.
--
-- == Filter Notation (from original README)
--
-- === Overtones/Pitch Set Filter
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

    -- * Positional Generation (legacy/internal)
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
  , matchesContext
  , parseComposersWithOrder
  , makePortmanteau
  , extractByPosition
  , takeFromBeginning
  , takeFromEnd
  , takeFromMiddle
  , printHeader
  ) where

import qualified Database.Bolt as Bolt
import qualified Data.Text as T
import           Data.Text (Text)
import           Control.Monad (forM_, when)
import           Data.Char (toLower)
import           Data.List (intercalate)
import           System.Random.MWC (GenIO, createSystemRandom, uniformRM)

import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as P
import qualified Harmonic.Rules.Types.Progression as Prog
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import qualified Harmonic.Rules.Types.Scale as Sc
import           Harmonic.Rules.Import.Graph (connectNeo4j)
import qualified Harmonic.Evaluation.Database.Query as Q
import qualified Harmonic.Evaluation.Scoring.Progression as PS
import           Control.Monad.IO.Class (liftIO)
import           Harmonic.Rules.Constraints.Filter (parseTuningNamed, isWildcard)
import           Harmonic.Rules.Constraints.Overtone (formatOvertoneAnnotation, formatOvertoneAnnotationPipe, possibleTriads)
import           Data.Foldable (toList)
import           Data.List (intercalate, sort, nub)
import           Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq

-- Sub-module imports
import           Harmonic.Framework.Builder.Types
import           Harmonic.Framework.Builder.Portmanteau
import           Harmonic.Framework.Builder.Diagnostics
import           Harmonic.Framework.Builder.Core
import qualified Harmonic.Framework.Builder.Strata as Strata

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
generate start len composerStr entropy context =
  generateWith defaultConfig start len composerStr entropy context

-------------------------------------------------------------------------------
-- String-Friendly Generation (TidalCycles Interface)
-------------------------------------------------------------------------------

-- |Positional generate with header + grid output (internal).
genPrint :: H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genPrint start len composerStr entropy ctx = do
  (prog, _diag) <- generate' start len composerStr entropy ctx
  putStrLn ""
  printHeader (T.pack composerStr) entropy ctx
  print prog
  putStrLn ""
  pure prog

-- |String-friendly generateWith for TidalCycles live coding.
genWith :: GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genWith config start len composerStr entropy ctx = generateWith config start len (T.pack composerStr) entropy ctx

-- |Generate with custom configuration
--
-- Simplified algorithm:
--   1. Start with user-provided CadenceState
--   2. For each step: build candidate pool, gamma-select next cadence
--   3. Candidate pool = graph transitions (filtered) + consonanceFallback
--   4. Pool size is configurable (default 30)
generateWith :: GeneratorConfig
             -> H.CadenceState
             -> Int
             -> Text
             -> Double
             -> HarmonicContext
             -> IO Prog.Progression
generateWith config start len composerStr entropy context = do
  let pctx = parseContextOnce context
  rng <- createSystemRandom
  chain <- if map toLower (T.unpack composerStr) == "none"
    then buildChainOffline config rng entropy context pctx start (len - 1)
    else do
      let composerWeights = Q.parseComposerWeights composerStr
      pipe <- connectNeo4j
      result <- Bolt.run pipe $ buildChain config rng entropy context pctx composerWeights start (len - 1)
      Bolt.close pipe
      pure result
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
generate' start len composerStr entropy ctx =
  genWith' defaultConfig start len composerStr entropy ctx

-- |Positional generate with compact musical summary (internal).
genPrint' :: H.CadenceState -> Int -> String -> Double -> HarmonicContext
          -> IO Prog.Progression
genPrint' start len composerStr entropy ctx = do
  (prog, diag) <- generate' start len composerStr entropy ctx
  renderStandardSteps composerStr ctx diag
  putStrLn ""
  printHeader (T.pack composerStr) entropy ctx
  print prog
  putStrLn ""
  pure prog

-- |Per-step Standard renderer extracted from 'genPrint''. Emits the
-- compact summary + per-step lines, terminated by the trailing ━ rule.
-- Does NOT print the final header + grid; callers do that.
renderStandardSteps :: String -> HarmonicContext -> GenerationDiagnostics -> IO ()
renderStandardSteps composerStr ctx diag = do
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
             ++ " → " ++ show (gdActualLen diag) ++ " chords (entropy " ++ show (gdEntropy diag) ++ ")"
  putStrLn $ composerModeStr composerStr
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  let bar1Suffix = if hasAnnotation && not (null allStates)
                   then let ann = annotateState (head allStates)
                        in if null ann then "" else "  " ++ ann
                   else ""
  putStrLn $ "  1: " ++ gdStartRoot diag ++ " " ++ gdStartCadence diag
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

    putStrLn $ "  " ++ show barNum ++ ": " ++ stateInfo ++ "  " ++ poolInfo
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

    putStrLn ""

  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

-- |Generate with custom configuration, returning diagnostics tuple (internal).
genWith' :: GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext
         -> IO (Prog.Progression, GenerationDiagnostics)
genWith' config start len composerStr entropy context = do
  let pctx = parseContextOnce context
  rng <- createSystemRandom
  (chain, stepDiags) <- if map toLower composerStr == "none"
    then buildChainOfflineWithDiag config rng entropy context pctx start (len - 1)
    else do
      let composerWeights = Q.parseComposerWeights (T.pack composerStr)
      pipe <- connectNeo4j
      result <- Bolt.run pipe $ buildChainWithDiag config rng entropy context pctx composerWeights start (len - 1)
      Bolt.close pipe
      pure result
  let prog = chainToProgression chain
      diag = GenerationDiagnostics
        { gdStartCadence = show (extractCadence start)
        , gdStartRoot = show (H.stateCadenceRoot start)
        , gdRequestedLen = len
        , gdActualLen = Prog.progLength prog
        , gdEntropy = entropy
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
generate'' start len composerStr entropy ctx =
  genWith'' defaultConfig start len composerStr entropy ctx

-- |Positional generate with verbose traces (internal).
genPrint'' :: H.CadenceState -> Int -> String -> Double -> HarmonicContext
           -> IO Prog.Progression
genPrint'' start len composerStr entropy ctx = do
  (prog, diag) <- generate'' start len composerStr entropy ctx
  renderVerboseSteps composerStr diag
  putStrLn ""
  printHeader (T.pack composerStr) entropy ctx
  print prog
  putStrLn ""
  pure prog

-- |Per-step Verbose renderer extracted from 'genPrint''''. Emits the
-- verbose summary + per-step trace, terminated by the trailing ━ rule.
-- Does NOT print the final header + grid; callers do that.
renderVerboseSteps :: String -> GenerationDiagnostics -> IO ()
renderVerboseSteps composerStr diag = do
  putStrLn ""
  putStrLn $ "Verbose Generation: " ++ gdStartRoot diag ++ " " ++ gdStartCadence diag
             ++ " → " ++ show (gdActualLen diag) ++ " chords (entropy " ++ show (gdEntropy diag) ++ ")"
  putStrLn $ composerModeStr composerStr
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  putStrLn $ "STEP 1: " ++ gdStartRoot diag ++ " " ++ gdStartCadence diag ++ " [starting state]"
  putStrLn ""

  forM_ (gdSteps diag) $ \step -> do
    let barNum = sdStepNumber step + 1
        mvmt = sdSelectedDbMovement step
        chord = case sdRenderedChord step of
                  Just c -> c
                  Nothing -> sdPosteriorRoot step
        src = "[" ++ sdSelectedFrom step ++ "]"
        selIdx = "(γ=" ++ show (sdGammaIndex step) ++ "/" ++ show (sdPoolSize step) ++ ")"

    putStrLn $ "STEP " ++ show barNum ++ ": " ++ sdPriorRoot step ++ " → "
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
genWith'' config start len composerStr entropy context = do
  let pctx = parseContextOnce context
  rng <- createSystemRandom
  (chain, stepDiags) <- if map toLower composerStr == "none"
    then buildChainOfflineWithDiagV config rng 2 entropy context pctx start (len - 1)
    else do
      let composerWeights = Q.parseComposerWeights (T.pack composerStr)
      pipe <- connectNeo4j
      result <- Bolt.run pipe $ buildChainWithDiagV config rng 2 entropy context pctx composerWeights start (len - 1)
      Bolt.close pipe
      pure result
  let prog = chainToProgression chain
      diag = GenerationDiagnostics
        { gdStartCadence = show (extractCadence start)
        , gdStartRoot = show (H.stateCadenceRoot start)
        , gdRequestedLen = len
        , gdActualLen = Prog.progLength prog
        , gdEntropy = entropy
        , gdSteps = stepDiags
        , gdProgression = prog
        }
  pure (prog, diag)

-------------------------------------------------------------------------------
-- Unified Interface
-------------------------------------------------------------------------------

-- |Generate a progression with NO diagnostic output (verbosity 0 - silent mode).
genSilent :: H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genSilent start len composerStr entropy ctx = do
  (prog, _diag) <- generate' start len composerStr entropy ctx
  pure prog

-- |Generate a progression with STANDARD diagnostic output (verbosity 1).
genStandard :: H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genStandard start len composerStr entropy ctx = do
  (prog, diag) <- generate' start len composerStr entropy ctx
  printDiagnostics 1 diag
  pure prog

-- |Generate a progression with VERBOSE diagnostic output (verbosity 2).
genVerbose :: H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genVerbose start len composerStr entropy ctx = do
  (prog, diag) <- generate'' start len composerStr entropy ctx
  printDiagnostics 2 diag
  pure prog

-- |Silent mode with custom 'GeneratorConfig'.
genSilent' :: GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genSilent' config start len composerStr entropy ctx = do
  (prog, _diag) <- genWith' config start len composerStr entropy ctx
  pure prog

-- |Standard diagnostics with custom 'GeneratorConfig'.
genStandard' :: GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genStandard' config start len composerStr entropy ctx = do
  (prog, diag) <- genWith' config start len composerStr entropy ctx
  printDiagnostics 1 diag
  pure prog

-- |Verbose diagnostics with custom 'GeneratorConfig'.
genVerbose' :: GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genVerbose' config start len composerStr entropy ctx = do
  (prog, diag) <- genWith'' config start len composerStr entropy ctx
  printDiagnostics 2 diag
  pure prog

-------------------------------------------------------------------------------
-- Modifier-Based Generation API
-------------------------------------------------------------------------------

-- |Execute a 'GenConfig', producing a progression.
--
-- Thin wrapper that calls 'execGenConfigWithDiag' (pure compute) and then
-- emits the appropriate diagnostics + header + grid via 'emitFinalised'.
-- Single-pass callers see byte-identical output to today.
execGenConfig :: GenConfig -> IO Prog.Progression
execGenConfig gc = do
  (prog, diag) <- execGenConfigWithDiag gc
  emitFinalised gc (PC.fromProgression prog, diag)
  pure prog

-- |Compute-only variant of 'execGenConfig'. Returns the progression and
-- its diagnostics without printing anything. Used by 'singlePassExecPCWithDiag'
-- and by 'generateBest' inside the K-attempt loop so per-attempt output
-- can be suppressed and only the winner's emitted.
execGenConfigWithDiag :: GenConfig -> IO (Prog.Progression, GenerationDiagnostics)
execGenConfigWithDiag gc = do
  start <- _gcCue gc
  case _gcMode gc of
    Fresh -> case _gcVerbosity gc of
      Silent   -> generate'  start (_gcLen gc) (_gcSeek gc) (_gcEntropy gc) (_gcTonal gc)
      Standard -> generate'  start (_gcLen gc) (_gcSeek gc) (_gcEntropy gc) (_gcTonal gc)
      Verbose  -> generate'' start (_gcLen gc) (_gcSeek gc) (_gcEntropy gc) (_gcTonal gc)

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
      -- Generate _gcLen+1 chords (cue + new), then drop cue, splice into source.
      (fullProg, regenDiag) <- generate' start (_gcLen gc + 1)
                                 (_gcSeek gc) (_gcEntropy gc) (_gcTonal gc)
      let newChords = tail $ toList $ Prog.unProgression fullProg
          result    = Prog.spliceProgression srcProg s e newChords
      pure (result, regenDiag)

    -- Strata modes are handled by the PC-returning path; they should not
    -- reach this function. Defensive fallback retains the old Fresh
    -- behaviour rather than crashing.
    StrataMode _    -> generate' start (_gcLen gc) (_gcSeek gc) (_gcEntropy gc) (_gcTonal gc)
    FromProgPC {}   -> generate' start (_gcLen gc) (_gcSeek gc) (_gcEntropy gc) (_gcTonal gc)

-- |Default generation configuration.
--
-- @
-- cue:     random root, major triad
-- len:     4
-- seek:    "*" (all composers)
-- entropy: 0.2
-- tonal:   hContext (chromatic)
-- @
defaultGenConfig :: GenConfig
defaultGenConfig = GenConfig
  { _gcCue         = defaultCue
  , _gcLen         = 4
  , _gcSeek        = "*"
  , _gcEntropy     = 0.2
  , _gcTonal       = hContext
  , _gcVerbosity   = Silent
  , _gcMode        = Fresh
  , _gcLenOverride = Nothing
  , _gcRelStrata   = Nothing
  , _gcAbsStrata   = Nothing
  -- Plan defaults: same-strata 0.90, flip-flop 0.80, same-tristrata 0.70.
  -- Values < 1.0 multiply 'badness' down (favouring the candidate); 1.0 is
  -- the no-op. The product caps at 0.70 * 0.80 * 0.90 ≈ 0.50, giving
  -- ≤2× favouring — overpowerable by strong graph-side confidence.
  , _gcBoostSame   = 0.90
  , _gcBoostFlip   = 0.80
  , _gcBoostTri    = 0.70
  , _gcMaxAttempts  = 1
  , _gcViableTarget = 1
  -- Calibrated from a 30-sample online probe (gen, 8 bars, entropy 0.4,
  -- seek "*"): totalScore distribution observed at min 0.50, median 0.67,
  -- max 0.78 with the online default weights. T=0.6 catches the bottom
  -- ~20% of attempts (fallback-driven or tritone-leap runs), keeping
  -- 'attempt 3 12' reliable. Tune with the 'viability' modifier.
  , _gcViabilityFloor = 0.6
  }
  where
    defaultCue = do
      rng <- createSystemRandom
      rootIdx <- uniformRM (0 :: Int, 11) rng
      let rootName = H.enharmonicFunc H.FlatSpelling (P.mkPitchClass rootIdx)
      pure $ H.initCadenceState 0 (show rootName) [0, 4, 7]

-- |Generation config with header + grid output (default).
--
-- @
-- s <- seek "*" $ gen
-- s <- seek "*" $ cue start $ tonal ctx $ len 4 $ entropy 0.3 $ gen
-- @
gen :: GenConfig
gen = defaultGenConfig

-- |Generation config with compact musical summary.
gen' :: GenConfig
gen' = defaultGenConfig { _gcVerbosity = Standard }

-- |Generation config with verbose diagnostic traces.
gen'' :: GenConfig
gen'' = defaultGenConfig { _gcVerbosity = Verbose }

-- |Static grid: repeats the cue chord for 'len' bars. No database access.
--
-- @s <- seek "*" $ cue start $ len 4 $ genGrid@
genGrid :: GenConfig
genGrid = defaultGenConfig { _gcMode = GridMode }

-- |Regenerate a range of bars within an existing progression.
-- The cue is inferred from the bar before the start position (wrapping).
--
-- Dispatches on @pcProvenance@:
--
-- * @Just _@ — strata-aware path: regenerates all three layers + provenance
--   in lockstep, with one-step lookahead at the @e → e+1@ seam to keep the
--   spliced bar sequence walk-graph valid under 'allowedNext'.
-- * @Nothing@ — legacy triad-only path: regenerates the triad layer via the
--   standard R→E→T pipeline, then splices.
--
-- @s' <- seek "*" $ entropy 0.3 $ genFrom s 2 3@
-- @s' <- seek "*" $ cue start $ genFrom s 2 3    -- override inferred cue@
-- @s' <- seek "*" $ len 6 $ genFrom s 2 3        -- expand range@
-- @s' <- seek "*" $ genFrom'  s 2 3              -- Standard per-step trace@
-- @s' <- seek "*" $ genFrom'' s 2 3              -- Verbose trace (+ scoreboard with 'attempt')@
genFrom :: PC.ProgressionContext -> Int -> Int -> GenConfig
genFrom pc s e = defaultGenConfig
  { _gcCue  = inferCue
  , _gcLen  = rSize
  , _gcMode = case PC.pcProvenance pc of
      Just _  -> FromProgPC pc s e
      Nothing -> FromProg (PC.triadLayer pc) s e
  }
  where
    triad = PC.triadLayer pc
    n = Prog.progLength triad
    rSize = if s <= e then e - s + 1 else n - s + 1 + e
    cuePos = ((s - 2) `mod` n) + 1  -- 1-indexed, wraps to N when s=1
    inferCue = case Prog.getCadenceState triad cuePos of
      Just cs -> pure cs
      Nothing -> _gcCue defaultGenConfig

-- |Standard-verbosity alias of 'genFrom'. Mirrors 'gen''/'genP''/'genI''.
genFrom' :: PC.ProgressionContext -> Int -> Int -> GenConfig
genFrom' pc s e = (genFrom pc s e) { _gcVerbosity = Standard }

-- |Verbose-verbosity alias of 'genFrom'. Mirrors 'gen'''/'genP'''/'genI'''.
genFrom'' :: PC.ProgressionContext -> Int -> Int -> GenConfig
genFrom'' pc s e = (genFrom pc s e) { _gcVerbosity = Verbose }

-------------------------------------------------------------------------------
-- Generation Modifiers
-------------------------------------------------------------------------------

-- |Set starting state.
--
-- @s <- seek "*" $ cue start $ gen@
cue :: H.CadenceState -> GenConfig -> GenConfig
cue start gc = gc { _gcCue = pure start }

-- |Set progression length (number of chords).
--
-- @s <- seek "*" $ len 8 $ gen@
len :: Int -> GenConfig -> GenConfig
len n gc = gc { _gcLen = n, _gcLenOverride = Nothing }

-- |Set composer blend and execute. Terminal modifier — produces 'IO'
-- 'PC.ProgressionContext'. For legacy 'gen'-family configs, all three layers
-- duplicate the generated triad progression and 'pcProvenance' is 'Nothing';
-- the 'genP' paradigm produces distinct strata/mode layers with 'Just'
-- provenance.
--
-- @s <- seek "*" $ gen@
-- @s <- seek "bach:70 debussy:30" $ cue start $ len 4 $ gen@
-- @s <- seek "none" $ cue start $ len 6 $ genVI@
seek :: String -> GenConfig -> IO PC.ProgressionContext
seek s gc = execGenConfigPC gc { _gcSeek = s }

-- |Terminal executor producing a 'PC.ProgressionContext'.
--
-- When @_gcMaxAttempts > 1@, dispatches through 'generateBest' for
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
-- For strata modes ('StrataMode', 'FromProgPC') Standard/Verbose use the
-- 'printStrataDiagnostics' renderer (legacy 'printDiagnostics' would mis-
-- render the strata trace). For legacy modes ('Fresh', 'GridMode',
-- 'FromProg') Standard uses 'renderStandardSteps' and Verbose uses
-- 'renderVerboseSteps' — matching the byte-for-byte output of the old
-- 'genPrint''/'genPrint''''' wrappers.
--
-- The header + grid always reflect the full 'PC.triadLayer pc'. For
-- 'FromProgPC' / 'FromProg' that's the spliced result (full source
-- progression with regen bars inserted), not the regen segment alone.
emitFinalised :: GenConfig -> (PC.ProgressionContext, GenerationDiagnostics) -> IO ()
emitFinalised gc (pc, diag) = do
  let isStrata = case _gcMode gc of
        StrataMode _    -> True
        FromProgPC {}   -> True
        _               -> False
  case _gcVerbosity gc of
    Silent   -> pure ()
    Standard -> if isStrata
                  then printStrataDiagnostics 1 diag
                  else renderStandardSteps (_gcSeek gc) (_gcTonal gc) diag
    Verbose  -> if isStrata
                  then printStrataDiagnostics 2 diag
                  else renderVerboseSteps (_gcSeek gc) diag
  putStrLn ""
  printHeader (T.pack (_gcSeek gc)) (_gcEntropy gc) (_gcTonal gc)
  print (PC.triadLayer pc)
  putStrLn ""

-- |Single-pass body of 'execGenConfigPC' — used directly when no multi-
-- attempt selection is requested. Thin wrapper that performs pure
-- generation via 'singlePassExecPCWithDiag' then emits the appropriate
-- diagnostics + header + grid via 'emitFinalised'.
singlePassExecPC :: GenConfig -> IO PC.ProgressionContext
singlePassExecPC gc = do
  (pc, diag) <- singlePassExecPCWithDiag gc
  emitFinalised gc (pc, diag)
  pure pc

-- |Pure-compute variant of 'singlePassExecPC'. Returns the
-- 'ProgressionContext' and its 'GenerationDiagnostics' without printing
-- anything. Used by the K-attempt loop inside 'generateBest' so per-
-- attempt output is suppressed and only the winner's emitted.
singlePassExecPCWithDiag :: GenConfig -> IO (PC.ProgressionContext, GenerationDiagnostics)
singlePassExecPCWithDiag gc = case _gcMode gc of
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
-- 'Bolt.Pipe' is opened for the entire K-attempt loop and 'psCadenceFav'
-- is populated via 'PS.scoreProgressionOnline' under the user's composer
-- blend. The online-weighted total ('PS.defaultWeights') is then used —
-- cadence-favourability is the dominant axis (0.4).
--
-- When @_gcSeek == "none"@, scoring is fully pure and uses
-- 'PS.defaultWeightsOffline' (cadence-fav weight zeroed, the other three
-- renormalised).
generateBest :: GenConfig -> IO PC.ProgressionContext
generateBest gc = do
  -- Immediate user feedback before the K-attempt loop blocks. Tidal's
  -- GHCi stdout is line-buffered, so the newline flushes right away.
  putStrLn "composing .."
  let online = map toLower (_gcSeek gc) /= "none"
  (winnerPC, winnerDiag, diags) <-
    if online then runOnline gc else runOffline gc
  -- All per-attempt printing was suppressed inside the loop (Phase 11
  -- moved every emission into 'emitFinalised'). Emit the winner exactly
  -- once, at the caller's verbosity.
  emitFinalised gc (winnerPC, winnerDiag)
  -- Verbose + multi-attempt: surface the full scoreboard.
  when (_gcVerbosity gc == Verbose && _gcMaxAttempts gc > 1) $
    printAttemptScoreboard (_gcViabilityFloor gc) diags
  pure winnerPC

-- |Offline arm of 'generateBest'. Pure scoring with
-- 'PS.defaultWeightsOffline'.
--
-- The loop receives the caller's 'GenConfig' as-is — per-attempt
-- diagnostics are collected at the caller's verbosity, which the
-- winner's 'emitFinalised' then renders. No printing happens inside the
-- loop (Phase 11 lifted every emission out of 'singlePassExecPCWithDiag').
runOffline :: GenConfig
           -> IO (PC.ProgressionContext, GenerationDiagnostics, [AttemptDiagnostic])
runOffline gc = do
  let maxN   = max 1 (_gcMaxAttempts gc)
      target = max 1 (_gcViableTarget gc)
      floorT = _gcViabilityFloor gc
  scored <- offlineLoop gc maxN target floorT
  finaliseScored gc scored

-- |Online arm of 'generateBest'. Opens one 'Bolt.Pipe' for the entire
-- K-attempt loop; scores each attempt via 'PS.scoreProgressionOnline'
-- using @_gcSeek@ as the composer blend; ranks via 'PS.defaultWeights'.
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
  scored <- Bolt.run pipe (onlineLoop seekTxt gc maxN target floorT)
  Bolt.close pipe
  finaliseScored gc scored

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
              isOk  = PS.psModeValidity ps >= 1.0 && tot >= floorT
              acc'  = (pc, ps, tot, isOk, diag) : acc
              viable' = if isOk then viableSoFar + 1 else viableSoFar
          go viable' acc' (remaining - 1)

-- |Inner loop for the online arm, run under 'Bolt.run pipe'.
onlineLoop
  :: T.Text               -- ^ seek string (composer blend)
  -> GenConfig            -- ^ caller's config (printing already lifted out)
  -> Int                  -- ^ maxAttempts
  -> Int                  -- ^ viableTarget
  -> Double               -- ^ viabilityFloor
  -> Bolt.BoltActionT IO [ScoredAttempt]
onlineLoop seekTxt gc maxN target floorT = go 0 [] maxN
  where
    go _ acc 0 = pure (reverse acc)
    go viableSoFar acc remaining
      | viableSoFar >= target = pure (reverse acc)
      | otherwise = do
          (pc, diag) <- liftIO (singlePassExecPCWithDiag gc)
          ps <- PS.scoreProgressionOnline seekTxt pc
          let tot   = PS.totalScore PS.defaultWeights ps
              isOk  = PS.psModeValidity ps >= 1.0 && tot >= floorT
              acc'  = (pc, ps, tot, isOk, diag) : acc
              viable' = if isOk then viableSoFar + 1 else viableSoFar
          go viable' acc' (remaining - 1)

-- |Shared post-loop: builds 'AttemptDiagnostic' values with index +
-- picked flag set on the maximum-totalScore attempt, and returns the
-- picked 'ProgressionContext', its 'GenerationDiagnostics' (for the
-- caller to emit via 'emitFinalised'), and the per-attempt diagnostic
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

-- |Extract a chord-name sequence from a triad-layer 'Progression' for
-- the scoreboard's diff column. Mirrors what 'Show Progression'
-- produces per cell, but as a plain list rather than a grid string.
chordNamesOf :: Prog.Progression -> [String]
chordNamesOf prog =
  let cads = toList (Prog.unProgression prog)
      enharms = map (H.enharmonicFunc . H.stateSpelling) cads
      chords = map H.fromCadenceState cads
  in zipWith Prog.showTriad enharms chords

-- |Set entropy (gamma shape parameter). Higher values = more unusual choices.
--
-- @s <- seek "*" $ entropy 0.5 $ gen@
entropy :: Double -> GenConfig -> GenConfig
entropy e gc = gc { _gcEntropy = e }

-- |Run multi-attempt rank-and-select generation: produce up to @maxAttempts@
-- candidate progressions, stop early once @viableTarget@ viable attempts
-- (all bars 'ModeOk') have been collected, then return the highest-scoring
-- one. Scoring blends root motion, voice leading, and mode validity via
-- 'PS.defaultWeightsOffline'.
--
-- @s <- seek "*" $ attempt 3 24 $ entropy 0.4 $ gen@   -- best of up to 24
--
-- Defaults are @attempt 1 1@ — i.e. the modifier is a no-op when omitted,
-- preserving legacy single-pass behaviour.
attempt :: Int -> Int -> GenConfig -> GenConfig
attempt viableTarget maxAttempts gc = gc
  { _gcViableTarget = max 1 viableTarget
  , _gcMaxAttempts  = max 1 maxAttempts
  }

-- |Set the viability quality floor used by 'attempt'. An attempt is
-- /viable/ iff @psModeValidity >= 1.0@ (structural invariant) and
-- @totalScore >= floor@. Default is @0.5@; passing @0.0@ recovers the
-- original structural-only viability.
--
-- @s <- seek "*" $ viability 0.65 $ attempt 3 24 $ gen@
--
-- Tune downward if @attempt N K@ frequently fails to collect N viable
-- within K (raise K or lower the floor); tune upward if K is being hit
-- consistently with mediocre-quality picks (lower K or raise the floor).
viability :: Double -> GenConfig -> GenConfig
viability t gc = gc { _gcViabilityFloor = max 0 t }

-- |Set harmonic context (R constraints).
--
-- @s <- seek "*" $ tonal (hcKey "0#" $ hContext) $ gen@
tonal :: HarmonicContext -> GenConfig -> GenConfig
tonal ctx gc = gc { _gcTonal = ctx }

-- |Per-bar position within the dynamically-changing active tristrata. Elements
-- @∈ {1,2,3}@ cycle circularly. Sets '_gcLenOverride' to the parsed list length
-- so '_gcLen' doesn't need to be set explicitly; 'len' applied later clears the
-- override (last-writer-wins).
--
-- @s <- seek "none" $ relStrata "1 1 2 2 3 3" $ genVI@  -- 6 bars
relStrata :: String -> GenConfig -> GenConfig
relStrata s gc =
  let ns = Sc.parseRelStrata s
  in gc { _gcRelStrata = Just ns
        , _gcLenOverride = if null ns then Nothing else Just (length ns)
        }

-- |Per-bar absolute strata label across all tristratas. Elements are Roman
-- numerals @I..XI@ cycling circularly. Sets '_gcLenOverride' to the parsed
-- list length.
--
-- @s <- seek "none" $ absStrata "I V X" $ genI@  -- 3 bars
absStrata :: String -> GenConfig -> GenConfig
absStrata s gc =
  let ss = Sc.parseAbsStrata s
  in gc { _gcAbsStrata = Just ss
        , _gcLenOverride = if null ss then Nothing else Just (length ss)
        }

-- |Override the same-strata continuity boost multiplier. Values below 1.0
-- favour candidates whose strata matches the previous bar's. Default 0.90.
-- Pass 1.0 to disable the bias.
--
-- @s <- seek "none" $ sameBoost 0.5 $ genVI@  -- strong same-strata pull
sameBoost :: Double -> GenConfig -> GenConfig
sameBoost x gc = gc { _gcBoostSame = x }

-- |Override the flip-flop boost multiplier (candidates matching the
-- grandparent strata when the current /= previous). Default 0.80.
flipBoost :: Double -> GenConfig -> GenConfig
flipBoost x gc = gc { _gcBoostFlip = x }

-- |Override the same-tristrata continuity boost multiplier. Values below
-- 1.0 favour candidates whose active tristrata matches the previous bar's.
-- Default 0.70 (strongest of the three).
triBoost :: Double -> GenConfig -> GenConfig
triBoost x gc = gc { _gcBoostTri = x }

-------------------------------------------------------------------------------
-- genP Paradigm (strata-first traversal)
-------------------------------------------------------------------------------

-- |Strata-first generation entrypoint. Seeded by a 'Sc.StrataLabel'; produces
-- a 'PC.ProgressionContext' with distinct triad, strata, and mode layers and
-- @pcProvenance = Just …@.
--
-- @s <- seek "none" $ cue start $ len 6 $ genP VI@
genP :: Sc.StrataLabel -> GenConfig
genP s = defaultGenConfig { _gcMode = StrataMode s }

-- |Standard-verbosity variant of 'genP'.
genP' :: Sc.StrataLabel -> GenConfig
genP' s = (genP s) { _gcVerbosity = Standard }

-- |Verbose-verbosity variant of 'genP'.
genP'' :: Sc.StrataLabel -> GenConfig
genP'' s = (genP s) { _gcVerbosity = Verbose }

-- 33 ergonomic aliases: one per Roman numeral × three verbosities.
genI, genII, genIII, genIV, genV, genVI, genVII, genVIII, genIX, genX, genXI :: GenConfig
genI     = genP Sc.I
genII    = genP Sc.II
genIII   = genP Sc.III
genIV    = genP Sc.IV
genV     = genP Sc.V
genVI    = genP Sc.VI
genVII   = genP Sc.VII
genVIII  = genP Sc.VIII
genIX    = genP Sc.IX
genX     = genP Sc.X
genXI    = genP Sc.XI

genI', genII', genIII', genIV', genV', genVI', genVII', genVIII', genIX', genX', genXI' :: GenConfig
genI'    = genP' Sc.I
genII'   = genP' Sc.II
genIII'  = genP' Sc.III
genIV'   = genP' Sc.IV
genV'    = genP' Sc.V
genVI'   = genP' Sc.VI
genVII'  = genP' Sc.VII
genVIII' = genP' Sc.VIII
genIX'   = genP' Sc.IX
genX'    = genP' Sc.X
genXI'   = genP' Sc.XI

genI'', genII'', genIII'', genIV'', genV'', genVI'', genVII'', genVIII'', genIX'', genX'', genXI'' :: GenConfig
genI''    = genP'' Sc.I
genII''   = genP'' Sc.II
genIII''  = genP'' Sc.III
genIV''   = genP'' Sc.IV
genV''    = genP'' Sc.V
genVI''   = genP'' Sc.VI
genVII''  = genP'' Sc.VII
genVIII'' = genP'' Sc.VIII
genIX''   = genP'' Sc.IX
genX''    = genP'' Sc.X
genXI''   = genP'' Sc.XI

-------------------------------------------------------------------------------
-- Strata traversal runner
-------------------------------------------------------------------------------

-- |Walk the (strata, tristrata) sequence for a 'StrataMode' run and build a
-- 'PC.ProgressionContext' with distinct triad/strata/mode layers.
--
-- The triad layer is generated by the full R→E→T pipeline: at each bar, the
-- active context's '_hcOvertones' is narrowed to the current strata's 5-PC
-- chroma (prime-notation), 'parseContextOnce' is rerun, and 'stepChainBody'
-- is invoked with a 'pcSoftBoost' multiplier computed from strata /
-- tristrata continuity against prior bars. Graph candidates (Neo4j) and
-- consonance-fallback candidates both flow through the usual filter →
-- score → gamma-select pipeline, with the soft-boost applied to 'badness'
-- at 'computeFallbackScoreWithBoost' (fallback) and inversely to
-- confidence in 'scoreByConfidence'-output (graph). Single-strata
-- containment is guaranteed by construction: the narrowed overtone set
-- precludes any triad whose chroma escapes 's_i'.
--
-- Strata and mode layer bars are representative 3-PC slices of
-- 'strataChroma s_i' and 'modeChroma m_i' respectively, rooted on the
-- generated triad's root so they transpose with the progression.
runStrataGen :: Sc.StrataLabel -> GenConfig -> IO (PC.ProgressionContext, GenerationDiagnostics)
runStrataGen sStart gc = do
  start <- _gcCue gc
  rng   <- createSystemRandom
  let basePctx = parseContextOnce (_gcTonal gc)
      allowed  = pcAllowedTristrata basePctx
      allowed' = if null allowed then Sc.validTristrata else allowed
      n        = max 1 (fromMaybe (_gcLen gc) (_gcLenOverride gc))
      (s0, t0) = Strata.initialPlacement allowed' sStart

      -- relStrata / absStrata narrowing on the walk's candidate pool.
      narrow :: Int -> [(Sc.StrataLabel, Sc.Tristrata)] -> [(Sc.StrataLabel, Sc.Tristrata)]
      narrow i pool =
        let pool1 = case _gcRelStrata gc of
              Just ps | not (null ps) ->
                let p = ps !! (i `mod` length ps)
                in [(s',t') | (s',t') <- pool, Sc.tristrataStrataAt t' p == s']
              _ -> pool
            pool2 = case _gcAbsStrata gc of
              Just ss | not (null ss) ->
                let s = ss !! (i `mod` length ss)
                in [(s',t') | (s',t') <- pool1, s' == s]
              _ -> pool1
        in pool2

  -- Sample one random seed per transition bar. Used by 'selectNextSeeded'
  -- to escape the pathological same-strata fixed point of the purely
  -- categorical 'selectNext'. 'n - 1' seeds cover bars 1..n-1; bar 0 is
  -- set by 'initialPlacement'.
  walkSeeds <- mapM (const (uniformRM (minBound :: Int, maxBound :: Int) rng))
                    [1 .. max 0 (n - 1)]

  let -- Pre-compute the (s_i, t_i) sequence for all n bars using the
      -- seeded stochastic selector (allows genuine traversal rather than
      -- sticking on the starting strata).
      walk :: Int -> [Int] -> (Sc.StrataLabel, Sc.Tristrata) -> [(Sc.StrataLabel, Sc.Tristrata)]
      walk i seeds prev
        | i >= n = []
        | otherwise =
            let cands  = narrow i (Strata.allowedNext allowed' prev)
                (seed, seedsRest) = case seeds of
                  (s : rest) -> (s, rest)
                  []         -> (0, [])
                chosen = fromMaybe prev (Strata.selectNextSeeded seed prev cands)
            in chosen : walk (i + 1) seedsRest chosen

      barSeq :: [(Sc.StrataLabel, Sc.Tristrata)]
      barSeq = (s0, t0) : walk 1 walkSeeds (s0, t0)

      -- Build an overtones string of prime-notation tokens ("C#' D' E'") for
      -- a strata's 5-PC chroma. 'parseOvertones'' treats "X'" as a pinned
      -- single PC (no overtone series expansion), so pcEffectiveOvertones
      -- after parseContextOnce is exactly the 5 PCs of the strata.
      strataOvertonesString :: Sc.StrataLabel -> String
      strataOvertonesString s =
        unwords [ show (P.sharp (P.mkPitchClass (P.unPitchClass pc))) ++ "'"
                | pc <- Sc.strataChroma s ]

      -- Per-bar soft-boost: compare bar i's (s,t) with bar i-1 and i-2.
      -- sameMult  ← candidate matches s_{i-1}      (strata continuity)
      -- flipMult  ← matches s_{i-2} AND not s_{i-1} (flip-flop return)
      -- triMult   ← t_i == t_{i-1}                 (tristrata continuity)
      -- Because the candidate pool is strata-narrowed, every candidate in
      -- bar i has strata s_i; so the (s', t') vs (s_{prev}, t_{prev}) test
      -- collapses to an s_i-vs-s_{prev} scalar. Boost is a per-bar scalar,
      -- applied uniformly across the bar's pool.
      boostFor :: Int -> Double
      boostFor 0 = 1.0  -- bar 0: no prior
      boostFor i =
        let (sCurr, tCurr) = barSeq !! i
            (sPrev, tPrev) = barSeq !! (i - 1)
            sGrand         = if i >= 2 then Just (fst (barSeq !! (i - 2))) else Nothing
            mSame = if sCurr == sPrev then _gcBoostSame gc else 1.0
            mFlip = case sGrand of
                      Just sg | sCurr == sg && sCurr /= sPrev -> _gcBoostFlip gc
                      _                                        -> 1.0
            mTri  = if tCurr == tPrev then _gcBoostTri gc else 1.0
            _     = tPrev  -- keep reference (silence unused warning if any)
        in mSame * mFlip * mTri

      -- Build the per-bar ParsedContext by narrowing _hcOvertones to the
      -- bar's strata chroma and attaching the bar's soft-boost.
      -- The supplier is 1-indexed (bar 1 = first generated bar, which is
      -- barSeq index 1 since barSeq[0] is the starting state).
      pctxAt :: Int -> ParsedContext
      pctxAt barIdx1 =
        let i      = barIdx1  -- barSeq index for the generated bar
            (s, _) = barSeq !! i
            ctx'   = hcOvertones (strataOvertonesString s) (_gcTonal gc)
            pctx   = parseContextOnce ctx'
            boost  = boostFor i
        in pctx { pcSoftBoost = boost
                , pcStrictContainment = True
                }

      -- Cue validation: the starting CadenceState's absolute PCs must all
      -- be members of the starting strata's chroma. If the cue escapes
      -- the strata, abort with a helpful message listing viable triads.
      startAbsPCs =
        let rpc = P.unPitchClass (P.pitchClass (H.stateCadenceRoot start))
            ivs = map P.unPitchClass (H.cadenceIntervals (H.stateCadence start))
        in [ (iv + rpc) `mod` 12 | iv <- ivs ]
      s0PCs      = map P.unPitchClass (Sc.strataChroma s0)
      cueValid   = all (`elem` s0PCs) startAbsPCs

  if not cueValid
    then do
      printInvalidCueError start s0
      let emptyPC = PC.ProgressionContext
            { PC.triadLayer   = mempty
            , PC.strataLayer  = mempty
            , PC.modeLayer    = mempty
            , PC.pcProvenance = Just Seq.empty
            }
          emptyDiag = GenerationDiagnostics
            { gdStartCadence = show (H.stateCadence start)
            , gdStartRoot    = show (H.stateCadenceRoot start)
            , gdRequestedLen = n
            , gdActualLen    = 0
            , gdEntropy      = _gcEntropy gc
            , gdSteps        = []
            , gdProgression  = mempty
            }
      pure (emptyPC, emptyDiag)
    else runStrataGenBody sStart gc start rng s0 t0 barSeq pctxAt boostFor n

-- |Body of 'runStrataGen' after cue validation has passed. Separated so
-- the cue-invalid path can abort cleanly without wiring through the
-- full chain-building machinery.
runStrataGenBody
  :: Sc.StrataLabel
  -> GenConfig
  -> H.CadenceState
  -> GenIO
  -> Sc.StrataLabel
  -> Sc.Tristrata
  -> [(Sc.StrataLabel, Sc.Tristrata)]
  -> (Int -> ParsedContext)
  -> (Int -> Double)
  -> Int
  -> IO (PC.ProgressionContext, GenerationDiagnostics)
runStrataGenBody _sStart gc start rng _s0 _t0 barSeq pctxAt boostFor n = do
  -- Keep the cue as bar 1 (matching 'gen' semantics). Generate n-1 more
  -- bars. Chain has n elements: [cue, gen_1, ..., gen_{n-1}]. Step i of
  -- generation produces chain[i] using barSeq[i]'s strata context.
  let pctxAtStep :: Int -> ParsedContext
      pctxAtStep = pctxAt
      -- Verbosity: Silent → no diagnostics, Standard → Just 1, Verbose → Just 2.
      verbArg = case _gcVerbosity gc of
        Silent   -> Nothing
        Standard -> Just 1
        Verbose  -> Just 2
  (chain, rawDiags) <-
    if map toLower (_gcSeek gc) == "none"
      then buildStrataChainOffline defaultConfig rng verbArg
             (_gcEntropy gc) (_gcTonal gc) pctxAtStep start (max 0 (n - 1))
      else do
        let composerWeights = Q.parseComposerWeights (T.pack (_gcSeek gc))
        pipe <- connectNeo4j
        result <- Bolt.run pipe $ buildStrataChain defaultConfig rng verbArg
                   (_gcEntropy gc) (_gcTonal gc) pctxAtStep composerWeights start (max 0 (n - 1))
        Bolt.close pipe
        pure result

  -- Assemble ProgressionContext. The triadLayer is the R→E→T chain.
  -- Strata & mode layers carry the full chroma (5 PCs / 7 PCs respectively)
  -- expressed as intervals from the bar's harmonic root, so they transpose
  -- with the progression and can be voiced as 5- or 7-pitch sets downstream.
  let -- Triad's harmonic root PC (post-detectInversion). For inversions
      -- 'H.stateCadenceRoot' is the bass; we want the harmonic root.
      -- 'H.fromCadenceState' runs detectInversion to set 'chordNoteName'
      -- to the harmonic root.
      harmonicRootOf :: H.CadenceState -> Int
      harmonicRootOf cs =
        P.unPitchClass (P.pitchClass (H.chordNoteName (H.fromCadenceState cs)))

      -- Triad's harmonic root note (used for spelling-aware rootName).
      harmonicRootNote :: H.CadenceState -> P.NoteName
      harmonicRootNote cs = H.chordNoteName (H.fromCadenceState cs)

      -- Express a chroma set as intervals from a chosen root PC. Preserves
      -- full cardinality (no truncation). Always starts at 0 because the
      -- root is in its own chroma by construction (every strata/mode contains
      -- its own root).
      chromaIntervals :: Int -> [P.PitchClass] -> [Int]
      chromaIntervals rootPC chroma =
        sort $ nub [ (P.unPitchClass p - rootPC) `mod` 12 | p <- chroma ]

      -- Per-bar mode classification, triad-anchored and history-aware.
      modeResults :: [Sc.ModeResult]
      modeResults =
        [ Strata.modeForTriad barSeq i (harmonicRootOf cs)
        | (i, cs) <- zip [0..] chain
        ]

      -- Per-bar chroma stored in the mode layer. 'ModeOk' contributes the
      -- 7-PC mode chroma; 'ModeInvalid' contributes its 6-PC overlap PCs
      -- as-is. No Aeolian masquerade — the layer faithfully reflects what
      -- 'modeForTriad' produced. Natural walks always yield 'ModeOk' (proven
      -- from 'allowedNext' adjacency); 'ModeInvalid' is reachable only via
      -- explicit 'absStrata' / 'relStrata' overrides that violate tristrata
      -- adjacency.
      modeChromaList :: [[P.PitchClass]]
      modeChromaList =
        [ case mr of
            Sc.ModeOk m         -> Sc.modeChroma m
            Sc.ModeInvalid pcs  -> pcs
        | mr <- modeResults
        ]

      -- Build a CadenceState from a root and root-relative intervals,
      -- preserving full cardinality. Bypasses 'H.initCadenceState' because
      -- that function routes through 'flatTriad' / 'toCadence' which
      -- truncate >3 PCs (the legacy assumption that a Cadence is exactly
      -- a triad). For strata / mode layers we want to keep the full 5 / 7
      -- PC chroma intact for downstream voicing.
      mkChromaCS :: P.NoteName -> [Int] -> H.CadenceState
      mkChromaCS root intervals =
        let rootPC          = P.unPitchClass (P.pitchClass root)
            pcs             = map P.mkPitchClass intervals
            absolutePitches = map (\i -> (i + rootPC) `mod` 12) intervals
            spelling        = H.inferSpelling absolutePitches
            cadence         = H.Cadence "" H.Unison pcs
        in H.CadenceState cadence root spelling

      -- Build per-bar strata-layer + mode-layer CadenceStates rooted on
      -- each generated triad's harmonic root, carrying the full 5 / 7 PC
      -- chroma respectively (6 PCs for override-driven 'ModeInvalid' bars).
      mkAuxLayers :: H.CadenceState
                  -> Sc.StrataLabel
                  -> [P.PitchClass]              -- mode/overlap chroma
                  -> (H.CadenceState, H.CadenceState)
      mkAuxLayers triadCS sCurr modeChromaPCs =
        let rootPC     = harmonicRootOf triadCS
            rootNote   = harmonicRootNote triadCS
            strataInts = chromaIntervals rootPC (Sc.strataChroma sCurr)
            modeInts   = chromaIntervals rootPC modeChromaPCs
            strataCS   = mkChromaCS rootNote strataInts
            modeCS     = mkChromaCS rootNote modeInts
        in (strataCS, modeCS)

      stratas = [s | (s, _) <- pairs]
      modes   = [m | (_, m) <- pairs]
      pairs   = zipWith3 mkAuxLayers chain (map fst barSeq) modeChromaList

      provSeq          = Seq.fromList [(t, s) | (s, t) <- take (length chain) barSeq]
      resultPC = PC.ProgressionContext
        { PC.triadLayer   = Prog.fromCadenceStates chain
        , PC.strataLayer  = Prog.fromCadenceStates stratas
        , PC.modeLayer    = Prog.fromCadenceStates modes
        , PC.pcProvenance = Just provSeq
        }

  -- Synthesize a "starter" StepDiagnostic for the cue bar (chain[0]) so
  -- the diagnostic loop has one entry per output bar. The cue has no
  -- prior state and wasn't selected from a candidate pool — 'sdSelectedFrom'
  -- = "starter" signals this to the renderer.
  let starterDiag = mkStarterDiag start
      allBaseDiags = starterDiag : rawDiags

  -- Attach strata/tristrata/mode/boost info. allBaseDiags has n entries
  -- aligned with chain / barSeq / modeList / modeResults.
  let tristrataIdxOf t =
        let tpairs = zip Sc.validTristrata [1 :: Int ..]
        in lookup t tpairs
      -- Per-bar enharmonic spelling, derived from the mode's chroma via
      -- 'H.inferSpelling' with the triad's harmonic root as the leading
      -- pitch. Reuses the same tooling as the progression-grid chord
      -- naming (which also threads through 'inferSpelling' inside
      -- 'advanceStateTraced'), but scoped to the bar's full modal context
      -- rather than a single 3-note chord. This gives the diagnostic
      -- block one coherent accidental system.
      barSpellingOf :: [P.PitchClass] -> Int -> H.EnharmonicSpelling
      barSpellingOf chroma rootPC =
        let pcs = rootPC : [p | p <- map P.unPitchClass chroma, p /= rootPC]
        in H.inferSpelling pcs

      -- Slash-notation chord rendering using the bar's spelling.
      slashChordWith :: H.EnharmonicSpelling -> H.CadenceState -> String
      slashChordWith spelling cs =
        let chord = H.fromCadenceState cs
        in Prog.showTriad (H.enharmonicFunc spelling) chord

      attachedDiags =
        [ let rootPC                = harmonicRootOf cs
              spelling              = barSpellingOf chroma rootPC
              (mMode, mParentKey)   = case mr of
                Sc.ModeOk mode     -> (Just mode, Just (Sc.parentKey mode))
                Sc.ModeInvalid _   -> (Nothing, Nothing)
          in d { sdStepNumber     = i + 1
               , sdRenderedChord  = Just (slashChordWith spelling cs)
               , sdStrataLabel    = Just s
               , sdTristrata      = Just t
               , sdTristrataIdx   = tristrataIdxOf t
               , sdMode           = mMode
               , sdStrataChroma   = Just (Sc.strataChroma s)
               , sdModeChroma     = Just chroma
               , sdSoftBoost      = Just (boostFor i)
               , sdHarmonicRootPC = Just rootPC
               , sdParentKey      = mParentKey
               , sdModeResult     = Just mr
               , sdBarSpelling    = Just spelling
               }
        | (i, d, (s, t), chroma, cs, mr) <- zip6 [0..] allBaseDiags barSeq modeChromaList chain modeResults
        ]
      attachedGen = GenerationDiagnostics
        { gdStartCadence = show (H.stateCadence start)
        , gdStartRoot    = show (H.stateCadenceRoot start)
        , gdRequestedLen = n
        , gdActualLen    = length chain
        , gdEntropy      = _gcEntropy gc
        , gdSteps        = attachedDiags
        , gdProgression  = PC.triadLayer resultPC
        }

  -- No inline printing; diagnostics and footer are emitted by the
  -- top-level caller via 'emitFinalised', so that multi-attempt mode
  -- can suppress losing attempts and surface only the winner.
  let _ = verbArg

  pure (resultPC, attachedGen)
  where
    zip4 as bs cs ds = [ (a, b, c, d) | ((a, b), (c, d)) <- zip (zip as bs) (zip cs ds) ]
    zip6 as bs cs ds es fs =
      [ (a, b, c, d, e, f)
      | ((a, b, c), (d, e, f)) <- zip (zip3 as bs cs) (zip3 ds es fs)
      ]

-- |Build a synthetic starter diagnostic for a 'genP' bar 0 (the cue).
-- The cue isn't selected from a candidate pool, so 'sdSelectedFrom' =
-- "starter" signals the renderer to omit the motion/γ cells.
mkStarterDiag :: H.CadenceState -> StepDiagnostic
mkStarterDiag cs =
  let rootPC = P.unPitchClass (P.pitchClass (H.stateCadenceRoot cs))
  in StepDiagnostic
       { sdStepNumber              = 1
       , sdPriorCadence            = ""
       , sdPriorRoot               = ""
       , sdPriorRootPC             = rootPC
       , sdSelectedDbIntervals     = ""
       , sdSelectedDbMovement      = ""
       , sdSelectedDbFunctionality = ""
       , sdGraphCount              = 0
       , sdGraphTop6               = []
       , sdFallbackCount           = 0
       , sdFallbackTop6            = []
       , sdPoolSize                = 0
       , sdEntropyUsed             = 0
       , sdGammaIndex              = -1
       , sdSelectedFrom            = "starter"
       , sdPosteriorRoot           = show (H.stateCadenceRoot cs)
       , sdPosteriorRootPC         = rootPC
       , sdRenderedChord           = Nothing
       , sdTransformTrace          = Nothing
       , sdAdvanceTrace            = Nothing
       , sdTristrataIdx            = Nothing
       , sdTristrata               = Nothing
       , sdStrataLabel             = Nothing
       , sdMode                    = Nothing
       , sdStrataChroma            = Nothing
       , sdModeChroma              = Nothing
       , sdSoftBoost               = Nothing
       , sdHarmonicRootPC          = Nothing
       , sdParentKey               = Nothing
       , sdModeResult              = Nothing
       , sdBarSpelling             = Nothing
       }

-------------------------------------------------------------------------------
-- Strata-aware partial regeneration
-------------------------------------------------------------------------------

-- |Regenerate a contiguous range of bars within an existing strata-aware
-- 'PC.ProgressionContext'. Mirrors 'runStrataGen' but seeded from the
-- source context's provenance instead of via 'initialPlacement', with a
-- one-step lookahead on the final regenerated bar so the @e → e+1@ seam
-- preserves walk-graph validity under 'Strata.allowedNext'.
--
-- By the Phase 1 invariant, any spliced sequence whose every edge satisfies
-- 'allowedNext' automatically produces only 'ModeOk' bars. Maintaining
-- adjacency at both seams (the @s-1 → s@ seam is automatic via seeding;
-- the @e → e+1@ seam is the lookahead's job) is sufficient.
runStrataGenFrom :: PC.ProgressionContext
                 -> Int   -- ^ 1-indexed start of regen range (inclusive)
                 -> Int   -- ^ 1-indexed end of regen range (inclusive); wraps when @start > end@
                 -> GenConfig
                 -> IO (PC.ProgressionContext, GenerationDiagnostics)
runStrataGenFrom srcPC s e gc = do
  let srcProvSeq = case PC.pcProvenance srcPC of
        Just sq -> sq
        Nothing -> error "runStrataGenFrom: source ProgressionContext lacks pcProvenance — call genFrom on a 'genI'/'genP'-derived context"
      srcProvList = toList srcProvSeq                -- [(Tristrata, StrataLabel)]
      srcN        = Seq.length srcProvSeq
      -- 1-indexed positions (wrap-aware).
      seedPos     = ((s - 2) `mod` srcN) + 1
      targetPos   = (e `mod` srcN) + 1
      (t_seed,   s_seed)   = srcProvList !! (seedPos   - 1)
      (t_target, s_target) = srcProvList !! (targetPos - 1)
      rSize       = if s <= e then e - s + 1 else srcN - s + 1 + e

      -- Cue: the source's triad-layer chord at seedPos. This was originally
      -- generated under the seed strata's narrowed context, so its absolute
      -- PCs lie within 's_seed's chroma — cue-validity is automatic.
      triadStates = toList (Prog.unProgression (PC.triadLayer srcPC))
      cueCS       = triadStates !! (seedPos - 1)

  rng <- createSystemRandom
  let basePctx = parseContextOnce (_gcTonal gc)
      allowed  = pcAllowedTristrata basePctx
      allowed' = if null allowed then Sc.validTristrata else allowed

      narrow :: Int -> [(Sc.StrataLabel, Sc.Tristrata)] -> [(Sc.StrataLabel, Sc.Tristrata)]
      narrow i pool =
        let pool1 = case _gcRelStrata gc of
              Just ps | not (null ps) ->
                let p = ps !! (i `mod` length ps)
                in [(s',t') | (s',t') <- pool, Sc.tristrataStrataAt t' p == s']
              _ -> pool
            pool2 = case _gcAbsStrata gc of
              Just ss | not (null ss) ->
                let lbl = ss !! (i `mod` length ss)
                in [(s',t') | (s',t') <- pool1, s' == lbl]
              _ -> pool1
        in pool2

  walkSeeds <- mapM (const (uniformRM (minBound :: Int, maxBound :: Int) rng))
                    [1 .. max 0 rSize]

  let -- Walk rSize bars seeded at (s_seed, t_seed). The final bar's pool is
      -- filtered to predecessors of the target (s_target, t_target) so the
      -- spliced sequence retains allowedNext-adjacency at the e→e+1 seam.
      -- If the filter empties the pool, fall back to the unfiltered pool.
      walkRange :: Int -> [Int] -> (Sc.StrataLabel, Sc.Tristrata) -> [(Sc.StrataLabel, Sc.Tristrata)]
      walkRange i seeds prev
        | i >= rSize = []
        | otherwise =
            let rawCands = narrow i (Strata.allowedNext allowed' prev)
                isFinal  = i == rSize - 1
                filtered =
                  if isFinal
                    then filter (\p -> (s_target, t_target)
                                       `elem` Strata.allowedNext allowed' p)
                                rawCands
                    else rawCands
                cands     = if isFinal && null filtered then rawCands else filtered
                (seed, seedsRest) = case seeds of
                  (sd : rest) -> (sd, rest)
                  []          -> (0, [])
                chosen    = fromMaybe prev (Strata.selectNextSeeded seed prev cands)
            in chosen : walkRange (i + 1) seedsRest chosen

      -- Local barSeq passed to runStrataGenBody. Index 0 = seed (the cue),
      -- indices 1..rSize = the regenerated bars.
      regenBarSeq :: [(Sc.StrataLabel, Sc.Tristrata)]
      regenBarSeq = (s_seed, t_seed) : walkRange 0 walkSeeds (s_seed, t_seed)
      regenN      = length regenBarSeq   -- = rSize + 1

      strataOvertonesString :: Sc.StrataLabel -> String
      strataOvertonesString sl =
        unwords [ show (P.sharp (P.mkPitchClass (P.unPitchClass pc))) ++ "'"
                | pc <- Sc.strataChroma sl ]

      boostFor :: Int -> Double
      boostFor 0 = 1.0
      boostFor i =
        let (sCurr, tCurr) = regenBarSeq !! i
            (sPrev, tPrev) = regenBarSeq !! (i - 1)
            sGrand         = if i >= 2 then Just (fst (regenBarSeq !! (i - 2))) else Nothing
            mSame = if sCurr == sPrev then _gcBoostSame gc else 1.0
            mFlip = case sGrand of
                      Just sg | sCurr == sg && sCurr /= sPrev -> _gcBoostFlip gc
                      _                                        -> 1.0
            mTri  = if tCurr == tPrev then _gcBoostTri gc else 1.0
            _     = tPrev
        in mSame * mFlip * mTri

      pctxAt :: Int -> ParsedContext
      pctxAt barIdx1 =
        let i      = barIdx1
            (sl, _) = regenBarSeq !! i
            ctx'   = hcOvertones (strataOvertonesString sl) (_gcTonal gc)
            pctx   = parseContextOnce ctx'
            boost  = boostFor i
        in pctx { pcSoftBoost = boost, pcStrictContainment = True }

  -- Reuse runStrataGenBody for chain construction. Result has regenN bars
  -- across all layers: index 0 = the seed (cue) bar, indices 1..rSize =
  -- the regenerated bars.
  (regenPC, regenDiag) <- runStrataGenBody s_seed gc cueCS rng s_seed t_seed
                                           regenBarSeq pctxAt boostFor regenN

  let -- Drop the cue bar (index 0) from each layer and from provenance.
      dropCue :: Prog.Progression -> Prog.Progression
      dropCue (Prog.Progression sq) = Prog.Progression (Seq.drop 1 sq)

      insertPC = PC.ProgressionContext
        { PC.triadLayer   = dropCue (PC.triadLayer regenPC)
        , PC.strataLayer  = dropCue (PC.strataLayer regenPC)
        , PC.modeLayer    = dropCue (PC.modeLayer regenPC)
        , PC.pcProvenance = case PC.pcProvenance regenPC of
            Just sq -> Just (Seq.drop 1 sq)
            Nothing -> Nothing
        }
      splicedPC = PC.pcSplice srcPC s e insertPC

  pure (splicedPC, regenDiag)

-- |Report an invalid starting cue for 'genP'. Prints a warning naming
-- the cue's escape pitches alongside a grid of viable triads in the
-- starting strata. Returns silently — the caller emits an empty
-- 'ProgressionContext' after this.
printInvalidCueError :: H.CadenceState -> Sc.StrataLabel -> IO ()
printInvalidCueError start s = do
  let rootPC       = P.unPitchClass (P.pitchClass (H.stateCadenceRoot start))
      intervals    = map P.unPitchClass (H.cadenceIntervals (H.stateCadence start))
      absPCs       = [ (i + rootPC) `mod` 12 | i <- intervals ]
      spelling     = H.inferSpelling absPCs
      enharm       = H.enharmonicFunc spelling
      chord        = H.fromCadenceState start
      chordName    = Prog.showTriad enharm chord
      strataPCs    = sort (map P.unPitchClass (Sc.strataChroma s))
      offenders    = [ p | p <- absPCs, p `notElem` strataPCs ]
      spellPC p    = show (enharm (P.mkPitchClass p))
      strataNames  = unwords (map spellPC strataPCs)
      offenderStr  = unwords (map spellPC offenders)
  putStrLn ""
  putStrLn $ "⚠ invalid starting state: " ++ chordName
             ++ "  (escapes strata " ++ show s
             ++ " — contains " ++ offenderStr ++ " outside {" ++ strataNames ++ "})"
  putStrLn $ "  viable triads in strata " ++ show s ++ " {" ++ strataNames ++ "}:"
  mapM_ (putStrLn . ("    " ++)) (viableTriadLines s enharm)
  putStrLn ""

-- |Produce one display line per root PC in the strata, listing every
-- distinct-by-name triad enumerated by 'possibleTriads' starting from
-- that root. Uses 'Prog.showTriad' for chord naming to match the grid
-- footer convention. Triads are padded to a uniform column width so
-- they align vertically across rows.
viableTriadLines :: Sc.StrataLabel -> (P.PitchClass -> P.NoteName) -> [String]
viableTriadLines s enharm =
  let strataPCs = sort (map P.unPitchClass (Sc.strataChroma s))
      triadsFor r =
        let ts = possibleTriads (r, strataPCs)
            names = nub [ Prog.showTriad enharm (H.toTriad enharm pcs) | pcs <- ts ]
        in names
      allNames  = concatMap triadsFor strataPCs
      colWidth  = if null allNames then 0 else maximum (map length allNames) + 2
      rootField = padR 4 . show . enharm . P.mkPitchClass
  in [ rootField r ++ "  " ++ concatMap (padR colWidth) (triadsFor r)
     | r <- strataPCs
     ]
  where
    padR n str = str ++ replicate (max 0 (n - length str)) ' '
