{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Core.Builder
-- Description : Generative engine for harmonic progressions with unified diagnostics interface
--
-- This module implements the main generation loop that connects:
--   * R (Rules): HarmonicContext constraints via Filter module
--   * E (Evaluation): Database-derived composer probabilities
--   * T (Traversal): Voice leading optimization
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
  ( -- * Generation
    generate
  , generateWith

    -- * Generation with Diagnostics (Verbosity 1)
  , generate'
  , genWith'
  , gen'

    -- * Generation with Max Diagnostics (Verbosity 2)
  , generate''
  , genWith''
  , gen''

    -- * Unified Interface (same return type, diagnostics printed as side effect)
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
  , hContext  -- String-friendly version for Tidal (renamed from 'context' to avoid collision)
  , defaultContext

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

    -- * String-friendly generation (for TidalCycles)
  , gen
  , genWith
  ) where

import qualified Database.Bolt as Bolt
import qualified Data.Text as T
import           Data.Text (Text)
import           Control.Monad (forM_, when)
import           Data.List (intercalate)
import           System.Random.MWC (createSystemRandom)

import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as P
import qualified Harmonic.Rules.Types.Progression as Prog
import           Harmonic.Rules.Import.Graph (connectNeo4j)
import qualified Harmonic.Evaluation.Database.Query as Q

-- Sub-module imports
import           Harmonic.Framework.Builder.Types
import           Harmonic.Framework.Builder.Portmanteau
import           Harmonic.Framework.Builder.Diagnostics
import           Harmonic.Framework.Builder.Core

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

-- |String-friendly generate for TidalCycles live coding.
-- Avoids Stringy Text conflicts in the REPL.
-- Prints only the progression grid with header (matching legacy chainCadence behavior).
--
-- Example:
--   let start = initCadenceState 0 "C" [0,4,7] FlatSpelling
--   prog <- gen start 8 "debussy stravinsky" 1.0 defaultContext
gen :: H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
gen start len composerStr entropy ctx = do
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
  -- Parse composer blend string (e.g., "bach:0.3 debussy:0.7" → Map)
  let composerWeights = Q.parseComposerWeights composerStr
      pctx = parseContextOnce context

  -- Create single RNG for entire generation run
  gen <- createSystemRandom

  -- Connect to Neo4j
  pipe <- connectNeo4j

  -- Generate the cadence chain (len-1 steps since start counts as first chord)
  chain <- Bolt.run pipe $ buildChain config gen entropy context pctx composerWeights start (len - 1)

  Bolt.close pipe

  -- Convert chain to Progression (chain already includes start)
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

-- |String-friendly generation with compact musical summary (for TidalCycles live coding).
--
-- Prints a concise summary showing the musical journey through the progression:
--   * Starting point and parameters
--   * Each step: movement, selected chord, source (graph/fallback)
--   * Final progression grid
--
-- Returns just the progression (not tuple) for clean REPL output.
gen' :: H.CadenceState -> Int -> String -> Double -> HarmonicContext
     -> IO Prog.Progression
gen' start len composerStr entropy ctx = do
  (prog, diag) <- generate' start len composerStr entropy ctx

  -- Print compact summary
  putStrLn ""
  putStrLn $ "Generation: " ++ gdStartRoot diag ++ " " ++ gdStartCadence diag
             ++ " → " ++ show (gdActualLen diag) ++ " chords (entropy " ++ show (gdEntropy diag) ++ ")"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  -- Show starting state as bar 1
  putStrLn $ "  1: " ++ gdStartRoot diag ++ " " ++ gdStartCadence diag ++ " [starting state]"
  putStrLn ""

  -- Show each step with bar number = stepNumber + 1
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

    -- Single line: bar number, state, pool, movement, chord, source, gamma
    putStrLn $ "  " ++ show barNum ++ ": " ++ stateInfo ++ "  " ++ poolInfo
               ++ "  " ++ mvmt ++ " → " ++ chord ++ "  " ++ src ++ " " ++ selIdx

    -- Top 6 candidates rendered as actual chords (with roots)
    let posteriorRootPC = sdPosteriorRootPC step

    -- Render candidate cadences as actual chords
    let renderCandidateName name =
          case parseCadenceFromString name posteriorRootPC of
            Just renderedName -> renderedName
            Nothing -> name  -- fallback to original if parse fails

    let topCands = if sdSelectedFrom step == "graph"
                   then take 6 (sdGraphTop6 step)
                   else take 6 [(n, s) | (n, s, _, _, _) <- sdFallbackTop6 step]

    when (not (null topCands)) $ do
      let candNames = [renderCandidateName name | (name, _) <- topCands]
          candStr = intercalate " | " candNames
      putStrLn $ "     Options: " ++ candStr
    putStrLn ""

  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn ""
  printHeader (T.pack composerStr) entropy ctx
  print prog
  putStrLn ""

  pure prog

-- |Generate with custom configuration, returning diagnostics tuple (internal).
genWith' :: GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext
         -> IO (Prog.Progression, GenerationDiagnostics)
genWith' config start len composerStr entropy context = do
  -- Parse composer blend string (e.g., "bach:0.3 debussy:0.7" → Map)
  let composerWeights = Q.parseComposerWeights (T.pack composerStr)
      pctx = parseContextOnce context

  -- Create single RNG for entire generation run
  gen <- createSystemRandom

  -- Connect to Neo4j
  pipe <- connectNeo4j

  -- Generate the cadence chain with diagnostics
  (chain, stepDiags) <- Bolt.run pipe $ buildChainWithDiag config gen entropy context pctx composerWeights start (len - 1)

  Bolt.close pipe

  -- Convert chain to Progression
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

-- |String-friendly generation with verbose traces (for TidalCycles live coding).
--
-- Prints detailed transform and advance traces for debugging:
--   * Per-step: movement, posterior root, rendered chord
--   * Transform trace: DB intervals → transposed → computed name (verification)
--   * Advance trace: PC arithmetic for root motion
--
-- Returns just the progression (not tuple) for clean REPL output.
gen'' :: H.CadenceState -> Int -> String -> Double -> HarmonicContext
      -> IO Prog.Progression
gen'' start len composerStr entropy ctx = do
  (prog, diag) <- generate'' start len composerStr entropy ctx

  -- Print verbose summary
  putStrLn ""
  putStrLn $ "Verbose Generation: " ++ gdStartRoot diag ++ " " ++ gdStartCadence diag
             ++ " → " ++ show (gdActualLen diag) ++ " chords (entropy " ++ show (gdEntropy diag) ++ ")"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  -- Show starting state as bar 1
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

    -- Pool composition
    putStrLn $ "  Pool: " ++ show (sdGraphCount step) ++ " graph, "
               ++ show (sdFallbackCount step) ++ " fallback"

    -- Top 6 candidates from selected pool (not rounded, show actual scores)
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
    -- Advance trace (if available)
    case sdAdvanceTrace step of
      Just at -> do
        putStrLn $ "  Advance: " ++ atCurrentRoot at ++ " (" ++ show (atCurrentRootPC at) ++ ")"
                   ++ " + " ++ show (atMovementInterval at) ++ " → "
                   ++ atNewRoot at ++ " (" ++ show (atNewRootPC at) ++ ")"
      Nothing -> return ()

    putStrLn ""

  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn ""
  printHeader (T.pack composerStr) entropy ctx
  print prog
  putStrLn ""

  pure prog

-- |Generate with custom configuration and maximum diagnostics (internal).
genWith'' :: GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext
          -> IO (Prog.Progression, GenerationDiagnostics)
genWith'' config start len composerStr entropy context = do
  -- Parse composer blend string (e.g., "bach:0.3 debussy:0.7" → Map)
  let composerWeights = Q.parseComposerWeights (T.pack composerStr)
      pctx = parseContextOnce context

  -- Create single RNG for entire generation run
  gen <- createSystemRandom

  -- Connect to Neo4j
  pipe <- connectNeo4j

  -- Generate the cadence chain with max diagnostics (verbosity = 2)
  (chain, stepDiags) <- Bolt.run pipe $ buildChainWithDiagV config gen 2 entropy context pctx composerWeights start (len - 1)

  Bolt.close pipe

  -- Convert chain to Progression
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
