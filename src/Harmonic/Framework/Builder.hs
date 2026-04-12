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

    -- * Generation Modifiers
  , cue
  , len
  , seek
  , entropy
  , tonal

    -- * Generation Configuration
  , GenConfig(..)
  , GenMode(..)
  , Verbosity(..)
  , defaultGenConfig
  , execGenConfig

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
import           System.Random.MWC (createSystemRandom, uniformRM)

import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as P
import qualified Harmonic.Rules.Types.Progression as Prog
import           Harmonic.Rules.Import.Graph (connectNeo4j)
import qualified Harmonic.Evaluation.Database.Query as Q
import           Harmonic.Rules.Constraints.Filter (parseTuningNamed, isWildcard)
import           Harmonic.Rules.Constraints.Overtone (formatOvertoneAnnotation, formatOvertoneAnnotationPipe)
import           Data.Foldable (toList)

-- Sub-module imports
import           Harmonic.Framework.Builder.Types
import           Harmonic.Framework.Builder.Portmanteau
import           Harmonic.Framework.Builder.Diagnostics
import           Harmonic.Framework.Builder.Core

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

  -- Parse tuning for overtone annotation (only when non-wildcard)
  let tuningNames = parseTuningNamed (_hcOvertones ctx)
      hasAnnotation = not (null tuningNames)
      allStates = toList (Prog.unProgression (gdProgression diag))
      -- Compute absolute pitch classes and format annotation for a CadenceState
      annotateState cs =
        let rootPC = P.unPitchClass (P.pitchClass (H.stateCadenceRoot cs))
            intervals = map P.unPitchClass (H.cadenceIntervals (H.stateCadence cs))
            absPitches = map (\i -> (i + rootPC) `mod` 12) intervals
            spelling = H.stateSpelling cs
            pcName pc = show (H.enharmonicFunc spelling (P.mkPitchClass pc))
        in formatOvertoneAnnotationPipe tuningNames absPitches pcName

  -- Print compact summary
  putStrLn ""
  putStrLn $ "Generation: " ++ gdStartRoot diag ++ " " ++ gdStartCadence diag
             ++ " → " ++ show (gdActualLen diag) ++ " chords (entropy " ++ show (gdEntropy diag) ++ ")"
  putStrLn $ composerModeStr composerStr
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  -- Show starting state as bar 1
  let bar1Suffix = if hasAnnotation && not (null allStates)
                   then let ann = annotateState (head allStates)
                        in if null ann then "" else "  " ++ ann
                   else ""
  putStrLn $ "  1: " ++ gdStartRoot diag ++ " " ++ gdStartCadence diag
             ++ " [starting state]" ++ bar1Suffix
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

    -- Overtone annotation suffix (inline on step line)
    let overtoneSuffix =
          if hasAnnotation
          then let stateIdx = barNum - 1
               in if stateIdx >= 0 && stateIdx < length allStates
                  then let ann = annotateState (allStates !! stateIdx)
                       in if null ann then "" else "  " ++ ann
                  else ""
          else ""

    -- Single line: bar number, state, pool, movement, chord, source, gamma, overtones
    putStrLn $ "  " ++ show barNum ++ ": " ++ stateInfo ++ "  " ++ poolInfo
               ++ "  " ++ mvmt ++ " → " ++ chord ++ "  " ++ src ++ " " ++ selIdx
               ++ overtoneSuffix

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
      putStrLn $ "     Candidates: " ++ candStr

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

  -- Print verbose summary
  putStrLn ""
  putStrLn $ "Verbose Generation: " ++ gdStartRoot diag ++ " " ++ gdStartCadence diag
             ++ " → " ++ show (gdActualLen diag) ++ " chords (entropy " ++ show (gdEntropy diag) ++ ")"
  putStrLn $ composerModeStr composerStr
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
execGenConfig :: GenConfig -> IO Prog.Progression
execGenConfig gc = do
  start <- _gcCue gc
  case _gcMode gc of
    Fresh -> case _gcVerbosity gc of
      Silent   -> genPrint   start (_gcLen gc) (_gcSeek gc) (_gcEntropy gc) (_gcTonal gc)
      Standard -> genPrint'  start (_gcLen gc) (_gcSeek gc) (_gcEntropy gc) (_gcTonal gc)
      Verbose  -> genPrint'' start (_gcLen gc) (_gcSeek gc) (_gcEntropy gc) (_gcTonal gc)

    GridMode -> do
      let grid = Prog.fromCadenceStates (replicate (_gcLen gc) start)
      putStrLn ""
      print grid
      putStrLn ""
      pure grid

    FromProg srcProg s e -> do
      -- Generate _gcLen+1 chords (cue + new), then drop cue
      fullProg <- generateWith defaultConfig start
                    (_gcLen gc + 1) (T.pack $ _gcSeek gc)
                    (_gcEntropy gc) (_gcTonal gc)
      let newChords = tail $ toList $ Prog.unProgression fullProg
          result = Prog.spliceProgression srcProg s e newChords
      putStrLn ""
      printHeader (T.pack $ _gcSeek gc) (_gcEntropy gc) (_gcTonal gc)
      print result
      putStrLn ""
      pure result

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
  { _gcCue       = defaultCue
  , _gcLen       = 4
  , _gcSeek      = "*"
  , _gcEntropy   = 0.2
  , _gcTonal     = hContext
  , _gcVerbosity = Silent
  , _gcMode      = Fresh
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
-- @s' <- seek "*" $ entropy 0.3 $ genFrom s (2,3)@
-- @s' <- seek "*" $ cue start $ genFrom s (2,3)   -- override inferred cue@
-- @s' <- seek "*" $ len 6 $ genFrom s (2,3)        -- expand range@
genFrom :: Prog.Progression -> (Int, Int) -> GenConfig
genFrom prog (s, e) = defaultGenConfig
  { _gcCue  = inferCue
  , _gcLen  = rSize
  , _gcMode = FromProg prog s e
  }
  where
    n = Prog.progLength prog
    rSize = if s <= e then e - s + 1 else n - s + 1 + e
    cuePos = ((s - 2) `mod` n) + 1  -- 1-indexed, wraps to N when s=1
    inferCue = case Prog.getCadenceState prog cuePos of
      Just cs -> pure cs
      Nothing -> _gcCue defaultGenConfig

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
len n gc = gc { _gcLen = n }

-- |Set composer blend and execute. Terminal modifier — produces IO Progression.
--
-- @s <- seek "*" $ gen@
-- @s <- seek "bach:70 debussy:30" $ cue start $ len 4 $ gen@
seek :: String -> GenConfig -> IO Prog.Progression
seek s gc = execGenConfig gc { _gcSeek = s }

-- |Set entropy (gamma shape parameter). Higher values = more unusual choices.
--
-- @s <- seek "*" $ entropy 0.5 $ gen@
entropy :: Double -> GenConfig -> GenConfig
entropy e gc = gc { _gcEntropy = e }

-- |Set harmonic context (R constraints).
--
-- @s <- seek "*" $ tonal (hcKey "0#" $ hContext) $ gen@
tonal :: HarmonicContext -> GenConfig -> GenConfig
tonal ctx gc = gc { _gcTonal = ctx }
