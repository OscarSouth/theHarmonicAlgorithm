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
genFrom :: PC.ProgressionContext -> (Int, Int) -> GenConfig
genFrom pc (s, e) = defaultGenConfig
  { _gcCue  = inferCue
  , _gcLen  = rSize
  , _gcMode = FromProg prog s e
  }
  where
    prog = PC.triadLayer pc
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

-- |Terminal executor producing a 'PC.ProgressionContext'. Dispatches on
-- '_gcMode': 'StrataMode' runs the strata-first traversal producing distinct
-- layers; all other modes fall through to 'execGenConfig' and wrap the
-- resulting triad 'Prog.Progression' via 'PC.fromProgression'.
execGenConfigPC :: GenConfig -> IO PC.ProgressionContext
execGenConfigPC gc = case _gcMode gc of
  StrataMode sStart -> runStrataGen sStart gc
  _                 -> PC.fromProgression <$> execGenConfig gc

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
runStrataGen :: Sc.StrataLabel -> GenConfig -> IO PC.ProgressionContext
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
      pure PC.ProgressionContext
        { PC.triadLayer   = mempty
        , PC.strataLayer  = mempty
        , PC.modeLayer    = mempty
        , PC.pcProvenance = Just Seq.empty
        }
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
  -> IO PC.ProgressionContext
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

      -- Mode value used for the mode-layer Cadence + diagnostic 'sdMode'.
      -- 'ModeInvalid' falls back to Aeolian rooted on the triad root so
      -- the mode layer still has a sensible 7-PC chroma to draw from.
      modeOf :: H.CadenceState -> Sc.ModeResult -> Sc.Mode
      modeOf cs (Sc.ModeOk m)        = m
      modeOf cs (Sc.ModeInvalid _)   = Sc.Mode Sc.Aeolian (P.mkPitchClass (harmonicRootOf cs))

      modeList :: [Sc.Mode]
      modeList = zipWith modeOf chain modeResults

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
      -- chroma respectively.
      mkAuxLayers :: H.CadenceState
                  -> Sc.StrataLabel
                  -> Sc.Mode
                  -> (H.CadenceState, H.CadenceState)
      mkAuxLayers triadCS sCurr mode =
        let rootPC     = harmonicRootOf triadCS
            rootNote   = harmonicRootNote triadCS
            strataInts = chromaIntervals rootPC (Sc.strataChroma sCurr)
            modeInts   = chromaIntervals rootPC (Sc.modeChroma mode)
            strataCS   = mkChromaCS rootNote strataInts
            modeCS     = mkChromaCS rootNote modeInts
        in (strataCS, modeCS)

      stratas = [s | (s, _) <- pairs]
      modes   = [m | (_, m) <- pairs]
      pairs   = zipWith3 mkAuxLayers chain (map fst barSeq) modeList

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
        [ let rootPC    = harmonicRootOf cs
              chroma    = case mr of
                Sc.ModeOk mode     -> Sc.modeChroma mode
                Sc.ModeInvalid pcs -> pcs
              spelling  = barSpellingOf chroma rootPC
          in d { sdStepNumber     = i + 1
               , sdRenderedChord  = Just (slashChordWith spelling cs)
               , sdStrataLabel    = Just s
               , sdTristrata      = Just t
               , sdTristrataIdx   = tristrataIdxOf t
               , sdMode           = Just m
               , sdStrataChroma   = Just (Sc.strataChroma s)
               , sdModeChroma     = Just (Sc.modeChroma m)
               , sdSoftBoost      = Just (boostFor i)
               , sdHarmonicRootPC = Just rootPC
               , sdParentKey      = Just (Sc.parentKey m)
               , sdModeResult     = Just mr
               , sdBarSpelling    = Just spelling
               }
        | (i, d, (s, t), m, cs, mr) <- zip6 [0..] allBaseDiags barSeq modeList chain modeResults
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

  case verbArg of
    Nothing -> pure ()
    Just v  -> printStrataDiagnostics v attachedGen

  -- Always emit the gen-style footer: composer/entropy/context header
  -- followed by the triad progression in chord-grid form. This matches
  -- the behaviour of 'genPrint'/'genPrint''/'genPrint''''' from the
  -- legacy 'gen' family.
  putStrLn ""
  printHeader (T.pack (_gcSeek gc)) (_gcEntropy gc) (_gcTonal gc)
  print (PC.triadLayer resultPC)
  putStrLn ""

  pure resultPC
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
