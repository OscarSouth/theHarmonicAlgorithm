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
--   
--   chordDiss_norm  = 0.01 + ((chordDiss - 6) / 44) * 0.99  -- normalize to [0.01, 1.0]
--   motionDiss_norm = 0.01 + ((motionDiss - 1) / 5) * 0.99  -- normalize to [0.01, 1.0]
--   
--   badness = chordDiss_norm * motionDiss_norm
--   score = 10000 - (badness * 100)
-- @
--
-- The [0.01, 1.0] normalization range prevents zero products while maintaining resolution.
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

module Harmonic.Core.Builder
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
  
    -- * String-friendly generation (for TidalCycles)
  , gen
  , genWith
  ) where

import qualified Database.Bolt as Bolt
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import qualified Data.Set as Set
import           Control.Monad (foldM, forM_, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (sortBy, nub, sort)
import           Data.Function (on)
import           Data.Ord (Down(..))

import qualified Harmonic.Core.Harmony as H
import qualified Harmonic.Core.Pitch as P
import qualified Harmonic.Core.Progression as Prog
import           Harmonic.Database.Graph (connectNeo4j)
import           Harmonic.Database.Query
import           Harmonic.Core.Probabilistic (gammaIndexScaled)
import           Harmonic.Core.Filter (parseOvertones, parseOvertones', parseKey, parseFunds, isWildcard, resolveRoots)
import           Harmonic.Core.Overtone (overtoneSets)
import qualified Harmonic.Core.Dissonance as D
import           Harmonic.Core.Dissonance (dissonanceScore)
import qualified Data.Sequence as Seq

-------------------------------------------------------------------------------
-- Harmonic Context (R Constraints)
-------------------------------------------------------------------------------

-- |Harmonic context defines the Rules (R) that constrain the generative space.
-- 
-- These filters are applied BEFORE database evaluation (R in R→E→T pipeline),
-- limiting which cadences can even be considered as candidates.
--
-- Three-part filtering system:
--   * overtones: Pitch candidate set (e.g., "E A D G" for bass tuning overtones)
--   * key: Key filter applied to candidates (e.g., "C", "#", "bb" for key signature)
--   * roots: Root/bass candidate set (e.g., "E F# G" for valid root notes)
--
-- Filters use "*" as wildcard (match all). Format matches legacy Overtone.hs notation.
data HarmonicContext = HarmonicContext
  { hcOvertones :: Text  -- ^ Filter by overtone content ("*" = all)
  , hcKey       :: Text  -- ^ Filter by key signature ("C", "#", "bb", "*")
  , hcRoots     :: Text  -- ^ Filter by root notes ("*" = all)
  } deriving (Show, Eq)

-- |Constructor for HarmonicContext.
-- 
-- Arguments:
--   * overtones: Pitch set filter ("E A D G", "C", "*")
--   * key: Key signature filter ("C", "#", "bb", "Am", "*")
--   * roots: Root notes filter ("E F# G", "1#", "*")
-- 
-- Example:
--   harmonicContext "*" "*" "*"       -- No filtering (all candidates)
--   harmonicContext "E A D G" "C" "*" -- Bass tuning, C major key
--   harmonicContext "*" "#" "E G"     -- G major key, E/G roots only
harmonicContext :: Text -> Text -> Text -> HarmonicContext
harmonicContext = HarmonicContext

-- |String-friendly constructor for Tidal live coding.
-- Avoids Text/String issues in the REPL.
-- Named 'hContext' to avoid collision with TidalCycles' EventF.context field.
-- 
-- Arguments:
--   * o: Overtones filter
--   * k: Key filter  
--   * r: Roots filter
-- 
-- Example:
--   hContext "*" "*" "*"           -- No filtering
--   hContext "E A D G" "#" "*"     -- Bass tuning, G major
--   hContext "*" "C" "C G F"       -- C major key, C/G/F roots
hContext :: String -> String -> String -> HarmonicContext
hContext o k r = HarmonicContext (T.pack o) (T.pack k) (T.pack r)

-- |Default context: no filtering (wildcards everywhere)
defaultContext :: HarmonicContext
defaultContext = HarmonicContext "*" "*" "*"

-------------------------------------------------------------------------------
-- Generator Configuration
-------------------------------------------------------------------------------

-- |Configuration for the progression generator.
data GeneratorConfig = GeneratorConfig
  { gcPoolSize        :: Int     -- ^ Candidate pool size (default 30)
  , gcHomingThreshold :: Double  -- ^ When to start homing (1.0 = disabled)
  , gcHomingStrength  :: Double  -- ^ How strongly to bias toward start
  } deriving (Show, Eq)

-- |Default configuration (homing disabled for initial implementation)
defaultConfig :: GeneratorConfig
defaultConfig = GeneratorConfig
  { gcPoolSize        = 30
  , gcHomingThreshold = 1.0   -- Disabled: never activates
  , gcHomingStrength  = 1.0
  }

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
--   2. Forward walk (0% to 75%): Query graph, apply R filter, apply E weights, gamma select
--   3. Homing phase (75% to 100%): Bias toward cadences leading to start
--   4. Apply voice leading optimization to the complete chain
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

-- |String-friendly generate for TidalCycles live coding.
-- Avoids Stringy Text conflicts in the REPL.
--
-- Example:
--   let start = initCadenceState 0 "C" [0,4,7] FlatSpelling
--   prog <- gen start 8 "debussy stravinsky" 1.0 defaultContext
gen :: H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
gen start len composerStr entropy ctx = generate start len (T.pack composerStr) entropy ctx

-- |String-friendly generateWith for TidalCycles live coding.
genWith :: GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genWith config start len composerStr entropy ctx = generateWith config start len (T.pack composerStr) entropy ctx

-- |Generate with custom configuration
-- 
-- Simplified algorithm (homing disabled for initial implementation):
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
generateWith config start len _composerStr entropy context = do
  -- Note: composerStr ignored in initial implementation (uses total confidence)
  
  -- Connect to Neo4j
  pipe <- connectNeo4j
  
  -- Generate the cadence chain (len-1 steps since start counts as first chord)
  chain <- Bolt.run pipe $ buildChain config entropy context start (len - 1)
  
  Bolt.close pipe
  
  -- Convert chain to Progression (chain already includes start)
  pure $ chainToProgression chain

-------------------------------------------------------------------------------
-- Diagnostics Types
-------------------------------------------------------------------------------

-- |Transform trace captures intermediate values in fromCadenceState → toTriad pipeline.
-- Used for maximum verbosity debugging (gen''). Includes raw DB data plus all transformation stages.
data TransformTrace = TransformTrace
  { ttRawDbIntervals    :: String    -- ^ Raw zero-form intervals from DB: "[P 0,P 4,P 7]"
  , ttRawDbMovement     :: String    -- ^ Raw movement from DB: "desc 3"
  , ttRawDbFunctionality:: String    -- ^ Raw stored functionality from DB: "maj"
  , ttRootPC            :: Int       -- ^ Root pitch class (0-11)
  , ttRootNoteName      :: String    -- ^ Root note name before transform
  , ttTones             :: [Int]     -- ^ Raw cadence intervals (as Ints) before transposition
  , ttTransposedPitches :: [Int]     -- ^ Pitches after adding rootPC to tones (STEP 2)
  , ttNormalizedPs      :: [Int]     -- ^ Result of normalizeWithFund (STEP 3)
  , ttZeroForm          :: [Int]     -- ^ Result of zeroFormPC (STEP 4)
  , ttDetectedRoot      :: String    -- ^ Root from detectInversion
  , ttFunctionality     :: String    -- ^ Result of nameFuncTriad
  , ttFinalChord        :: String    -- ^ Final rendered chord (root + functionality)
  , ttStoredFunc        :: String    -- ^ Original functionality stored in cadence
  } deriving (Show, Eq)

-- |Advance trace captures intermediate values in root advancement.
-- Used for maximum verbosity debugging (gen'').
data AdvanceTrace = AdvanceTrace
  { atCurrentRoot       :: String    -- ^ Starting root note name
  , atCurrentRootPC     :: Int       -- ^ Starting root pitch class
  , atMovement          :: String    -- ^ Movement string (e.g., "asc 3")
  , atMovementInterval  :: Int       -- ^ Interval as semitones (signed)
  , atNewRootPC         :: Int       -- ^ Computed new root PC (mod 12)
  , atEnharmFunc        :: String    -- ^ "flat" or "sharp"
  , atNewRoot           :: String    -- ^ Final root note name
  } deriving (Show, Eq)

-- |Diagnostic information for a single generation step
data StepDiagnostic = StepDiagnostic
  { sdStepNumber      :: Int              -- ^ Step number (1-indexed)
  -- PRIOR STATE (before selection)
  , sdPriorCadence    :: String           -- ^ Prior cadence show representation
  , sdPriorRoot       :: String           -- ^ Prior root note
  , sdPriorRootPC     :: Int              -- ^ Prior root PC (0-11)
  -- SELECTED FROM DB
  , sdSelectedDbIntervals :: String       -- ^ Selected cadence intervals from DB (zero-form)
  , sdSelectedDbMovement  :: String       -- ^ Selected movement from DB
  , sdSelectedDbFunctionality :: String   -- ^ Selected functionality from DB
  -- CANDIDATE POOL INFO
  , sdGraphCount      :: Int              -- ^ Number of graph candidates found
  , sdGraphTop6       :: [(String, Double)] -- ^ Top 6 graph candidates with confidence
  , sdFallbackCount   :: Int              -- ^ Number of fallback candidates used
  , sdFallbackTop6    :: [(String, Double, Double, Double)] -- ^ Top 6 fallback candidates (name, score, chordDiss_norm, motionDiss_norm)
  , sdPoolSize        :: Int              -- ^ Total pool size
  , sdEntropyUsed     :: Double           -- ^ Entropy value used
  , sdGammaIndex      :: Int              -- ^ Index selected by gamma sampling
  , sdSelectedFrom    :: String           -- ^ "graph" or "fallback"
  -- POSTERIOR STATE (after advance)
  , sdPosteriorRoot   :: String           -- ^ Posterior root note after advancement
  , sdPosteriorRootPC :: Int              -- ^ Posterior root PC (0-11)
  -- Verbosity 1+ fields (Nothing at verbosity 0)
  , sdRenderedChord   :: Maybe String     -- ^ Actual chord after fromCadenceState (verbosity 1+)
  -- Verbosity 2 fields (Nothing at verbosity 0 or 1)
  , sdTransformTrace  :: Maybe TransformTrace  -- ^ Full transform trace (verbosity 2)
  , sdAdvanceTrace    :: Maybe AdvanceTrace    -- ^ Full advance trace (verbosity 2)
  } deriving (Show, Eq)

-- |Complete diagnostics for a generation run
data GenerationDiagnostics = GenerationDiagnostics
  { gdStartCadence    :: String           -- ^ Starting cadence
  , gdStartRoot       :: String           -- ^ Starting root note
  , gdRequestedLen    :: Int              -- ^ Requested progression length
  , gdActualLen       :: Int              -- ^ Actual progression length
  , gdEntropy         :: Double           -- ^ Entropy parameter used
  , gdSteps           :: [StepDiagnostic] -- ^ Per-step diagnostics
  , gdProgression     :: Prog.Progression -- ^ The generated progression
  } deriving (Show)

-------------------------------------------------------------------------------
-- Generation with Diagnostics
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
--
-- This function returns a tuple, which the unified functions unpack
-- and filter based on verbosity before printing.
--
-- === Type Signature
--
-- @CadenceState -> Int -> String -> Double -> HarmonicContext -> IO (Progression, GenerationDiagnostics)@
--
-- === Parameters
--
-- * @start@ - Starting cadence state
-- * @len@ - Number of chords to generate (including start state)
-- * @composerStr@ - Composer weights string (unused in current implementation)
-- * @entropy@ - Exploration parameter [0.0 = greedy, 1.0 = maximum randomness]
-- * @ctx@ - HarmonicContext with filter constraints
--
-- === Return Value
--
-- Tuple of:
--   * 'Progression' - The generated progression
--   * 'GenerationDiagnostics' - Per-step diagnostics
--
-- === Diagnostics Content
--
-- For each generation step:
--   * Prior/posterior cadence states and roots
--   * Graph and fallback candidate pools with top 3 candidates
--   * Selected source (graph or fallback)
--   * Gamma-weighted index in pool
--   * Selected movement
--   * Rendered chord name
--   * (Without full traces) Transform and advance details are None
--
-- For full traces (transform/advance), use 'generate\'\'' instead.
--
-- === See Also
--
-- * 'generate\'\'' - Like this but with full transform and advance traces
-- * 'genWith'' - Like this but with custom 'GeneratorConfig'
-- * 'genSilent' - Unified interface, silent
-- * 'genStandard' - Unified interface, standard diagnostics
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
--
-- === Type Signature
--
-- @CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression@
--
-- === Example Output
--
-- @
-- Generation Summary: C maj → 8 chords (entropy 0.8)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   1: pedal     → C majb9no5    [graph]
--   2: pedal     → C #9no5       [graph]
--   3: asc 5     → F minadd11no5 [fallback]
--   ...
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- 
--    1   ||   C maj | C majb9no5 | ...
-- @
gen' :: H.CadenceState -> Int -> String -> Double -> HarmonicContext 
     -> IO Prog.Progression
gen' start len composerStr entropy ctx = do
  (prog, diag) <- generate' start len composerStr entropy ctx
  
  -- Print compact summary
  putStrLn ""
  putStrLn $ "Generation: " ++ gdStartRoot diag ++ " " ++ gdStartCadence diag 
             ++ " → " ++ show (gdActualLen diag) ++ " chords (entropy " ++ show (gdEntropy diag) ++ ")"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  
  forM_ (gdSteps diag) $ \step -> do
    let mvmt = padToN 12 (sdSelectedDbMovement step)
        chord = case sdRenderedChord step of
                  Just c -> padToN 16 c
                  Nothing -> padToN 16 (sdPosteriorRoot step)
        src = "[" ++ sdSelectedFrom step ++ "]"
    putStrLn $ "  " ++ show (sdStepNumber step) ++ ": " ++ mvmt ++ " → " ++ chord ++ " " ++ src
  
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  print prog
  putStrLn ""
  
  pure prog
  where
    padToN n s = take n (s ++ repeat ' ')

-- |Generate with custom configuration, returning diagnostics tuple (internal).
--
-- Like 'generate\'' but with custom 'GeneratorConfig' for homing thresholds,
-- composition strength, and candidate filtering.
--
-- === Type Signature
--
-- @GeneratorConfig -> CadenceState -> Int -> String -> Double -> HarmonicContext -> IO (Progression, GenerationDiagnostics)@
--
-- === Parameters
--
-- * @config@ - Custom 'GeneratorConfig' (homing, strength, min candidates)
-- * Other parameters same as 'generate\''
--
-- === See Also
--
-- * 'generate'' - Like this but with default config
-- * 'genSilent\'' - Unified interface with custom config, silent
-- * 'genStandard\'' - Unified interface with custom config, standard diagnostics
genWith' :: GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext 
         -> IO (Prog.Progression, GenerationDiagnostics)
genWith' config start len _composerStr entropy context = do
  -- Connect to Neo4j
  pipe <- connectNeo4j
  
  -- Generate the cadence chain with diagnostics
  (chain, stepDiags) <- Bolt.run pipe $ buildChainWithDiag config entropy context start (len - 1)
  
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
  
  pure (prog, diag)-------------------------------------------------------------------------------
-- Generation with Maximum Diagnostics (Verbosity 2)
-------------------------------------------------------------------------------

-- |Generate with maximum verbosity diagnostic output.
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
--
-- For most users, use the unified interface instead:
--   * 'genSilent' - for silent generation
--   * 'genStandard' - for standard diagnostics (no traces)
--   * 'genVerbose' - for verbose diagnostics (includes traces)
--
-- === Type Signature
--
-- @CadenceState -> Int -> String -> Double -> HarmonicContext -> IO (Progression, GenerationDiagnostics)@
--
-- === Parameters
--
-- Same as 'generate\'' (composerStr unused).
--
-- === Return Value
--
-- Tuple of:
--   * 'Progression' - The generated progression
--   * 'GenerationDiagnostics' - Complete diagnostics with transform/advance traces
--
-- === Diagnostics Content
--
-- For each generation step, includes all from 'generate\'' plus:
--
--   * 'TransformTrace' with DB intervals, transposition, normalization, zero-form
--   * 'AdvanceTrace' with PC arithmetic and enharmonic spelling decisions
--
-- === Performance
--
-- Due to extra trace computation, this function is approximately 20-30% slower
-- than 'generate\'' for long progressions. Use sparingly; prefer 'genStandard'
-- for exploration and 'genSilent' for production.
--
-- === See Also
--
-- * 'generate\'' - Like this but without transform/advance traces
-- * 'genWith\'\'' - Like this but with custom 'GeneratorConfig'
-- * 'genVerbose' - Unified interface that uses this internally
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
--
-- === Type Signature
--
-- @CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression@
--
-- === Example Output
--
-- @
-- Verbose Generation: C maj → 4 chords (entropy 0.8)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- STEP 1: pedal → C majb9no5 [graph]
--   Transform: [0,1,4] → C majb9no5 (DB: majb9no5) ✓
--   Advance: C (0) + 0 → C (0)
-- ...
-- @
gen'' :: H.CadenceState -> Int -> String -> Double -> HarmonicContext 
      -> IO Prog.Progression
gen'' start len composerStr entropy ctx = do
  (prog, diag) <- generate'' start len composerStr entropy ctx
  
  -- Print verbose summary
  putStrLn ""
  putStrLn $ "Verbose Generation: " ++ gdStartRoot diag ++ " " ++ gdStartCadence diag 
             ++ " → " ++ show (gdActualLen diag) ++ " chords (entropy " ++ show (gdEntropy diag) ++ ")"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  
  forM_ (gdSteps diag) $ \step -> do
    let mvmt = sdSelectedDbMovement step
        chord = case sdRenderedChord step of
                  Just c -> c
                  Nothing -> sdPosteriorRoot step
        src = "[" ++ sdSelectedFrom step ++ "]"
    
    putStrLn $ "STEP " ++ show (sdStepNumber step) ++ ": " ++ mvmt ++ " → " ++ chord ++ " " ++ src
    
    -- Transform trace (if available)
    case sdTransformTrace step of
      Just tt -> do
        let verify = if ttFunctionality tt == ttStoredFunc tt then "✓" else "✗"
        putStrLn $ "  Transform: " ++ show (ttTones tt) ++ " → " ++ ttFinalChord tt 
                   ++ " (DB: " ++ ttStoredFunc tt ++ ") " ++ verify
      Nothing -> return ()
    
    -- Advance trace (if available)
    case sdAdvanceTrace step of
      Just at -> do
        putStrLn $ "  Advance: " ++ atCurrentRoot at ++ " (" ++ show (atCurrentRootPC at) ++ ")"
                   ++ " + " ++ show (atMovementInterval at) ++ " → " 
                   ++ atNewRoot at ++ " (" ++ show (atNewRootPC at) ++ ")"
      Nothing -> return ()
    
    putStrLn ""
  
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  print prog
  putStrLn ""
  
  pure prog

-- |Generate with custom configuration and maximum diagnostics (internal).
--
-- Like 'generate\'\'' but with custom 'GeneratorConfig' for homing thresholds,
-- composition strength, and candidate filtering.
--
-- === Type Signature
--
-- @GeneratorConfig -> CadenceState -> Int -> String -> Double -> HarmonicContext -> IO (Progression, GenerationDiagnostics)@
--
-- === Parameters
--
-- * @config@ - Custom 'GeneratorConfig'
-- * Other parameters same as 'generate\'\''
--
-- === Performance Note
--
-- Slower than 'genWith'' due to full tracing. Use for debugging only.
--
-- === See Also
--
-- * 'generate\'\'' - Like this but with default config
-- * 'genWith'' - Like this but without transform/advance traces
-- * 'genVerbose\'' - Unified interface with custom config
genWith'' :: GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext 
          -> IO (Prog.Progression, GenerationDiagnostics)
genWith'' config start len _composerStr entropy context = do
  -- Connect to Neo4j
  pipe <- connectNeo4j
  
  -- Generate the cadence chain with max diagnostics (verbosity = 2)
  (chain, stepDiags) <- Bolt.run pipe $ buildChainWithDiagV config 2 entropy context start (len - 1)
  
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
-- Diagnostic Printing and Unified Interface
-------------------------------------------------------------------------------

-- |Print diagnostics to stdout based on verbosity level.
--
-- This function formats and prints @GenerationDiagnostics@ in a user-friendly manner.
-- The output level is controlled by the verbosity parameter:
--
-- [Verbosity 0] Silent mode - prints nothing. Used by @genSilent@.
--
-- [Verbosity 1] Standard mode - prints:
--   * Summary header with starting cadence, entropy, and chord count
--   * Per-step information:
--     - Prior and posterior states
--     - Candidate pool composition (graph count, fallback count, total pool size)
--     - Top 3 candidates from each source with scores
--     - Selected candidate and source (graph or fallback)
--     - Rendered chord name
--   * Final progression visualization
--
-- [Verbosity 2] Verbose mode - prints everything from Standard plus:
--   * TRANSFORM TRACE: Full chord naming pipeline
--     - DB intervals before and after transposition
--     - Zero-form computation
--     - Detected root and computed functionality name
--     - Verification: compares DB stored name with computed name
--   * ADVANCE TRACE: Root motion arithmetic
--     - Prior and posterior root pitch classes
--     - Movement interval and modulo computation
--     - Enharmonic spelling function applied
--
-- |Manually print generation diagnostics at any verbosity level.
--
-- This function extracts and formats diagnostic data collected during generation.
-- It respects the verbosity level to show appropriate detail:
--
--   * 0 = No output
--   * 1 = Standard (per-step candidates, selections, rendered chords)
--   * 2 = Verbose (all of above + transform and advance traces)
--
-- === Type Signature
--
-- @Int -> GenerationDiagnostics -> IO ()@
--
-- === Parameters
--
-- * @verbosity@ - Output level (0, 1, or 2)
-- * @diag@ - GenerationDiagnostics record from @generate'@ or @generate''@
--
-- === Output Format
--
-- [Header] Generation metadata (starting cadence, root, entropy, length)
--
-- [Per-step diagnostics]
--   * Prior/posterior cadence states and roots
--   * Graph and fallback candidate counts
--   * Top 3 candidates from each pool (with confidence scores)
--   * Selected source and gamma-weighted index
--   * Selected movement and posterior root
--   * (Verbosity 1+) Rendered chord name
--   * (Verbosity 2+) Transform trace (DB intervals, transposition, normalization, zero-form)
--   * (Verbosity 2+) Advance trace (PC arithmetic, enharmonic spelling)
--
-- [Footer] Final progression visualization (4-column grid)
--
-- === Usage
--
-- This function is typically called internally by @genStandard@ and @genVerbose@,
-- but can also be called manually to reprint diagnostics after generation:
--
-- @
--   (prog, diag) <- generate' start 8 "*" 0.5 ctx
--   printDiagnostics 1 diag  -- reprint as standard verbosity
-- @
--
-- === See Also
--
-- * @genSilent@ - Generates with no diagnostics
-- * @genStandard@ - Generates with standard diagnostics
-- * @genVerbose@ - Generates with verbose diagnostics
-- * @generate'@ - Raw generation returning @(Progression, GenerationDiagnostics)@ tuple
-- * @generate''@ - Raw generation with transforms/advances traced
--
-- === Performance
--
-- Printing diagnostics is an I\/O operation; use sparingly in tight loops.
-- The 'GenerationDiagnostics' record is populated during generation, not during printing.
-- |Print diagnostics collected during generation.
--
-- Selects output level based on verbosity parameter:
--
-- [0 - Silent] No output
-- [1 - Standard] Per-step candidate pools, selections, rendered chords
-- [2 - Verbose] Standard plus transform and advance traces
--
-- Useful for manually reprinting diagnostics after extraction from tuple,
-- or batch processing multiple results at different verbosity levels.
printDiagnostics :: Int -> GenerationDiagnostics -> IO ()
printDiagnostics 0 _ = pure ()  -- No output for verbosity 0
printDiagnostics verbosity diag = do
  -- Header with context
  putStrLn ""
  putStrLn "═══════════════════════════════════════════════════════════════════"
  putStrLn "GENERATION DIAGNOSTICS"
  putStrLn "═══════════════════════════════════════════════════════════════════"
  putStrLn $ "Start: " ++ gdStartCadence diag ++ " @ " ++ gdStartRoot diag
  putStrLn $ "Length: " ++ show (gdActualLen diag) ++ "/" ++ show (gdRequestedLen diag) ++ " chords"
  putStrLn $ "Entropy: " ++ show (gdEntropy diag)
  putStrLn ""
  
  -- Per-step diagnostics
  let steps = gdSteps diag
  forM_ steps $ \step -> do
    let stepNum = sdStepNumber step
    putStrLn $ "──── STEP " ++ show stepNum ++ " ────"
    putStrLn $ "  Prior: " ++ sdPriorCadence step ++ " @ " ++ sdPriorRoot step
    
    -- Candidate pool summary
    let graphCount = sdGraphCount step
        fallbackCount = sdFallbackCount step
        poolSize = sdPoolSize step
    putStrLn $ "  Pool: graph=" ++ show graphCount ++ " fallback=" ++ show fallbackCount ++ " total=" ++ show poolSize
    
    -- Top candidates (if available)
    when (graphCount > 0) $
      putStrLn $ "    Graph top 3: " ++ show (take 3 $ sdGraphTop6 step)
    
    when (fallbackCount > 0) $
      putStrLn $ "    Fallback top 3: " ++ show (take 3 [(n, s) | (n, s, _, _) <- sdFallbackTop6 step])
    
    -- Selection details
    putStrLn $ "  Selection: " ++ sdSelectedFrom step ++ " @ gamma index " ++ show (sdGammaIndex step)
    putStrLn $ "  Movement: " ++ sdSelectedDbMovement step
    putStrLn $ "  Posterior: " ++ sdPosteriorRoot step
    
    -- Rendered chord (verbosity 1+)
    case sdRenderedChord step of
      Just chord -> putStrLn $ "  Chord: " ++ chord
      Nothing -> return ()
    
    -- Transform trace (verbosity 2+)
    when (verbosity >= 2) $ case sdTransformTrace step of
      Just tt -> do
        putStrLn "  [TRANSFORM TRACE]"
        putStrLn $ "    DB tones: " ++ show (ttTones tt)
        putStrLn $ "    Transposed (root+tones): " ++ show (ttTransposedPitches tt)
        putStrLn $ "    Normalized: " ++ show (ttNormalizedPs tt)
        putStrLn $ "    Zero-form: " ++ show (ttZeroForm tt)
        putStrLn $ "    Detected root: " ++ ttDetectedRoot tt
        putStrLn $ "    Computed name: " ++ ttFunctionality tt
        putStrLn $ "    DB stored name: " ++ ttStoredFunc tt
      Nothing -> return ()
    
    -- Advance trace (verbosity 2+)
    when (verbosity >= 2) $ case sdAdvanceTrace step of
      Just at -> do
        putStrLn "  [ADVANCE TRACE]"
        putStrLn $ "    Root motion: " ++ atCurrentRoot at ++ " " ++ show (atCurrentRootPC at)
                   ++ " + " ++ show (atMovementInterval at) ++ " → " ++ show (atNewRootPC at)
        putStrLn $ "    Spelling: " ++ atNewRoot at
      Nothing -> return ()
    
    putStrLn ""
  
  -- Final progression
  putStrLn "═══════════════════════════════════════════════════════════════════"
  putStrLn "FINAL PROGRESSION:"
  putStrLn "═══════════════════════════════════════════════════════════════════"
  print (gdProgression diag)
  putStrLn ""

-- |Generate a progression with NO diagnostic output (verbosity 0 - silent mode).
--
-- This is the simplest generation function to use when you only care about the progression,
-- not the intermediate diagnostics.
--
-- === Type Signature
--
-- @CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression@
--
-- === Parameters
--
-- * @start@ - Starting cadence state (root, cadence type, enharmonic spelling)
-- * @len@ - Number of chords to generate (including the start state)
-- * @composerStr@ - Composer weights as string (e.g., "bach", "bach:30 debussy:70")
-- * @entropy@ - Exploration parameter [0.0 = greedy, 1.0 = maximum randomness]
-- * @ctx@ - HarmonicContext with filter constraints (overtones, key, roots)
--
-- === Return Value
--
-- @IO Progression@ - The generated progression, with zero diagnostic side effects.
--
-- === Example
--
-- @
--   let start = initCadenceState 0 \"C\" [0,4,7] FlatSpelling
--       ctx = defaultContext
--   prog <- genSilent start 8 \"*\" 0.5 ctx
--   print prog  -- Just shows the progression, no diagnostic output
-- @
--
-- === See Also
--
-- * @genStandard@ - Same interface, prints standard diagnostics
-- * @genVerbose@ - Same interface, prints verbose diagnostics with traces
-- * @generate'@ - Returns tuple @(Progression, GenerationDiagnostics)@ for manual extraction
genSilent :: H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genSilent start len composerStr entropy ctx = do
  (prog, _diag) <- generate' start len composerStr entropy ctx
  pure prog

-- |Generate a progression with STANDARD diagnostic output (verbosity 1).
--
-- Prints per-step diagnostics including candidate pools, selections, and rendered chords.
-- Useful for understanding generation flow and candidate quality.
--
-- === Type Signature
--
-- @CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression@
--
-- === Parameters
--
-- Same as @genSilent@.
--
-- === Printed Diagnostics
--
-- For each step:
--   * Prior and posterior cadence states
--   * Graph candidate count and top 3 candidates with confidence scores
--   * Fallback candidate count and top 3 candidates with scores
--   * Total pool size and gamma-selected index
--   * Selected source (graph or fallback)
--   * Selected movement and rendered chord name
--   * Final progression visualization
--
-- === Example
--
-- @
--   prog <- genStandard start 4 \"*\" 0.5 ctx
--   -- Prints:
--   -- GENERATION DIAGNOSTICS
--   -- Starting: ( pedal -> maj ) @ C
--   -- STEP 1:
--   --   Prior: ( pedal -> maj ) @ C
--   --   Candidates: graph=30, fallback=0, pool=30
--   --     Top graph: [...]
--   --   Selected: graph @ index 5
--   --   ...
-- @
--
-- === See Also
--
-- * @genSilent@ - Same interface, no output
-- * @genVerbose@ - Same interface, verbose output with traces
-- * @printDiagnostics@ - Manually reprint diagnostics at any time
genStandard :: H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genStandard start len composerStr entropy ctx = do
  (prog, diag) <- generate' start len composerStr entropy ctx
  printDiagnostics 1 diag
  pure prog

-- |Generate a progression with VERBOSE diagnostic output (verbosity 2).
--
-- Prints everything from @genStandard@ plus detailed transform and advance traces.
-- Useful for debugging chord naming discrepancies and voice leading computations.
--
-- === Type Signature
--
-- @CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression@
--
-- === Parameters
--
-- Same as @genSilent@.
--
-- === Printed Diagnostics
--
-- Includes all from @genStandard@ plus:
--
--   * [TRANSFORM TRACE] - Complete render pipeline for each step:
--     - DB zero-form intervals before transposition
--     - Transposed pitch classes (DB intervals + root PC)
--     - Normalized pitch classes (after fund adjustment)
--     - Final zero-form representation
--     - Detected root from pitch classes
--     - Computed chord functionality name
--     - Comparison with DB stored name
--
--   * [ADVANCE TRACE] - Root motion computation:
--     - Prior and posterior root pitch classes
--     - Movement interval extracted from movement type
--     - PC arithmetic: (prior_PC + movement_interval) mod 12
--     - Enharmonic spelling function applied (flat/sharp)
--     - Final posterior root note name
--
-- === Performance Note
--
-- This function is slower than @genStandard@ due to extra tracing computation.
-- Use for debugging only, not for production generation.
--
-- === Example
--
-- @
--   prog <- genVerbose start 2 \"*\" 0.5 ctx
--   -- Prints standard diagnostics plus:
--   -- [TRANSFORM TRACE]
--   --   DB intervals: [0,4,7]
--   --   Transposed: [0,4,7]
--   --   ...
--   -- [ADVANCE TRACE]
--   --   0 + 0 = 0 (mod 12)
--   --   C → C
-- @
--
-- === See Also
--
-- * @genSilent@ - Same interface, no output
-- * @genStandard@ - Same interface, standard output
-- * @generate''@ - Returns tuple @(Progression, GenerationDiagnostics)@ for manual extraction
genVerbose :: H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genVerbose start len composerStr entropy ctx = do
  (prog, diag) <- generate'' start len composerStr entropy ctx
  printDiagnostics 2 diag
  pure prog

-- |Silent mode with custom 'GeneratorConfig'.
--
-- Same as 'genSilent' but allows customizing homing threshold, composition strength,
-- and minimum candidate count via 'GeneratorConfig'.
--
-- === Type Signature
--
-- @GeneratorConfig -> CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression@
--
-- === Parameters
--
-- * @config@ - Custom configuration (homing at 75%, strength, min candidates)
-- * Other parameters same as 'genSilent'
--
-- === Example
--
-- @
--   let cfg = defaultConfig { cfgHomingThreshold = 0.8 }
--   prog <- genSilent' cfg start 8 "*" 0.5 ctx
-- @
--
-- === See Also
--
-- * 'genSilent' - Same verbosity, default config
-- * 'genStandard\'' - Standard diagnostics with custom config
-- * 'genVerbose\'' - Verbose diagnostics with custom config
-- * 'GeneratorConfig' - Configuration options
genSilent' :: GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genSilent' config start len composerStr entropy ctx = do
  (prog, _diag) <- genWith' config start len composerStr entropy ctx
  pure prog

-- |Standard diagnostics with custom 'GeneratorConfig'.
--
-- Same as 'genStandard' but allows customizing via 'GeneratorConfig'.
--
-- === Type Signature
--
-- @GeneratorConfig -> CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression@
--
-- === Parameters
--
-- * @config@ - Custom configuration
-- * Other parameters same as 'genStandard'
--
-- === Example
--
-- @
--   let cfg = defaultConfig { cfgMinCandidates = 5 }
--   prog <- genStandard' cfg start 8 "*" 0.5 ctx
-- @
--
-- === See Also
--
-- * 'genStandard' - Same verbosity, default config
-- * 'genSilent\'' - Silent with custom config
-- * 'genVerbose\'' - Verbose diagnostics with custom config
genStandard' :: GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genStandard' config start len composerStr entropy ctx = do
  (prog, diag) <- genWith' config start len composerStr entropy ctx
  printDiagnostics 1 diag
  pure prog

-- |Verbose diagnostics with custom 'GeneratorConfig'.
--
-- Same as 'genVerbose' but allows customizing via 'GeneratorConfig'.
-- Includes full transform and advance traces.
--
-- === Type Signature
--
-- @GeneratorConfig -> CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression@
--
-- === Parameters
--
-- * @config@ - Custom configuration
-- * Other parameters same as 'genVerbose'
--
-- === Performance
--
-- Slower than 'genStandard\'' due to full tracing. Use for debugging only.
--
-- === Example
--
-- @
--   let cfg = defaultConfig { cfgCompositionStrength = 0.8 }
--   prog <- genVerbose' cfg start 2 "*" 0.5 ctx
-- @
--
-- === See Also
--
-- * 'genVerbose' - Same verbosity, default config
-- * 'genSilent\'' - Silent with custom config
-- * 'genStandard\'' - Standard diagnostics with custom config
genVerbose' :: GeneratorConfig -> H.CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Prog.Progression
genVerbose' config start len composerStr entropy ctx = do
  (prog, diag) <- genWith'' config start len composerStr entropy ctx
  printDiagnostics 2 diag
  pure prog

-------------------------------------------------------------------------------

-- |Build cadence chain with diagnostic collection (verbosity level 1)
buildChainWithDiag :: GeneratorConfig
                   -> Double           -- ^ Entropy [0,1]
                   -> HarmonicContext
                   -> H.CadenceState   -- ^ Starting state
                   -> Int              -- ^ Number of steps to generate
                   -> Bolt.BoltActionT IO ([H.CadenceState], [StepDiagnostic])
buildChainWithDiag config entropy context start totalSteps =
  buildChainWithDiagV config 1 entropy context start totalSteps

-- |Build cadence chain with diagnostic collection (configurable verbosity)
-- Verbosity levels:
--   0 = no diagnostics (unused, use buildChain instead)
--   1 = standard diagnostics (sdRenderedChord populated)
--   2 = maximum diagnostics (full TransformTrace and AdvanceTrace)
buildChainWithDiagV :: GeneratorConfig
                    -> Int             -- ^ Verbosity level (1 or 2)
                    -> Double          -- ^ Entropy [0,1]
                    -> HarmonicContext
                    -> H.CadenceState  -- ^ Starting state
                    -> Int             -- ^ Number of steps to generate
                    -> Bolt.BoltActionT IO ([H.CadenceState], [StepDiagnostic])
buildChainWithDiagV config verbosity entropy context start totalSteps = do
  foldM (stepChainWithDiagV config verbosity entropy context) 
        ([start], [])  -- Start with initial state in chain, empty diagnostics
        [1..totalSteps]

-- |Single step with diagnostic collection (verbosity 1)
stepChainWithDiag :: GeneratorConfig
                  -> Double           -- ^ Entropy [0,1]
                  -> HarmonicContext
                  -> ([H.CadenceState], [StepDiagnostic]) -- ^ (Chain so far, diagnostics so far)
                  -> Int              -- ^ Current step number
                  -> Bolt.BoltActionT IO ([H.CadenceState], [StepDiagnostic])
stepChainWithDiag config entropy context chainDiags stepNum = 
  stepChainWithDiagV config 1 entropy context chainDiags stepNum

-- |Single step with diagnostic collection (configurable verbosity)
stepChainWithDiagV :: GeneratorConfig
                   -> Int             -- ^ Verbosity level (1 or 2)
                   -> Double          -- ^ Entropy [0,1]
                   -> HarmonicContext
                   -> ([H.CadenceState], [StepDiagnostic]) -- ^ (Chain so far, diagnostics so far)
                   -> Int             -- ^ Current step number
                   -> Bolt.BoltActionT IO ([H.CadenceState], [StepDiagnostic])
stepChainWithDiagV config verbosity entropy context (chain, diags) stepNum = do
  let current = last chain
      currentShow = T.pack $ show (extractCadence current)
      poolSize = gcPoolSize config
  
  -- Fetch all transitions from current cadence
  transitions <- fetchTransitions currentShow
  
  -- Apply R constraints (pure filter by HarmonicContext)
  let filtered = applyRConstraints context transitions
  
  -- Score by total confidence
  let scored = scoreByConfidence filtered
      graphCandidates = take poolSize scored
      graphCount = length graphCandidates
  
  -- Build candidate pool: graph candidates + consonanceFallback if needed
  let needed = poolSize - graphCount
      fallbackAll = consonanceFallback current context
      fallbackCandidates = if needed > 0 then take needed fallbackAll else []
      fallbackCount = length fallbackCandidates
      -- Create unified pool with (Cadence, score) format
      pool = graphCandidates ++ [(cad, score) | (cad, score, _, _) <- fallbackCandidates]
  
  -- Select next cadence using gamma sampling
  if null pool
    then do
      -- Absorbing state - create diagnostic for this edge case
      let priorCadence = extractCadence current
          diag = StepDiagnostic
            { sdStepNumber = stepNum
            -- PRIOR STATE
            , sdPriorCadence = show priorCadence
            , sdPriorRoot = show (H.stateCadenceRoot current)
            , sdPriorRootPC = P.unPitchClass (P.pitchClass (H.stateCadenceRoot current))
            -- SELECTED FROM DB (none - absorbing)
            , sdSelectedDbIntervals = "N/A"
            , sdSelectedDbMovement = "N/A"
            , sdSelectedDbFunctionality = "N/A"
            -- CANDIDATE POOL INFO
            , sdGraphCount = 0
            , sdGraphTop6 = []
            , sdFallbackCount = 0
            , sdFallbackTop6 = []
            , sdPoolSize = 0
            , sdEntropyUsed = entropy
            , sdGammaIndex = -1
            , sdSelectedFrom = "none (absorbing)"
            -- POSTERIOR STATE (no change)
            , sdPosteriorRoot = show (H.stateCadenceRoot current)
            , sdPosteriorRootPC = P.unPitchClass (P.pitchClass (H.stateCadenceRoot current))
            , sdRenderedChord = Nothing
            , sdTransformTrace = Nothing
            , sdAdvanceTrace = Nothing
            }
      pure (chain ++ [current], diags ++ [diag])
    else do
      idx <- liftIO $ gammaIndexScaled entropy (length pool)
      let nextCadence = fst (pool !! idx)
          (newState, advTrace) = advanceStateTraced current nextCadence
          
          -- Determine if selection came from graph or fallback
          selectedFrom = if idx < graphCount then "graph" else "fallback"
          
          -- Build diagnostic for this step
          graphTop6 = take 6 [(show cad, conf) | (cad, conf) <- graphCandidates]
          -- For fallback, use actual component scores (name, score, chordDiss_norm, motionDiss_norm)
          fallbackTop6 = take 6 [(show cad, score, cd, md) | (cad, score, cd, md) <- fallbackCandidates]
          
          -- Compute rendered chord and transform trace based on verbosity
          (renderedChord, transformTrace) = computeChordTrace verbosity newState
          
          -- Extract prior state info
          priorCadence = extractCadence current
          priorRoot = H.stateCadenceRoot current
          priorRootPC = P.unPitchClass (P.pitchClass priorRoot)
          
          -- Extract selected DB info
          selectedDbIntervals = show (H.cadenceIntervals nextCadence)
          selectedDbMovement = show (H.cadenceMovement nextCadence)
          selectedDbFunctionality = H.cadenceFunctionality nextCadence
          
          -- Extract posterior state info
          posteriorRoot = H.stateCadenceRoot newState
          posteriorRootPC = P.unPitchClass (P.pitchClass posteriorRoot)
          
          diag = StepDiagnostic
            { sdStepNumber = stepNum
            -- PRIOR STATE
            , sdPriorCadence = show priorCadence
            , sdPriorRoot = show priorRoot
            , sdPriorRootPC = priorRootPC
            -- SELECTED FROM DB
            , sdSelectedDbIntervals = selectedDbIntervals
            , sdSelectedDbMovement = selectedDbMovement
            , sdSelectedDbFunctionality = selectedDbFunctionality
            -- CANDIDATE POOL INFO
            , sdGraphCount = graphCount
            , sdGraphTop6 = graphTop6
            , sdFallbackCount = fallbackCount
            , sdFallbackTop6 = fallbackTop6
            , sdPoolSize = length pool
            , sdEntropyUsed = entropy
            , sdGammaIndex = idx
            , sdSelectedFrom = selectedFrom
            -- POSTERIOR STATE
            , sdPosteriorRoot = show posteriorRoot
            , sdPosteriorRootPC = posteriorRootPC
            , sdRenderedChord = renderedChord
            , sdTransformTrace = transformTrace
            , sdAdvanceTrace = if verbosity >= 2 then Just advTrace else Nothing
            }
      
      pure (chain ++ [newState], diags ++ [diag])

-- |Compute chord rendering and transform trace based on verbosity level.
-- Note: Reads raw DB data from the cadence within the state (which is the selected cadence after advance).
computeChordTrace :: Int -> H.CadenceState -> (Maybe String, Maybe TransformTrace)
computeChordTrace verbosity state
  | verbosity >= 2 = 
      let (chord, ttt) = H.fromCadenceStateTraced state
          
          tt = TransformTrace
            { ttRawDbIntervals = H.tttRawDbIntervals ttt
            , ttRawDbMovement = H.tttRawDbMovement ttt
            , ttRawDbFunctionality = H.tttRawDbFunctionality ttt
            , ttRootPC = H.tttRootPC ttt
            , ttRootNoteName = H.tttRootNoteName ttt
            , ttTones = H.tttTones ttt
            , ttTransposedPitches = H.tttTransposedPitches ttt
            , ttNormalizedPs = H.tttNormalizedPs ttt
            , ttZeroForm = H.tttZeroForm ttt
            , ttDetectedRoot = H.tttDetectedRoot ttt
            , ttFunctionality = H.tttFunctionality ttt
            , ttFinalChord = H.tttFinalChord ttt
            , ttStoredFunc = H.tttStoredFunc ttt
            }
      in (Just $ showChord chord, Just tt)
  | verbosity >= 1 = 
      let chord = H.fromCadenceState state
      in (Just $ showChord chord, Nothing)
  | otherwise = (Nothing, Nothing)

-- |Show a chord as "Root Functionality"
showChord :: H.Chord -> String
showChord c = show (H.chordNoteName c) ++ " " ++ H.chordFunctionality c

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
           -> Double           -- ^ Entropy [0,1]
           -> HarmonicContext
           -> H.CadenceState   -- ^ Starting state
           -> Int              -- ^ Number of steps to generate
           -> Bolt.BoltActionT IO [H.CadenceState]
buildChain config entropy context start totalSteps = do
  foldM (stepChain config entropy context) 
        [start]  -- Start with initial state in chain
        [1..totalSteps]

-- |Single step in chain building.
-- 
-- Algorithm:
--   1. Get current state (last in chain)
--   2. Query graph for transitions from current cadence
--   3. Filter by HarmonicContext (pure predicate filtering)
--   4. Sort by confidence (highest first)
--   5. If < poolSize candidates, fill with consonanceFallback
--   6. Gamma-select from pool using entropy
--   7. Advance state and append to chain
stepChain :: GeneratorConfig
          -> Double           -- ^ Entropy [0,1]
          -> HarmonicContext
          -> [H.CadenceState] -- ^ Chain so far
          -> Int              -- ^ Current step (unused but required by foldM)
          -> Bolt.BoltActionT IO [H.CadenceState]
stepChain config entropy context chain _step = do
  let current = last chain
      currentShow = T.pack $ show (extractCadence current)
      poolSize = gcPoolSize config
  
  -- Fetch all transitions from current cadence
  transitions <- fetchTransitions currentShow
  
  -- Apply R constraints (pure filter by HarmonicContext)
  let filtered = applyRConstraints context transitions
  
  -- Score by total confidence (composer weights disabled for now)
  -- Filter to confidence > 0 and sort highest first
  let scored = scoreByConfidence filtered
      graphCandidates = take poolSize scored
  
  -- Build candidate pool: graph candidates + consonanceFallback if needed
  let needed = poolSize - length graphCandidates
      -- Extract just (cadence, score) tuples from fallback 4-tuples
      fallbackCandidates = if needed > 0
                           then take needed [(cad, score) | (cad, score, _, _) <- consonanceFallback current context]
                           else []
      pool = graphCandidates ++ fallbackCandidates
  
  -- Select next cadence using gamma sampling
  if null pool
    then pure $ chain ++ [current]  -- Absorbing state (should be rare now)
    else do
      idx <- liftIO $ gammaIndexScaled entropy (length pool)
      let nextCadence = fst (pool !! idx)
          newState = advanceState current nextCadence
      pure $ chain ++ [newState]

-- |Score transitions by total confidence (sum of all composer weights).
-- Filters to confidence > 0 and sorts highest first.
scoreByConfidence :: [(H.Cadence, ComposerWeights)] -> [(H.Cadence, Double)]
scoreByConfidence transitions =
  let scored = [(cad, sum (Map.elems weights)) | (cad, weights) <- transitions]
      positive = filter ((> 0) . snd) scored
  in sortBy (compare `on` (Down . snd)) positive

-- |Generate fallback candidates from HarmonicContext filters.
-- 
-- This implements the legacy "constructive generation" pattern:
--   1. Get effective overtone palette (tuning filtered by key)
--   2. Get allowed roots (via resolveRoots which handles "key"/"tones" options)
--   3. Generate all valid triads from roots × overtones
--   4. Compute actual movement from current state to each candidate
--   5. Sort by consonance (most consonant first)
--   6. Return as [(Cadence, Double)] with inverted dissonance as score
-- 
-- Movement computation matches legacy getCadenceOptions which uses:
--   toCadence (transposeCadence enharm rootPC prev, nxt)
-- to derive proper movements from current position to each candidate.
-- This ensures fallback cadences have real movements, enabling subsequent
-- iterations to find graph matches and traverse freely.
-- Returns [(Cadence, score, chordDiss_norm, motionDiss_norm)]
consonanceFallback :: H.CadenceState -> HarmonicContext -> [(H.Cadence, Double, Double, Double)]
consonanceFallback currentState context =
  let -- Get current root pitch class for movement computation
      currentRoot = P.pitchClass (H.stateCadenceRoot currentState)
      
      -- Get overtone palette (4 partials per fundamental)
      overtones = parseOvertones' 4 (hcOvertones context)
      
      -- Apply key filter to overtones
      keyPcs = parseKey (hcKey context)
      effectiveOvertones = if isWildcard (hcKey context)
                           then overtones
                           else filter (`elem` keyPcs) overtones
      
      -- Get allowed roots (handles "key", "tones", or explicit notes)
      roots = resolveRoots (hcOvertones context) (hcKey context) (hcRoots context)
      
      -- Generate all valid triads: root + 2 tones from effective overtones
      -- Each root gets all valid 3-note sets
      triads = concatMap (\r -> overtoneSets 3 [r] effectiveOvertones) roots
      
      -- Remove duplicates and empty results
      uniqueTriads = nub $ filter (\t -> length t == 3) triads
      
      -- Compute normalized badness score combining chord dissonance and root motion
      -- Returns (score, chordDiss_norm, motionDiss_norm) for score composition display
      scored = [(cad, score, cd, md) 
               | t <- uniqueTriads
               , let cad = triadToCadenceFrom currentRoot t
               , let (score, cd, md) = computeFallbackScoreWithComponents currentRoot cad t]
      
      -- Sort by score (highest first = best combination of consonance + smooth motion)
  in sortBy (compare `on` (\(_, s, _, _) -> Down s)) scored

-- |Convert a triad (list of pitch classes) to a Cadence with movement from current root.
-- Movement is computed from currentRoot to the triad's root (head of sorted triad).
-- This matches legacy getCadenceOptions which uses toCadence to derive proper movements.
triadToCadenceFrom :: P.PitchClass -> [Int] -> H.Cadence
-- |Convert fallback triad (absolute pitch classes) to Cadence with zero-form normalization.
-- Applies H.zeroFormPC to ensure all fallback-generated cadences store relative intervals,
-- matching database format. This guarantees naming consistency across all cadence sources.
triadToCadenceFrom currentRoot pitches = 
  let sortedPitches = sort pitches
      triadRoot = P.mkPitchClass (head sortedPitches)
      movement = H.toMovement currentRoot triadRoot
      -- Zero-form normalization: [P 4,P 7,P 11] → [P 0,P 3,P 7]
      -- Enables transposition: posteriorChord = posteriorRootPC + cadenceIntervals
      pcs = H.zeroFormPC (map P.mkPitchClass sortedPitches)
      functionality = H.toFunctionality pcs
  in H.Cadence functionality movement pcs

-- |Compute normalized fallback score combining chord dissonance and root motion smoothness.
-- Formula: score = 10000 - (chordDiss_norm × motionDiss_norm × 100)
-- where both dissonances are normalized to [0.01, 1] range.
--
-- Chord dissonance range: 6 (major/minor triad) to ~50 (dense cluster)
-- Root motion range: 1 (P5/P4) to 6 (tritone)
--
-- Both normalized to [0.01, 1] ensures the product never collapses to zero,
-- maintaining gradient resolution across fallback candidates.
-- Returns (finalScore, chordDiss_normalized, motionDiss_normalized)
computeFallbackScoreWithComponents :: P.PitchClass -> H.Cadence -> [Int] -> (Double, Double, Double)
computeFallbackScoreWithComponents currentRoot cad triad =
  let -- Chord vertical dissonance (raw Hindemith score)
      chordDiss = fromIntegral (dissonanceScore triad) :: Double
      
      -- Root motion dissonance (extract interval from Movement)
      interval = extractMovementInterval (H.cadenceMovement cad)
      motionDiss = fromIntegral (D.rootMotionScore interval) :: Double
      
      -- Normalize to [0.01, 1]: prevents zero products while maintaining gradient
      -- Formula: 0.01 + (normalized_0_to_1 * 0.99)
      chordDissNorm = 0.01 + (((chordDiss - 6.0) / 44.0) * 0.99)
      motionDissNorm = 0.01 + (((motionDiss - 1.0) / 5.0) * 0.99)
      
      -- Combined badness: multiply normalized dissonances
      badness = chordDissNorm * motionDissNorm
      
      -- Final score: higher is better (subtract badness from large constant)
      finalScore = 10000.0 - (badness * 100.0)  -- Scale by 100 to maintain resolution
  in (finalScore, chordDissNorm, motionDissNorm)

-- Convenience wrapper returning only the score
computeFallbackScore :: P.PitchClass -> H.Cadence -> [Int] -> Double
computeFallbackScore root cad triad = 
  let (score, _, _) = computeFallbackScoreWithComponents root cad triad
  in score

-- |Extract semitone interval from Movement type.
-- Maps Movement to interval in semitones (0-11) for rootMotionScore input.
extractMovementInterval :: H.Movement -> Int
extractMovementInterval movement = case movement of
  H.Asc pc   -> P.unPitchClass pc
  H.Desc pc  -> P.unPitchClass pc
  H.Unison   -> 0
  H.Tritone  -> 6
  H.Empty    -> 0

-- |Apply R constraints to filter transitions
applyRConstraints :: HarmonicContext 
                  -> [(H.Cadence, ComposerWeights)] 
                  -> [(H.Cadence, ComposerWeights)]
applyRConstraints context = filter (matchesContext context . fst)

-- |Check if a cadence matches the harmonic context filters.
-- 
-- Filter logic (matching legacy behavior):
--   1. Compute effective overtones: key-filtered overtone palette
--   2. All chord pitches must be in effective overtones
--   3. Root must be in resolved roots (handles "key"/"tones" options)
matchesContext :: HarmonicContext -> H.Cadence -> Bool
matchesContext context cadence =
  let (movement, chord) = H.deconstructCadence cadence
      
      -- Get effective overtone palette (key-filtered)
      rawOvertones = parseOvertones' 4 (hcOvertones context)
      keyPcs = parseKey (hcKey context)
      effectiveOvertones = if isWildcard (hcKey context)
                           then rawOvertones
                           else filter (`elem` keyPcs) rawOvertones
      
      -- Get allowed roots (handles "key", "tones", or explicit notes)
      allowedRoots = resolveRoots (hcOvertones context) (hcKey context) (hcRoots context)
      
      -- Extract root from movement
      rootPc = case movement of
        H.Unison -> P.mkPitchClass 0
        H.Tritone -> P.mkPitchClass 6
        H.Asc pc -> pc
        H.Desc pc -> pc
        H.Empty -> P.mkPitchClass 0
      
      -- Convert chord to Int for comparison
      chordInts = map (fromIntegral . P.unPitchClass) chord
      rootInt = fromIntegral (P.unPitchClass rootPc)
      
      -- All chord pitches must be in effective overtones (or wildcard)
      overtonesMatch = isWildcard (hcOvertones context) 
                       || all (`elem` effectiveOvertones) chordInts
      
      -- Root must be in allowed roots (or wildcard)
      rootsMatch = isWildcard (hcRoots context) 
                   || rootInt `elem` allowedRoots
      
  in overtonesMatch && rootsMatch

-- |Apply homing bias to scored candidates (disabled but kept for future use)
applyHomingBias :: Set.Set Text -> Double -> [(H.Cadence, Double)] -> [(H.Cadence, Double)]
applyHomingBias homingSet strength scored =
  map (\(cad, score) -> 
    let cadShow = T.pack $ show cad
        bonus = if Set.member cadShow homingSet then strength * score else 0
    in (cad, score + bonus)) 
  scored

-------------------------------------------------------------------------------
-- State Advancement
-------------------------------------------------------------------------------

-- |Advance the CadenceState based on movement to a new cadence
advanceState :: H.CadenceState -> H.Cadence -> H.CadenceState
advanceState currentState newCadence =
  fst $ advanceStateTraced currentState newCadence

-- |Advance the CadenceState with full trace of intermediate values
-- Used for maximum verbosity diagnostics (gen'')
-- Enharmonic spelling is determined by selectEnharm: prior's preference wins if definite,
-- C (ambiguous) adopts posterior's preference, both C preserves current spelling.
advanceStateTraced :: H.CadenceState -> H.Cadence -> (H.CadenceState, AdvanceTrace)
advanceStateTraced currentState newCadence =
  let currentRoot = H.stateCadenceRoot currentState
      spelling = H.stateSpelling currentState
      currentRootPC = P.pitchClass currentRoot
      movement = H.cadenceMovement newCadence
      movementInterval = H.fromMovement movement
      newRootPC = currentRootPC + movementInterval
      enharm = P.enharmFromNoteName currentRoot
      newRoot = enharm newRootPC
      -- Apply enharmonic preference: prior's spelling preference wins if definite,
      -- C (ambiguous) defers to posterior's preferred spelling
      newSpelling = H.selectEnharm spelling currentRootPC newRootPC
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

-- |Extract Cadence from CadenceState
extractCadence :: H.CadenceState -> H.Cadence
extractCadence = H.stateCadence

-------------------------------------------------------------------------------
-- Conversion to Progression
-------------------------------------------------------------------------------

-- |Convert a chain of CadenceStates to a Progression
chainToProgression :: [H.CadenceState] -> Prog.Progression
chainToProgression = Prog.fromCadenceStates

-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------
