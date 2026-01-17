-- |
-- Module      : Harmonic.Lib
-- Description : Re-export convenience module for Phase B + Phase C types
--
-- = Overview
--
-- This module provides a unified interface for harmonic generation via three functions:
--
-- * 'genSilent' - No diagnostic output (use for production)
-- * 'genStandard' - Standard diagnostics (use for exploration)
-- * 'genVerbose' - Verbose diagnostics with traces (use for debugging)
--
-- All three have /identical type signatures/ and return 'IO Progression',
-- enabling seamless switching between verbosity levels without code changes.
--
-- = Usage Example
--
-- @
-- import Harmonic.Lib
--
-- let start = initCadenceState 0 "C" [0,4,7] FlatSpelling
--     ctx = defaultContext
--
-- -- Silent: Just get the progression
-- prog1 <- genSilent start 8 "*" 0.5 ctx
-- 
-- -- Standard: See per-step candidates and selections
-- prog2 <- genStandard start 8 "*" 0.5 ctx
-- 
-- -- Verbose: Debug chord naming and voice leading
-- prog3 <- genVerbose start 8 "*" 0.5 ctx
-- @
--
-- = Unified Generation Interface
--
-- The three public functions share identical signatures:
--
-- > genSilent    :: CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression
-- > genStandard  :: CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression
-- > genVerbose   :: CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression
--
-- [Silent mode (verbosity 0)]
--   No output. Return type is 'IO Progression'.
--   Use when you only care about the final result.
--
-- [Standard mode (verbosity 1)]
--   Prints per-step candidate pools, selections, and rendered chords.
--   Return type is 'IO Progression'.
--
-- [Verbose mode (verbosity 2)]
--   Prints everything from standard + detailed transform and advance traces.
--   Return type is 'IO Progression'.
--   Slower; use only for debugging.
--
-- = Behind the Scenes
--
-- Internally, all three call:
--
-- * 'generate\'' - Returns @(Progression, GenerationDiagnostics)@ tuple with standard traces
--
-- Then 'printDiagnostics' extracts and formats the diagnostics based on verbosity level.
--
-- = For GHCi Exploration
--
-- Import this module to get all Phase B types and Phase C generation functions.
-- The module re-exports core Haskell modules and TidalCycles bridge functions.

module Harmonic.Lib (
  -- ========== PRIMARY INTERFACE FOR LIVE CODING ==========
  -- Three functions with clean signatures for TidalCycles:
  --   gen   - silent (no output)
  --   gen'  - compact summary
  --   gen'' - verbose traces
  gen, gen', gen'',
  
  -- ========== ALTERNATIVE UNIFIED INTERFACE ==========
  -- Same as above but with descriptive names
  genSilent, genStandard, genVerbose,
  
  -- ========== CONTEXT & CONFIGURATION ==========
  HarmonicContext(..), harmonicContext, hContext, defaultContext,
  GeneratorConfig(..), defaultConfig,
  
  -- ========== PHASE B: CORE MUSIC TYPES ==========
  module Harmonic.Rules.Types.Pitch,
  module Harmonic.Rules.Types.Harmony,
  module Harmonic.Evaluation.Scoring.Dissonance,
  module Harmonic.Rules.Constraints.Overtone,

  -- VoiceLeading (cyclic DP paradigms)
  voiceLeadingCost, totalCost, cyclicCost,
  voiceMovement, minimalMovement,
  allVoicings, initialCompact,
  solveRoot, solveFlow,

  module Harmonic.Rules.Types.Progression,

  -- ========== PHASE C: INTERACTIVE BEHAVIOUR ==========
  module Harmonic.Traversal.Probabilistic,
  
  -- ========== FILTER FUNCTIONS ==========
  -- String-friendly versions for TidalCycles
  overtones, Harmonic.Rules.Constraints.Filter.key, funds, tuning, wildcard,
  -- Text versions
  parseOvertones, parseKey, parseFunds, parseTuning, isWildcard,
  
  -- ========== DATABASE INTERFACE ==========
  module Harmonic.Evaluation.Database.Query,
  connectNeo4j,

  -- ========== INGESTION PIPELINE ==========
  module Harmonic.Rules.Import.CSV,
  module Harmonic.Rules.Import.Transform,
  
  -- ========== TIDAL INTERFACE ==========
  -- Pattern-level operations
  VoiceFunction, rootNotes, bassNotes,
  arrange, applyProg, voiceRange,
  lookupProgression, lookupChord, VoiceType(..), voiceBy, harmony,
  overlapF,
  
  -- Arranger functions (voicing paradigms)
  rotate, excerpt, insert, switch, clone, extract,
  transposeP, Harmonic.Interface.Tidal.Arranger.reverse, fuse, fuse2, interleave, expandP,
  progOverlap, progOverlapF, progOverlapB,
  root, flow, lite, literal,

  -- Explicit progression construction
  fromChords, prog, fromChordsFlat, fromChordsSharp,

  -- Scale source (switch mechanism)
  ScaleSource(..), melodyStateFrom,

  module Harmonic.Interface.Tidal.Instruments,
  module Harmonic.Interface.Tidal.Utils,
  module Harmonic.Config,
  
  -- ========== INTERNAL FUNCTIONS (advanced use only) ==========
  -- Tuple-returning versions for manual diagnostics extraction
  generate, generateWith, genWith,
  generate', genWith',
  generate'', genWith'',
  printDiagnostics,
  StepDiagnostic(..), GenerationDiagnostics(..),
  TransformTrace(..), AdvanceTrace(..),
  genSilent', genStandard', genVerbose'
) where

-- Phase B: Core Music Types
import Harmonic.Rules.Types.Pitch
import Harmonic.Rules.Types.Harmony
import Harmonic.Evaluation.Scoring.Dissonance
import Harmonic.Rules.Constraints.Overtone
import Harmonic.Evaluation.Scoring.VoiceLeading (voiceLeadingCost, totalCost, cyclicCost, voiceMovement, minimalMovement, allVoicings, initialCompact, solveRoot, solveFlow)
import Harmonic.Rules.Types.Progression
-- Phase C: Interactive Behaviour
import Harmonic.Traversal.Probabilistic
import Harmonic.Framework.Builder (HarmonicContext(..), harmonicContext, hContext, defaultContext, GeneratorConfig(..), defaultConfig, generate, generateWith, gen, genWith, generate', gen', genWith', generate'', gen'', genWith'', genSilent, genStandard, genVerbose, genSilent', genStandard', genVerbose', printDiagnostics, StepDiagnostic(..), GenerationDiagnostics(..), TransformTrace(..), AdvanceTrace(..))
import Harmonic.Rules.Constraints.Filter (overtones, key, funds, tuning, wildcard, parseOvertones, parseKey, parseFunds, parseTuning, isWildcard)
import Harmonic.Evaluation.Database.Query
-- Infrastructure (selective imports to avoid conflicts)
import Harmonic.Rules.Import.Graph (connectNeo4j)
import Harmonic.Rules.Import.CSV
import Harmonic.Rules.Import.Transform
import Harmonic.Interface.Tidal.Bridge (
    VoiceFunction, rootNotes, bassNotes,
    arrange, applyProg, voiceRange,
    lookupProgression, lookupChord, VoiceType(..), voiceBy, harmony,
    overlapF
  )
import Harmonic.Interface.Tidal.Arranger (
    rotate, excerpt, insert, switch, clone, extract,
    transposeP, reverse, fuse, fuse2, interleave, expandP,
    progOverlap, progOverlapF, progOverlapB,
    root, flow, lite, literal,
    fromChords, prog, fromChordsFlat, fromChordsSharp,
    ScaleSource(..), melodyStateFrom
  )
import Harmonic.Interface.Tidal.Instruments
import Harmonic.Interface.Tidal.Utils
import Harmonic.Config
