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
  module Harmonic.Core.Pitch,
  module Harmonic.Core.Harmony,
  module Harmonic.Core.Dissonance,
  module Harmonic.Core.Overtone,
  
  -- VoiceLeading (cyclic DP paradigms)
  voiceLeadingCost, totalCost, cyclicCost,
  voiceMovement, minimalMovement,
  allVoicings, initialCompact,
  solveRoot, solveFlow,
  
  module Harmonic.Core.Progression,
  
  -- ========== PHASE C: INTERACTIVE BEHAVIOUR ==========
  module Harmonic.Core.Probabilistic,
  
  -- ========== FILTER FUNCTIONS ==========
  -- String-friendly versions for TidalCycles
  overtones, Harmonic.Core.Filter.key, funds, tuning, wildcard,
  -- Text versions
  parseOvertones, parseKey, parseFunds, parseTuning, isWildcard,
  
  -- ========== DATABASE INTERFACE ==========
  module Harmonic.Database.Query,
  connectNeo4j,
  
  -- ========== INGESTION PIPELINE ==========
  module Harmonic.Ingestion.CSV,
  module Harmonic.Ingestion.Transform,
  
  -- ========== TIDAL INTERFACE ==========
  -- Pattern-level operations
  VoiceFunction, rootNotes, bassNotes,
  arrange, applyProg, voiceRange,
  lookupProgression, lookupChord, VoiceType(..), voiceBy, harmony,
  overlapF,
  
  -- Arranger functions (voicing paradigms)
  rotate, excerpt, insert, switch, clone, extract,
  transposeP, Harmonic.Tidal.Arranger.reverse, fuse, fuse2, interleave, expandP,
  progOverlap, progOverlapF, progOverlapB,
  root, flow, lite, literal,
  
  module Harmonic.Tidal.Instruments,
  module Harmonic.Tidal.Utils,
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
import Harmonic.Core.Pitch
import Harmonic.Core.Harmony
import Harmonic.Core.Dissonance
import Harmonic.Core.Overtone
import Harmonic.Core.VoiceLeading (voiceLeadingCost, totalCost, cyclicCost, voiceMovement, minimalMovement, allVoicings, initialCompact, solveRoot, solveFlow)
import Harmonic.Core.Progression
-- Phase C: Interactive Behaviour
import Harmonic.Core.Probabilistic
import Harmonic.Core.Builder (HarmonicContext(..), harmonicContext, hContext, defaultContext, GeneratorConfig(..), defaultConfig, generate, generateWith, gen, genWith, generate', gen', genWith', generate'', gen'', genWith'', genSilent, genStandard, genVerbose, genSilent', genStandard', genVerbose', printDiagnostics, StepDiagnostic(..), GenerationDiagnostics(..), TransformTrace(..), AdvanceTrace(..))
import Harmonic.Core.Filter (overtones, key, funds, tuning, wildcard, parseOvertones, parseKey, parseFunds, parseTuning, isWildcard)
import Harmonic.Database.Query
-- Infrastructure (selective imports to avoid conflicts)
import Harmonic.Database.Graph (connectNeo4j)
import Harmonic.Ingestion.CSV
import Harmonic.Ingestion.Transform
import Harmonic.Tidal.Interface (
    VoiceFunction, rootNotes, bassNotes,
    arrange, applyProg, voiceRange,
    lookupProgression, lookupChord, VoiceType(..), voiceBy, harmony,
    overlapF
  )
import Harmonic.Tidal.Arranger (
    rotate, excerpt, insert, switch, clone, extract,
    transposeP, reverse, fuse, fuse2, interleave, expandP,
    progOverlap, progOverlapF, progOverlapB,
    root, flow, lite, literal
  )
import Harmonic.Tidal.Instruments
import Harmonic.Tidal.Utils
import Harmonic.Config
