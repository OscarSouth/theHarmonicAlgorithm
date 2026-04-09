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
-- let start = initCadenceState 0 "C" [0,4,7]
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
--
-- = Academic Lineage
--
-- This project originated from three academic documents:
--
-- 1. /The Harmonic Algorithm/ (South, 2016) — MA thesis: exhaustive
--    combinatorial analysis of overtone harmonics on the Electric Contrabass
--    Cittern (EAeGB\/EAeGC tunings) and standard bass (EADG). Charts all
--    3-note overtone combinations over 12 chromatic bass notes.
--
-- 2. /Harmonic Algorithm Reflections/ (South, 2016) — companion reflective
--    document. Contains \"The Parting Glass\" arrangement score and documents
--    technique development including Three Point Playing and Overtone 5
--    discovery.
--
-- 3. /Data Science In The Creative Process/ (South, 2018) — DBS Higher
--    Diploma. Implements the algorithm computationally in Haskell using
--    Wiggins' Creative Systems Framework \<R,T,E\>. Resolves \"Generative
--    Uninspiration\" via Markov model trained on the YCACL Bach chorales.
--
-- = References
--
-- * South, O. (2016). /The Harmonic Algorithm/. MA thesis.
-- * South, O. (2016). /Harmonic Algorithm Reflections/.
-- * South, O. (2018). /Data Science In The Creative Process/. DBS.
-- * Wiggins, G.A. (2001). /Towards a more precise characterisation of creativity in AI/.
-- * Hindemith, P. (1937). /The Craft of Musical Composition/.
-- * Pastorius, J. (2001). /Modern Electric Bass/. — harmonic vocabulary on bass.
-- * Manring, M. — Hipshot re-tuner technique, multiple D-Tuners.
-- * Bailey, S. & Wooten, V. (1993). /Bass Extremes/. — artificial harmonics.
-- * McLean, A. (2007). /Improvising with Synthesised Vocables/. — TidalCycles origins.

module Harmonic.Lib (
  -- ========== PRIMARY INTERFACE FOR LIVE CODING ==========
  -- Modifier-based generation API:
  --   gen   - header + grid output
  --   gen'  - compact summary
  --   gen'' - verbose traces
  gen, gen', gen'',
  genGrid, genFrom,

  -- ========== GENERATION MODIFIERS ==========
  cue, len, seek, entropy, tonal,

  -- ========== GENERATION TYPES ==========
  GenConfig(..), GenMode(..), Verbosity(..),
  defaultGenConfig, execGenConfig,

  -- ========== POSITIONAL GENERATION (legacy) ==========
  genSilent, genStandard, genVerbose,
  genPrint, genPrint', genPrint'',

  -- ========== CONTEXT & CONFIGURATION ==========
  HarmonicContext(..), harmonicContext, hContext, defaultContext,
  Drift(..), hcOvertones, hcKey, hcRoots, dissonant, consonant, invSkip,
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
  -- Overtone annotation support
  parseTuningNamed,

  -- ========== DATABASE INTERFACE ==========
  module Harmonic.Evaluation.Database.Query,
  connectNeo4j,

  -- ========== INGESTION PIPELINE ==========
  module Harmonic.Rules.Import.CSV,
  module Harmonic.Rules.Import.Transform,

  -- ========== TIDAL INTERFACE ==========
  -- Pattern-level operations
  VoiceFunction, voiceRange,
  arrange, arrange', warp, rep, lookupChordAt,
  lookupChord, lookupProgression, VoiceType(..), voiceBy, harmony,
  overlapF,

  -- Form / Kinetics
  FormNode(..), Kinetics(..), IK, at, iK, formK,
  ki, slate, withForm,
  -- Arranger functions (voicing paradigms)
  rotate, excerpt, insert, switch, clone, extract,
  transposeP, Harmonic.Interface.Tidal.Arranger.reverse, fuse, fuse2, interleave, expandP,
  progOverlap, progOverlapF, progOverlapB,
  grid, flow, lite, literal, root,

  -- Explicit progression construction
  fromChords, prog,

  -- Groove interface (drums/sub bass)
  subKick, fund,

  -- Scale source (switch mechanism)
  ScaleSource(..), melodyStateFrom,

  -- Starting state construction
  lead, parseLeadTokens, LeadToken(..),

  module Harmonic.Interface.Tidal.Instruments,
  module Harmonic.Interface.Tidal.Orchestra,
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
import Harmonic.Framework.Builder (
    -- Modifier-based API
    gen, gen', gen'',
    genGrid, genFrom,
    cue, len, seek, entropy, tonal,
    GenConfig(..), GenMode(..), Verbosity(..),
    defaultGenConfig, execGenConfig,
    -- Positional API
    genPrint, genPrint', genPrint'',
    generate, generateWith, genWith,
    generate', genWith',
    generate'', genWith'',
    genSilent, genStandard, genVerbose,
    genSilent', genStandard', genVerbose',
    printDiagnostics,
    -- Context & types
    HarmonicContext(..), harmonicContext, hContext, defaultContext,
    Drift(..), hcOvertones, hcKey, hcRoots, dissonant, consonant, invSkip,
    GeneratorConfig(..), defaultConfig,
    StepDiagnostic(..), GenerationDiagnostics(..), TransformTrace(..), AdvanceTrace(..)
  )
import Harmonic.Rules.Constraints.Filter (overtones, key, funds, tuning, wildcard, parseOvertones, parseKey, parseFunds, parseTuning, isWildcard, parseTuningNamed)
import Harmonic.Evaluation.Database.Query
-- Infrastructure (selective imports to avoid conflicts)
import Harmonic.Rules.Import.Graph (connectNeo4j)
import Harmonic.Rules.Import.CSV
import Harmonic.Rules.Import.Transform
import Harmonic.Interface.Tidal.Bridge (
    VoiceFunction, voiceRange,
    arrange, arrange', warp, rep, lookupChordAt,
    lookupChord, lookupProgression, VoiceType(..), voiceBy, harmony,
    overlapF
  )
import Harmonic.Interface.Tidal.Arranger (
    rotate, excerpt, insert, switch, clone, extract,
    transposeP, reverse, fuse, fuse2, interleave, expandP,
    progOverlap, progOverlapF, progOverlapB,
    grid, flow, lite, literal, root,
    fromChords, prog,
    ScaleSource(..), melodyStateFrom,
    lead, parseLeadTokens, LeadToken(..)
  )
import Harmonic.Interface.Tidal.Groove (subKick, fund)
import Harmonic.Interface.Tidal.Form (
    FormNode(..), Kinetics(..), IK, at, iK, formK,
    ki, slate, withForm
  )
import Harmonic.Interface.Tidal.Instruments
import Harmonic.Interface.Tidal.Orchestra
import Harmonic.Interface.Tidal.Utils
import Harmonic.Config
