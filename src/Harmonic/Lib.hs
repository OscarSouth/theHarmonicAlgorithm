-- |
-- Module      : Harmonic.Lib
-- Description : Re-export convenience module for Phase B + Phase C types
--
-- For GHCi exploration, import this module to get access to all Phase B
-- types and Phase C generation functions.

module Harmonic.Lib (
  -- Phase B: Core Music Types (New)
  module Harmonic.Core.Pitch,
  module Harmonic.Core.Harmony,
  module Harmonic.Core.Dissonance,
  module Harmonic.Core.Overtone,
  -- VoiceLeading (explicit exports - cyclic DP paradigms only)
  voiceLeadingCost, totalCost, cyclicCost,
  voiceMovement, minimalMovement,
  allVoicings, initialCompact,
  solveRoot, solveFlow,
  module Harmonic.Core.Progression,
  -- Phase C: Interactive Behaviour
  module Harmonic.Core.Probabilistic,
  -- Builder (now fully Phase B)
  HarmonicContext(..), harmonicContext, hContext, defaultContext,
  GeneratorConfig(..), defaultConfig,
  generate, generateWith,
  gen, genWith,  -- String-friendly versions for TidalCycles
  -- Diagnostic generation (verbosity 1)
  generate', gen', genWith',
  -- Maximum verbosity generation (verbosity 2)
  generate'', gen'', genWith'',
  -- Diagnostic types
  StepDiagnostic(..), GenerationDiagnostics(..),
  TransformTrace(..), AdvanceTrace(..),
  -- Filter functions (String-friendly for Tidal)
  overtones, Harmonic.Core.Filter.key, funds, tuning, wildcard,
  -- Filter functions (Text versions)
  parseOvertones, parseKey, parseFunds, parseTuning, isWildcard,
  module Harmonic.Database.Query,
  -- Infrastructure (excluding conflicts)
  connectNeo4j,  -- from Graph (Query has its own ComposerWeights)
  module Harmonic.Ingestion.CSV,
  module Harmonic.Ingestion.Transform,
  -- Tidal Interface (pattern-level operations)
  VoiceFunction, rootNotes, bassNotes,
  arrange, applyProg, voiceRange,
  lookupProgression, lookupChord, VoiceType(..), voiceBy, harmony,
  overlapF,  -- Pattern-level overlap from Interface
  -- Arranger functions (voicing paradigms)
  rotate, excerpt, insert, switch, clone, extract,
  transposeP, Harmonic.Tidal.Arranger.reverse, fuse, fuse2, interleave, expandP,
  progOverlap, progOverlapF, progOverlapB,
  root, flow, lite, literal,  -- 3 voicing paradigms
  module Harmonic.Tidal.Instruments,
  module Harmonic.Tidal.Utils,
  module Harmonic.Config
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
import Harmonic.Core.Builder (HarmonicContext(..), harmonicContext, hContext, defaultContext, GeneratorConfig(..), defaultConfig, generate, generateWith, gen, genWith, generate', gen', genWith', generate'', gen'', genWith'', StepDiagnostic(..), GenerationDiagnostics(..), TransformTrace(..), AdvanceTrace(..))
import Harmonic.Core.Filter (overtones, key, funds, tuning, wildcard, parseOvertones, parseKey, parseFunds, parseTuning, isWildcard)
import Harmonic.Database.Query
-- Infrastructure (selective imports to avoid conflicts)
import Harmonic.Database.Graph (connectNeo4j)  -- ComposerWeights comes from Query
import Harmonic.Ingestion.CSV
import Harmonic.Ingestion.Transform
import Harmonic.Tidal.Interface (
    VoiceFunction, rootNotes, bassNotes,
    arrange, applyProg, voiceRange,
    lookupProgression, lookupChord, VoiceType(..), voiceBy, harmony,
    overlapF  -- Pattern-level overlap for sustain/legato
  )  -- Exclude progLength (use Progression.progLength)
import Harmonic.Tidal.Arranger (
    rotate, excerpt, insert, switch, clone, extract,
    transposeP, reverse, fuse, fuse2, interleave, expandP,
    progOverlap, progOverlapF, progOverlapB,
    root, flow, lite, literal
  )
import Harmonic.Tidal.Instruments
import Harmonic.Tidal.Utils
import Harmonic.Config
