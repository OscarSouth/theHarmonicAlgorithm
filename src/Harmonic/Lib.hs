-- |
-- Module      : Harmonic.Lib
-- Description : The single import for live coding with theHarmonicAlgorithm
--
-- = Overview
--
-- One import gives a performance script everything it needs: the generative
-- engine, the music-theory primitives it reasons over, and the TidalCycles
-- bridge that turns its output into sound.
--
-- @import Harmonic.Lib@
--
-- = Quick start
--
-- Generation reads as a chain of modifiers applied to a generator. Each
-- modifier narrows what the engine may choose; the generator at the end of
-- the chain runs the walk:
--
-- @
-- tempo = 87
--
-- ctx = invSkip 1
--     $ hcOvertones \"E A D G\"
--     $ hcKey \"2#\"
--     $ hContext
--
-- start \<- lead \"C maj\"
--
-- s \<- seek \"*\" $ cue start $ tonal ctx $ len 8 $ entropy 0.4 $ attempt 3 12 $ gen
-- @
--
-- 'seek' picks the corpus — @\"*\"@ for all composers, @\"bach\"@ for one,
-- @\"bach:30 debussy:70\"@ for a weighted blend, @\"none\"@ to run offline with
-- no Neo4j. 'attempt' generates several candidates and keeps the best.
--
-- The result is then played by describing a form and handing it to
-- instruments:
--
-- @
-- form = [ at 0 1.0 1.0 s ]
--
-- do
--   let k = iK tempo form (warp \"[1 2 3 4]\/4\")
--   mapM_ id [ hush, setbpm tempo
--            , p \"strings\" $ stack
--                [ violin1    T (0,1) k voiceLines flow Soprano
--                , cello      T (0,1) k voiceLines flow Tenor8vb
--                , contrabass T (0,1) k voiceLines grid Bass8vb
--                ]
--            ]
-- @
--
-- = Verbosity
--
-- Every generator has three tiers, marked by the prime suffix — the same
-- convention used throughout the library for tiers and variants:
--
-- * @gen@ — the chord grid only
-- * @gen'@ — per-step musical context and the grid
-- * @gen''@ — full traces, the grid, and the multi-attempt scoreboard
--
-- The tiers have identical types, so switching verbosity never changes the
-- surrounding code. The same holds for @genP@ \/ @genP'@ \/ @genP''@,
-- @genFrom@ \/ @genFrom'@ \/ @genFrom''@, and the Roman numeral aliases.
--
-- = Where to go next
--
-- * "Harmonic.Framework.Builder" — the generation engine and every modifier
-- * "Harmonic.Interface.Tidal.Orchestra" — the 15 orchestral instruments
-- * "Harmonic.Interface.Tidal.Arranger" — voicing strategies and rearranging
-- * "Harmonic.Interface.Tidal.Form" — form and kinetics
--
-- = Legacy positional interface
--
-- Predating the modifier chain, 'genSilent', 'genStandard' and 'genVerbose'
-- take their arguments positionally and share one signature:
--
-- > genSilent :: CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression
--
-- They remain supported, but note that 'initCadenceState' silently truncates
-- chords of more than three pitch classes to a triad; 'lead' is the
-- better-behaved way to build a starting state.
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
-- * Wiggins, G.A. (2006). /A preliminary framework for description, analysis and comparison of creative systems/. Knowledge-Based Systems 19(7). — the R\/T\/E framework and failure taxonomy.
-- * Hindemith, P. (1937). /The Craft of Musical Composition/.
-- * Pastorius, J. (2001). /Modern Electric Bass/. — harmonic vocabulary on bass.
-- * Manring, M. — Hipshot re-tuner technique, multiple D-Tuners.
-- * Bailey, S. & Wooten, V. (1993). /Bass Extremes/. — artificial harmonics.
-- * McLean, A. (2007). /Improvising with Synthesised Vocables/. — TidalCycles origins.

module Harmonic.Lib (
  -- * Primary interface for live coding
  -- | The modifier-based generation API. Each generator comes in three
  -- verbosity tiers, marked by the prime suffix:
  --
  -- * @gen@ — header + grid output
  -- * @gen'@ — compact summary
  -- * @gen''@ — verbose traces
  --
  -- Modifiers compose right-to-left onto a generator:
  --
  -- @s \<- seek "*" $ attempt 3 12 $ entropy 0.4 $ gen@

  -- ** Triadic generation
  gen, gen', gen'',
  genGrid, genGrid', genGrid'', genFrom, genFrom', genFrom'',

  -- ** Chordscale layers (gen \/ genJ)
  -- | gen and genJ contexts carry derived S\/M layers: whole-progression
  -- key-area analysis assigns every bar a key (major, or composite minor)
  -- and realises it as the mode on the bar's root (M, 7 tones) and the
  -- best-fitting anhemitonic pentatonic (S, 5 tones). Automatic at
  -- generation; apply 'chordscale' by hand to 'lead''-built contexts.
  -- 'chordscaleReport' prints the per-bar key \/ form \/ mode \/ pentatonic.
  chordscale,
  renderChordscaleReport, chordscaleReport,

  -- ** The genE paradigm (polytonal)
  -- | Three simultaneous triad progressions from one walk: a foundation
  -- (T) plus two partner chains (S\/M) sharing 2 pitch classes with it per
  -- bar and unioning to 5. Read pairs, the pentad and the pivot tones
  -- through the Layer selectors (TS\/TM\/SM\/TSM\/PT); 'genEReport' prints
  -- every view. See USER_GUIDE section 20 and documents\/POLYTONAL.md.
  genE, genE', genE'',
  polyLayerViews, genEReport,

  -- ** The genJ paradigm (jazz Change graph)
  -- | Generation over the jazz corpus graph: variable-arity leadsheet
  -- harmony (3-6 tones), cued by 'leadJ', walked by the same modifier
  -- chain as every family. See USER_GUIDE section 21.
  genJ, genJ', genJ'',

  -- ** The genP paradigm (strata-first)
  -- | Three-layer generation (triad \/ strata \/ mode). The roman-numeral
  -- aliases pin the starting tristrata; 'genPReport' prints the per-bar
  -- provenance.
  genP, genP', genP'',
  renderTristrataReport, genPReport,
  genI,   genII,   genIII,   genIV,   genV,   genVI,   genVII,   genVIII,   genIX,   genX,   genXI,
  genI',  genII',  genIII',  genIV',  genV',  genVI',  genVII',  genVIII',  genIX',  genX',  genXI',
  genI'', genII'', genIII'', genIV'', genV'', genVI'', genVII'', genVIII'', genIX'', genX'', genXI'',

  -- ** Generation modifiers
  cue, len, seek, entropy, tonal,
  relStrata, absStrata,
  sameBoost, flipBoost, triBoost,
  attempt, viability, steer,

  -- ** Generation types
  GenConfig(..), GenMode(..), Verbosity(..),
  defaultGenConfig, execGenConfig, execGenConfigPC,

  -- ** Positional generation (legacy)
  genSilent, genStandard, genVerbose,
  genPrint, genPrint', genPrint'',

  -- * Context and configuration
  -- | A 'HarmonicContext' constrains what the generator may choose. Build one
  -- by chaining modifiers onto 'hContext':
  --
  -- @ctx = invSkip 1 $ hcOvertones "E A D G" $ hcPedal "E?" $ hContext@
  HarmonicContext(..), harmonicContext, hContext,
  Drift(..), hcOvertones, hcKey, hcRoots, dissonant, consonant, invSkip, hcPedal, hcTristrata,

  -- * Core music types
  module Harmonic.Rules.Types.Pitch,
  module Harmonic.Rules.Types.Harmony,
  module Harmonic.Evaluation.Scoring.Dissonance,
  module Harmonic.Rules.Constraints.Overtone,

  -- ** Voice leading (cyclic DP paradigms)
  voiceLeadingCost, totalCost, cyclicCost,
  voiceMovement, minimalMovement,
  allVoicings, initialCompact,
  solveRoot, solveFlow,

  -- ** Progressions and scales
  module Harmonic.Rules.Types.Progression,
  module Harmonic.Rules.Types.Scale,
  module Harmonic.Rules.Types.ProgressionContext,

  -- * Interactive behaviour
  module Harmonic.Traversal.Probabilistic,

  -- * Filter functions
  -- ** String-friendly versions for TidalCycles
  overtones, Harmonic.Rules.Constraints.Filter.key, funds, tuning, wildcard,
  -- ** Text versions
  parseOvertones, parseKey, parseFunds, parseTuning, isWildcard,
  -- ** Overtone annotation support
  parseTuningNamed,

  -- * Database interface
  module Harmonic.Evaluation.Database.Query,
  connectNeo4j,

  -- * Ingestion pipeline
  -- | Corpus ingestion. Not needed to play — see "Harmonic.Rules.Import.CSV".
  module Harmonic.Rules.Import.CSV,
  module Harmonic.Rules.Import.Transform,

  -- * TidalCycles interface
  -- ** Pattern-level operations
  VoiceFunction, voiceRange,
  arrange, arrange', parallel, warp, rep, lookupChordAt,
  lookupChord, lookupProgression,
  overlapF,

  -- ** Form and kinetics
  FormNode(..), FormTime(..), Transition(..), Kinetics(..), IK,
  at, at', rh, rh', iK, lK, formK,
  ki, slate, withForm,

  -- ** Arranger functions (voicing paradigms)
  rotate, excerpt, insert, switch, clone, extract,
  transposeP, Harmonic.Interface.Tidal.Arranger.reverse, fuse, fuse2, interleave, expandP,
  progOverlap, progOverlapF, progOverlapB,
  grid, flow, lite, literal, root,

  -- ** Explicit progression construction
  fromChords, prog,

  -- ** Groove interface (drums and sub bass)
  subKick, fund, noteoff,
  son32, son23, rumba32, rumba23, bossa32, bossa23,
  bellpat32, bellpat23,

  -- ** Walking-bass line interface
  lineHarmony,

  -- ** Scale source (switch mechanism)
  ScaleSource(..), melodyStateFrom,

  -- ** Starting state construction
  lead, lead', leadJ, parseLeadTokens, LeadToken(..),

  -- ** Instruments and orchestra
  module Harmonic.Interface.Tidal.Instruments,
  module Harmonic.Interface.Tidal.Orchestra,
  module Harmonic.Interface.Tidal.Utils,
  module Harmonic.Interface.Tidal.Motif,
  module Harmonic.Interface.Tidal.Display,
  module Harmonic.Interface.Tidal.Devices.S1,
  module Harmonic.Interface.Tidal.Devices.P6,
  module Harmonic.Interface.Tidal.Devices.JV1010,
  module Harmonic.Config,

  -- * Internal (advanced use only)
  -- | Tuple-returning versions for manual diagnostics extraction.
  generate, generateWith,
  generate', generate'',
  printDiagnostics,
  StepDiagnostic(..), GenerationDiagnostics(..),
  TransformTrace(..), AdvanceTrace(..),
) where

-- Phase B: Core Music Types
import Harmonic.Rules.Types.Pitch
-- hiding: the Layer-B mostConsonant replica (test-only export; the
-- Dissonance copy is the production one and keeps the Lib name)
-- 'mostConsonant' has replicas in both Harmony and Dissonance (pinned to
-- agreement by the suite); hide one so the re-export is unambiguous.
import Harmonic.Rules.Types.Harmony hiding (mostConsonant)
import Harmonic.Evaluation.Scoring.Dissonance
import Harmonic.Rules.Constraints.Overtone
import Harmonic.Evaluation.Scoring.VoiceLeading (voiceLeadingCost, totalCost, cyclicCost, voiceMovement, minimalMovement, allVoicings, initialCompact, solveRoot, solveFlow)
import Harmonic.Rules.Types.Progression
import Harmonic.Rules.Types.Scale
import Harmonic.Rules.Types.ProgressionContext
-- Phase C: Interactive Behaviour
import Harmonic.Traversal.Probabilistic
import Harmonic.Framework.Builder (
    -- Modifier-based API
    gen, gen', gen'',
    genE, genE', genE'',
    genGrid, genGrid', genGrid'', genFrom, genFrom', genFrom'',
    -- genJ paradigm (jazz Change graph)
    genJ, genJ', genJ'',
    -- genP paradigm (strata-first)
    genP, genP', genP'',
    genI,   genII,   genIII,   genIV,   genV,   genVI,   genVII,   genVIII,   genIX,   genX,   genXI,
    genI',  genII',  genIII',  genIV',  genV',  genVI',  genVII',  genVIII',  genIX',  genX',  genXI',
    genI'', genII'', genIII'', genIV'', genV'', genVI'', genVII'', genVIII'', genIX'', genX'', genXI'',
    cue, len, seek, entropy, tonal,
    relStrata, absStrata,
    sameBoost, flipBoost, triBoost,
    attempt, viability, steer,
    GenConfig(..), GenMode(..), Verbosity(..),
    defaultGenConfig, execGenConfig, execGenConfigPC,
    -- Positional API
    genPrint, genPrint', genPrint'',
    generate, generateWith,
    generate', generate'',
    genSilent, genStandard, genVerbose,
    printDiagnostics,
    -- Context & types
    HarmonicContext(..), harmonicContext, hContext,
    Drift(..), hcOvertones, hcKey, hcRoots, dissonant, consonant, invSkip, hcPedal, hcTristrata,
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
    arrange, arrange', parallel, warp, rep, lookupChordAt,
    lookupChord, lookupProgression,
    overlapF
  )
import Harmonic.Interface.Tidal.Arranger (
    rotate, excerpt, insert, switch, clone, extract,
    transposeP, reverse, fuse, fuse2, interleave, expandP,
    progOverlap, progOverlapF, progOverlapB,
    grid, flow, lite, literal, root,
    fromChords, prog,
    ScaleSource(..), melodyStateFrom,
    lead, lead', leadJ, parseLeadTokens, LeadToken(..)
  )
import Harmonic.Interface.Tidal.Groove
  ( subKick, fund, noteoff
  , son32, son23, rumba32, rumba23, bossa32, bossa23
  , bellpat32, bellpat23 )
import Harmonic.Interface.Tidal.LineHarmony (lineHarmony)
import Harmonic.Interface.Tidal.Form (
    FormNode(..), FormTime(..), Transition(..), Kinetics(..), IK,
    at, at', rh, rh', iK, lK, formK,
    ki, slate, withForm
  )
import Harmonic.Interface.Tidal.Instruments
import Harmonic.Interface.Tidal.Orchestra
import Harmonic.Interface.Tidal.Utils
import Harmonic.Interface.Tidal.Motif
import Harmonic.Interface.Tidal.Display
import Harmonic.Interface.Tidal.Devices.S1
import Harmonic.Interface.Tidal.Devices.P6
import Harmonic.Interface.Tidal.Devices.JV1010
import Harmonic.Interface.Tidal.OctatripentatonicT (renderTristrataReport, genPReport)
import Harmonic.Interface.Tidal.PolytonalT (polyLayerViews, genEReport)
import Harmonic.Evaluation.Analysis.KeyArea (chordscale)
import Harmonic.Interface.Tidal.ChordscaleT (renderChordscaleReport, chordscaleReport)
import Harmonic.Config
