{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Core.Builder
-- Description : Generative engine for harmonic progressions
-- 
-- This module implements the main generation loop that connects:
--   * R (Rules): HarmonicContext constraints via Filter module
--   * E (Evaluation): Database-derived composer probabilities
--   * T (Traversal): Voice leading optimization
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
import           Control.Monad (foldM)
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
  , sdGraphTop3       :: [(String, Double)] -- ^ Top 3 graph candidates with confidence
  , sdFallbackCount   :: Int              -- ^ Number of fallback candidates used
  , sdFallbackTop3    :: [(String, Double)] -- ^ Top 3 fallback candidates with score
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

-- |Generate a progression with full diagnostic output.
-- Returns both the progression and detailed per-step diagnostics.
generate' :: H.CadenceState -> Int -> String -> Double -> HarmonicContext 
          -> IO (Prog.Progression, GenerationDiagnostics)
generate' start len composerStr entropy ctx = 
  genWith' defaultConfig start len composerStr entropy ctx

-- |String-friendly generate' for TidalCycles live coding.
gen' :: H.CadenceState -> Int -> String -> Double -> HarmonicContext 
     -> IO (Prog.Progression, GenerationDiagnostics)
gen' = generate'

-- |Generate with custom configuration and diagnostics
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
  
  pure (prog, diag)

-------------------------------------------------------------------------------
-- Generation with Maximum Diagnostics (Verbosity 2)
-------------------------------------------------------------------------------

-- |Generate with maximum verbosity diagnostic output.
-- Returns both the progression and detailed per-step diagnostics with full transform traces.
-- 
-- NOTE: This is slower than gen' due to extra tracing computation.
-- Use for debugging transform discrepancies only.
generate'' :: H.CadenceState -> Int -> String -> Double -> HarmonicContext 
           -> IO (Prog.Progression, GenerationDiagnostics)
generate'' start len composerStr entropy ctx = 
  genWith'' defaultConfig start len composerStr entropy ctx

-- |String-friendly generate'' for TidalCycles live coding.
-- Maximum verbosity with full transform traces.
gen'' :: H.CadenceState -> Int -> String -> Double -> HarmonicContext 
      -> IO (Prog.Progression, GenerationDiagnostics)
gen'' = generate''

-- |Generate with custom configuration and maximum diagnostics.
-- Includes full transform traces for debugging chord name discrepancies.
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
-- Diagnostic Chain Building (with Verbosity)
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
      pool = graphCandidates ++ fallbackCandidates
  
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
            , sdGraphTop3 = []
            , sdFallbackCount = 0
            , sdFallbackTop3 = []
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
          graphTop3 = take 3 [(show cad, conf) | (cad, conf) <- graphCandidates]
          fallbackTop3 = take 3 [(show cad, score) | (cad, score) <- fallbackCandidates]
          
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
            , sdGraphTop3 = graphTop3
            , sdFallbackCount = fallbackCount
            , sdFallbackTop3 = fallbackTop3
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
      fallbackCandidates = if needed > 0
                           then take needed $ consonanceFallback current context
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
consonanceFallback :: H.CadenceState -> HarmonicContext -> [(H.Cadence, Double)]
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
      
      -- Score by consonance (lower dissonance = higher score)
      -- Use 100 - dissonance so more consonant triads rank higher
      -- Compute actual movement from current root to triad root (head of sorted triad)
      scored = [(triadToCadenceFrom currentRoot t, 100 - fromIntegral (dissonanceScore t)) 
               | t <- uniqueTriads]
      
      -- Sort by score (highest first = most consonant)
  in sortBy (compare `on` (Down . snd)) scored

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
