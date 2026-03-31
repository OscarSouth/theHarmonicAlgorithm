{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Framework.Builder.Types
-- Description : Types for the harmonic generation engine
--
-- Data types and configuration for the Builder module.
-- Includes HarmonicContext, GeneratorConfig, ParsedContext,
-- and all diagnostic types.

module Harmonic.Framework.Builder.Types
  ( -- * Harmonic Context (R constraints)
    HarmonicContext(..)
  , harmonicContext
  , hContext
  , defaultContext

    -- * Dissonance Drift
  , Drift(..)
  , dissonant
  , consonant

    -- * Configuration
  , GeneratorConfig(..)
  , defaultConfig

    -- * Pre-parsed Context
  , ParsedContext(..)
  , parseContextOnce

    -- * Bass Direction (re-exported from Filter)
  , BassDirection(..)

    -- * Diagnostics Types
  , TransformTrace(..)
  , AdvanceTrace(..)
  , StepDiagnostic(..)
  , GenerationDiagnostics(..)
  ) where

import qualified Data.IntSet as IntSet
import qualified Data.Text as T
import           Data.Text (Text)

import qualified Harmonic.Rules.Types.Progression as Prog
import           Harmonic.Rules.Constraints.Filter (parseOvertones', parseKey, isWildcard, resolveRoots,
                                                     BassDirection(..), parseBassDirection, stripDirectionToken)

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
  { hcOvertones :: Text   -- ^ Filter by overtone content ("*" = all)
  , hcKey       :: Text   -- ^ Filter by key signature ("C", "#", "bb", "*")
  , hcRoots     :: Text   -- ^ Filter by root notes ("*" = all)
  , hcDrift     :: Drift  -- ^ Dissonance drift direction
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
harmonicContext o k r = HarmonicContext o k r Free

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
hContext o k r = HarmonicContext (T.pack o) (T.pack k) (T.pack r) Free

-- |Default context: no filtering (wildcards everywhere)
defaultContext :: HarmonicContext
defaultContext = HarmonicContext "*" "*" "*" Free

-------------------------------------------------------------------------------
-- Dissonance Drift
-------------------------------------------------------------------------------

-- |Direction of dissonance drift across a generated progression.
--
-- When applied to a HarmonicContext, the generation engine filters the
-- candidate pool at each step so that only chords with equal or greater
-- (Dissonant) or equal or lesser (Consonant) dissonance than the current
-- chord are eligible. Free imposes no constraint (default).
data Drift = Dissonant | Consonant | Free deriving (Show, Eq)

-- |Modify context to trend toward increasing dissonance.
-- Each subsequent chord must have dissonance >= the current chord.
dissonant :: HarmonicContext -> HarmonicContext
dissonant ctx = ctx { hcDrift = Dissonant }

-- |Modify context to trend toward decreasing dissonance.
-- Each subsequent chord must have dissonance <= the current chord.
consonant :: HarmonicContext -> HarmonicContext
consonant ctx = ctx { hcDrift = Consonant }

-------------------------------------------------------------------------------
-- Generator Configuration
-------------------------------------------------------------------------------

-- |Configuration for the progression generator.
data GeneratorConfig = GeneratorConfig
  { gcPoolSize :: Int  -- ^ Candidate pool size (default 30)
  } deriving (Show, Eq)

-- |Default configuration.
defaultConfig :: GeneratorConfig
defaultConfig = GeneratorConfig
  { gcPoolSize = 30
  }

-- |Pre-parsed HarmonicContext for O(1) membership tests.
-- Computed once per generation run, avoiding repeated text parsing.
data ParsedContext = ParsedContext
  { pcEffectiveOvertones :: !IntSet.IntSet  -- ^ Key-filtered overtone pitch classes
  , pcAllowedBassNotes   :: !IntSet.IntSet  -- ^ Resolved root/bass pitch classes
  , pcIsRootsWild        :: !Bool           -- ^ Whether roots filter is wildcard
  , pcIsKeyWild          :: !Bool           -- ^ Whether key filter is wildcard
  , pcIsOvertonesWild    :: !Bool           -- ^ Whether overtones filter is wildcard
  , pcRawOvertones       :: ![Int]          -- ^ Raw overtone list (for fallback triad generation)
  , pcBassDirection      :: !(Maybe BassDirection)  -- ^ Rise/fall bass direction constraint
  , pcDrift              :: !Drift                  -- ^ Dissonance drift direction
  }

-- |Parse a HarmonicContext once into efficient lookup structures.
parseContextOnce :: HarmonicContext -> ParsedContext
parseContextOnce ctx =
  let rawOvertones = parseOvertones' 4 (hcOvertones ctx)
      keyPcs = parseKey (hcKey ctx)
      keyWild = isWildcard (hcKey ctx)
      effectiveOvertones = if keyWild
                           then rawOvertones
                           else filter (`elem` keyPcs) rawOvertones
      -- Strip direction token before resolving roots
      rootsRaw = hcRoots ctx
      rootsStripped = stripDirectionToken rootsRaw
      bassDir = parseBassDirection rootsRaw
      allowedBassNotes = resolveRoots (hcOvertones ctx) (hcKey ctx) rootsStripped
  in ParsedContext
    { pcEffectiveOvertones = IntSet.fromList effectiveOvertones
    , pcAllowedBassNotes   = IntSet.fromList allowedBassNotes
    , pcIsRootsWild        = isWildcard rootsStripped
    , pcIsKeyWild          = keyWild
    , pcIsOvertonesWild    = isWildcard (hcOvertones ctx)
    , pcRawOvertones       = rawOvertones
    , pcBassDirection      = bassDir
    , pcDrift              = hcDrift ctx
    }

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
  , sdFallbackTop6    :: [(String, Double, Double, Double, Double)] -- ^ Top 6 fallback candidates (name, score, chordDiss, motionDiss, gammaDraw)
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
