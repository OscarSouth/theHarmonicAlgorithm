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

    -- * Pre-parsed Context
  , ParsedContext(..)
  , parseContextOnce

    -- * Bass Direction (re-exported from Filter)
  , BassDirection(..)

    -- * Generation Configuration (Modifier-Based API)
  , Verbosity(..)
  , GenConfig(..)
  , GenMode(..)

    -- * Diagnostics Types
  , TransformTrace(..)
  , AdvanceTrace(..)
  , StepDiagnostic(..)
  , GenerationDiagnostics(..)
  ) where

import           Data.List (intercalate)
import qualified Data.IntSet as IntSet
import qualified Data.Text as T
import           Data.Text (Text)

import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Progression as Prog
import           Harmonic.Rules.Constraints.Filter (parseOvertones', parseKey, isWildcard, resolveRoots,
                                                     BassDirection(..), parseBassDirection, stripDirectionToken,
                                                     noteNameToPitchClass)

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
  { _hcOvertones        :: Text   -- ^ Filter by overtone content ("*" = all)
  , _hcKey              :: Text   -- ^ Filter by key signature ("C", "#", "bb", "*")
  , _hcRoots            :: Text   -- ^ Filter by root notes ("*" = all)
  , _hcDrift            :: Drift  -- ^ Dissonance drift direction
  , _hcInversionSpacing :: Int    -- ^ Minimum non-inversions between inversions (default 0)
  , _hcPedal            :: Text   -- ^ Required/preferred tones ("C G?", "" = no pedal)
  } deriving (Eq)

instance Show HarmonicContext where
  show ctx = intercalate " | " $ filter (not . null)
    [ "overtones " ++ T.unpack (_hcOvertones ctx)
    , "key " ++ T.unpack (_hcKey ctx)
    , "roots " ++ T.unpack (_hcRoots ctx)
    , driftStr (_hcDrift ctx)
    , invStr (_hcInversionSpacing ctx)
    , pedalStr (_hcPedal ctx)
    ]
    where
      driftStr Free      = ""
      driftStr Dissonant = "drift dissonant"
      driftStr Consonant = "drift consonant"
      invStr 0 = ""
      invStr n = "inv skip " ++ show n
      pedalStr t = if T.null (T.strip t) then "" else "pedal " ++ T.unpack t

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
harmonicContext o k r = HarmonicContext o k r Free 0 ""

-- |Default harmonic context for Tidal live coding: all wildcards (chromatic).
-- Named 'hContext' to avoid collision with TidalCycles' EventF.context field.
--
-- Use modifier functions to constrain the context:
--
-- @
-- ctx = invSkip 2
--     $ consonant
--     $ hcRoots "C E G"
--     $ hcKey "0#"
--     $ hcOvertones "E A D G"
--     $ hContext
-- @
hContext :: HarmonicContext
hContext = HarmonicContext "*" "*" "*" Free 0 ""

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

-- |Set overtone filter. Default: @"*"@ (all pitches).
--
-- @hcOvertones "E A D G" $ hContext@ — bass tuning overtones
hcOvertones :: String -> HarmonicContext -> HarmonicContext
hcOvertones o ctx = ctx { _hcOvertones = T.pack o }

-- |Set key filter. Default: @"*"@ (chromatic).
--
-- @hcKey "0#" $ hContext@ — C major
hcKey :: String -> HarmonicContext -> HarmonicContext
hcKey k ctx = ctx { _hcKey = T.pack k }

-- |Set roots/bass filter. Default: @"*"@ (all roots).
--
-- @hcRoots "C E G" $ hContext@ — only C, E, G as bass notes
hcRoots :: String -> HarmonicContext -> HarmonicContext
hcRoots r ctx = ctx { _hcRoots = T.pack r }

-- |Modify context to trend toward increasing dissonance.
-- Each subsequent chord must have dissonance >= the current chord.
dissonant :: HarmonicContext -> HarmonicContext
dissonant ctx = ctx { _hcDrift = Dissonant }

-- |Modify context to trend toward decreasing dissonance.
-- Each subsequent chord must have dissonance <= the current chord.
consonant :: HarmonicContext -> HarmonicContext
consonant ctx = ctx { _hcDrift = Consonant }

-- |Set minimum number of non-inversion states between inversions.
--
-- @invSkip 0@ allows inversions at any step (default, current behaviour).
-- @invSkip 1@ requires at least 1 non-inversion between inversions.
-- @invSkip 2@ requires at least 2 non-inversions between inversions.
-- The starting state counts toward the counter (a non-inversion start
-- means the first generated step may already be an inversion with @invSkip 1@).
invSkip :: Int -> HarmonicContext -> HarmonicContext
invSkip n ctx = ctx { _hcInversionSpacing = n }

-- |Require specific pitch classes to be present in every generated chord.
--
-- Tokens are note names (@"C"@, @"G#"@, @"Bb"@). A trailing @?@ marks a tone
-- as preferred rather than required — it is applied when it does not reduce
-- the candidate pool below a minimum viable size, and relaxed otherwise.
--
-- @hcPedal "C" $ hContext@       — C must appear in every chord
-- @hcPedal "C G" $ hContext@     — C and G must both appear
-- @hcPedal "C G?" $ hContext@    — C required, G preferred
hcPedal :: String -> HarmonicContext -> HarmonicContext
hcPedal p ctx = ctx { _hcPedal = T.pack p }

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
  , pcInversionSpacing   :: !Int                    -- ^ Minimum non-inversions between inversions
  , pcPedalRequired      :: !IntSet.IntSet  -- ^ Pitch classes that must be present in every chord
  , pcPedalPreferred     :: !IntSet.IntSet  -- ^ Preferred pitch classes (relaxed if pool too small)
  }

-- |Parse pedal tone string into required and preferred IntSets.
-- Tokens ending in '?' are preferred; all others are required.
-- Invalid note names are silently ignored.
parsePedalTones :: Text -> (IntSet.IntSet, IntSet.IntSet)
parsePedalTones input
  | T.null (T.strip input) = (IntSet.empty, IntSet.empty)
  | otherwise =
      let tokens = T.words input
          classify t
            | T.isSuffixOf "?" t = (False, noteNameToPitchClass (T.init t))
            | otherwise          = (True,  noteNameToPitchClass t)
          pairs = map classify tokens
          required  = IntSet.fromList [pc | (True,  Just pc) <- pairs]
          preferred = IntSet.fromList [pc | (False, Just pc) <- pairs]
      in (required, preferred)

-- |Parse a HarmonicContext once into efficient lookup structures.
parseContextOnce :: HarmonicContext -> ParsedContext
parseContextOnce ctx =
  let rawOvertones = parseOvertones' 4 (_hcOvertones ctx)
      keyPcs = parseKey (_hcKey ctx)
      keyWild = isWildcard (_hcKey ctx)
      effectiveOvertones = if keyWild
                           then rawOvertones
                           else filter (`elem` keyPcs) rawOvertones
      -- Strip direction token before resolving roots
      rootsRaw = _hcRoots ctx
      rootsStripped = stripDirectionToken rootsRaw
      bassDir = parseBassDirection rootsRaw
      allowedBassNotes = resolveRoots (_hcOvertones ctx) (_hcKey ctx) rootsStripped
      (pedalReq, pedalPref) = parsePedalTones (_hcPedal ctx)
  in ParsedContext
    { pcEffectiveOvertones = IntSet.fromList effectiveOvertones
    , pcAllowedBassNotes   = IntSet.fromList allowedBassNotes
    , pcIsRootsWild        = isWildcard rootsStripped
    , pcIsKeyWild          = keyWild
    , pcIsOvertonesWild    = isWildcard (_hcOvertones ctx)
    , pcRawOvertones       = rawOvertones
    , pcBassDirection      = bassDir
    , pcDrift              = _hcDrift ctx
    , pcInversionSpacing   = _hcInversionSpacing ctx
    , pcPedalRequired      = pedalReq
    , pcPedalPreferred     = pedalPref
    }

-------------------------------------------------------------------------------
-- Generation Configuration (Modifier-Based API)
-------------------------------------------------------------------------------

-- |Verbosity level for generation output.
data Verbosity = Silent | Standard | Verbose deriving (Show, Eq)

-- |Generation mode.
data GenMode
  = Fresh                                      -- ^ Standard gen (new progression)
  | FromProg Prog.Progression !Int !Int        -- ^ Regenerate range in existing
  | GridMode                                    -- ^ Static repetition of cue chord

-- |Configuration for the modifier-based generation API.
--
-- Built via modifier chains:
--
-- @
-- s <- seek "*" $ cue start $ tonal ctx $ len 4 $ entropy 0.3 $ gen
-- @
data GenConfig = GenConfig
  { _gcCue       :: IO H.CadenceState  -- ^ Starting state (default: random)
  , _gcLen       :: Int                 -- ^ Number of chords (default: 4)
  , _gcSeek      :: String              -- ^ Composer blend string (default: "*")
  , _gcEntropy   :: Double              -- ^ Gamma shape parameter (default: 0.2)
  , _gcTonal     :: HarmonicContext     -- ^ R constraints (default: hContext)
  , _gcVerbosity :: Verbosity           -- ^ Output level (default: Silent)
  , _gcMode      :: GenMode             -- ^ Generation mode (default: Fresh)
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
