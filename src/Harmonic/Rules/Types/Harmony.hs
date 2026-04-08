{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

-- |
-- Module      : Harmonic.Rules.Types.Harmony
-- Description : Chord, Cadence, and their Concrete State representations
--
-- This module establishes the Abstract\/Concrete distinction:
--
--   Abstract (transposition-invariant):
--
--     * 'Chord'   - A pitch structure defined by intervals from root
--     * 'Cadence' - A movement type and target chord quality
--
--   Concrete (rooted in pitch space):
--
--     * 'ChordState'   - A Chord with a specific root pitch
--     * 'CadenceState' - A Cadence with a specific root pitch
--
-- == Academic Lineage
--
-- /Data Science In The Creative Process/ (South, 2018): the MusicData
-- module's chord naming conventions (@nameFuncTriad@, @nameFuncChord@)
-- ported verbatim from legacy MusicData.hs (lines 443-479).
--
-- /The Harmonic Algorithm/ (South, 2016): the concept of harmonic
-- extrapolation categories (Simple, Complex, Impractical) which informed
-- the chord quality taxonomy used here.

module Harmonic.Rules.Types.Harmony
  ( -- * Functionality (Chord Quality)
    Functionality
  , toFunctionality
  , toFunctionalityChord
  
    -- * Abstract Types
  , Chord(..)
  , Cadence(..)
  
    -- * Movement
  , Movement(..)
  , toMovement
  , fromMovement
  
    -- * Enharmonic Spelling
  , EnharmonicSpelling(..)
  , EnharmonicPreference(..)
  , enharmonicPreference
  , selectEnharm
  , defaultEnharm
  , spellingToPreference
  , enharmonicFunc
  , inferSpelling
  , noteNameImpliesSpelling
  , isAmbiguousPattern

    -- * Concrete State Types
  , ChordState(..)
  , CadenceState(..)
  
    -- * Triad Construction (exactly 3 pitches, with reduction)
  , toTriad
  , flatTriad
  , sharpTriad
  
    -- * Chord Construction (preserves all pitches)
  , toChord
  , flatChord
  , sharpChord
  
    -- * Cadence Construction
  , toCadence
  
    -- * State Conversion
  , fromChordState
  , fromCadenceState
  , initCadenceState
  
    -- * Tracing (for maximum verbosity diagnostics)
  , ToTriadTrace(..)
  , fromCadenceStateTraced
  
    -- * DB Serialization (compatible with Neo4j format)
  , constructCadence
  , deconstructCadence
  
    -- * Utilities
  , rootNote
  , inversions
  , isInversion
  , normalForm
  , primeForm
  , zeroFormPC
  ) where

import GHC.Generics (Generic)
import Data.Function (on)
import Data.List (sort, sortBy)
import qualified Data.List as List
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Applicative ((<|>))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

import Harmonic.Rules.Types.Pitch

-------------------------------------------------------------------------------
-- Enharmonic Spelling
-------------------------------------------------------------------------------

-- |Enharmonic spelling preference (sharp or flat)
-- Stored in CadenceState to enable consistent spelling throughout a progression
data EnharmonicSpelling = SharpSpelling | FlatSpelling
  deriving (Eq, Ord, Show, Read, Generic)

-- |Convert EnharmonicSpelling to the actual function (PitchClass -> NoteName)
-- |Classify a pitch class by its natural enharmonic preference.
-- C (PC 0) is AMBIGUOUS - it adopts the pairing's preference (prior or posterior).
-- All other pitch classes have definite preferences.
data EnharmonicPreference = FlatPref | SharpPref | AmbiguousPref
  deriving (Eq, Show)

-- |Get the natural enharmonic preference for a pitch class.
-- C (0) is ambiguous and should be handled specially.
-- F#/Gb (6) is always treated as sharp (F#).
enharmonicPreference :: PitchClass -> EnharmonicPreference
enharmonicPreference (P 0)  = AmbiguousPref  -- C: ambiguous, adopts context
enharmonicPreference (P 1)  = FlatPref       -- Db
enharmonicPreference (P 2)  = SharpPref      -- D
enharmonicPreference (P 3)  = FlatPref       -- Eb
enharmonicPreference (P 4)  = SharpPref      -- E
enharmonicPreference (P 5)  = FlatPref       -- F
enharmonicPreference (P 6)  = SharpPref      -- F# (tritone treated as sharp)
enharmonicPreference (P 7)  = SharpPref      -- G
enharmonicPreference (P 8)  = FlatPref       -- Ab
enharmonicPreference (P 9)  = SharpPref      -- A
enharmonicPreference (P 10) = FlatPref       -- Bb
enharmonicPreference (P 11) = SharpPref      -- B
enharmonicPreference _      = FlatPref       -- Fallback (shouldn't occur for mod 12)

-- |Determine enharmonic spelling for posterior root given prior's actual spelling and both pitch classes.
-- 
-- Rules:
-- (1) SAME PITCH CLASS: Always use prior's actual spelling (the root hasn't changed!)
--     - Example: D# → D# keep Sharp, Eb → Eb keep Flat
-- (2) C IS FLEXIBLE: When C is prior or posterior, the OTHER pitch's preference can override
--     - Prior is C + posterior is definite: adopt posterior's preference
--     - Posterior is C + prior is definite: adopt prior's actual spelling
--     - Both C: persist current (no preference to guide)
-- (3) BOTH DEFINITE (different pitch classes): Consensus-based switching
--     a) If prior's actual spelling matches posterior's preference → persist (consensus on choice)
--     b) If both have SAME preference AND it differs from prior's actual → switch (consensus to switch)
--     c) Otherwise: persist prior's actual spelling (disagreement or already aligned)
--
-- Examples:
--   D# → D# (same PC) → persist prior's actual spelling (Sharp or Flat, doesn't matter which)
--   D# (actual Sharp) → E (SharpPref) → both prefer Sharp, actual differs → switch to Sharp!
selectEnharm :: EnharmonicSpelling -> PitchClass -> PitchClass -> EnharmonicSpelling
selectEnharm current prior posterior
  -- (1) Same pitch class: always keep prior's actual spelling
  | prior == posterior = current
selectEnharm current prior posterior =
  let priorPref = enharmonicPreference prior
      posteriorPref = enharmonicPreference posterior
      -- Check if prior's actual spelling matches posterior's preference
      prior_actual_matches_posterior_pref = case (current, posteriorPref) of
        (FlatSpelling, FlatPref) -> True
        (SharpSpelling, SharpPref) -> True
        _ -> False
      -- Check if both prefer the same thing but differ from current
      both_same_pref_differs_from_current = case (priorPref, posteriorPref, current) of
        (FlatPref, FlatPref, SharpSpelling) -> True    -- Both flat but we're sharp
        (SharpPref, SharpPref, FlatSpelling) -> True    -- Both sharp but we're flat
        _ -> False
  in case (priorPref, posteriorPref) of
    -- Prior is C (ambiguous): posterior's definite preference overrides
    (AmbiguousPref, FlatPref) -> FlatSpelling
    (AmbiguousPref, SharpPref) -> SharpSpelling
    -- Posterior is C (ambiguous): prior's actual spelling carries through
    (FlatPref, AmbiguousPref) -> current
    (SharpPref, AmbiguousPref) -> current
    -- Both C (ambiguous): persist current
    (AmbiguousPref, AmbiguousPref) -> current
    -- Both definite with same preference
    (FlatPref, FlatPref) ->
      if prior_actual_matches_posterior_pref then FlatSpelling
      else if both_same_pref_differs_from_current then FlatSpelling
      else current
    (SharpPref, SharpPref) ->
      if prior_actual_matches_posterior_pref then SharpSpelling
      else if both_same_pref_differs_from_current then SharpSpelling
      else current
    -- Both definite but disagree in preference: persist current (prior's actual spelling)
    _ -> current

-- |Default enharmonic spelling for initial state based on root pitch class.
-- C defaults to flat; all others use their natural preference.
defaultEnharm :: PitchClass -> EnharmonicSpelling
defaultEnharm (P 0) = FlatSpelling  -- C defaults to flat
defaultEnharm pc = case enharmonicPreference pc of
  FlatPref -> FlatSpelling
  SharpPref -> SharpSpelling
  AmbiguousPref -> FlatSpelling  -- Fallback (only C should be ambiguous)

-- |Convert enharmonic spelling to the corresponding preference (for symmetry).
spellingToPreference :: EnharmonicSpelling -> EnharmonicPreference
spellingToPreference FlatSpelling = FlatPref
spellingToPreference SharpSpelling = SharpPref

enharmonicFunc :: EnharmonicSpelling -> (PitchClass -> NoteName)
enharmonicFunc SharpSpelling = sharp
enharmonicFunc FlatSpelling  = flat

-- |Infer enharmonic spelling from absolute pitches (bass first).
-- Three-layer system:
--   Layer 1: 3-set match — bass matches AND remaining 2 PCs are subset of chord
--   Layer 2: 2-set match — bass matches AND remaining 1 PC is subset of chord
--   Layer 3: Root fallback — defaultEnharm on bass PC
inferSpelling :: [Int] -> EnharmonicSpelling
inferSpelling [] = FlatSpelling
inferSpelling pcs =
  let bassNorm = head pcs `mod` 12
      restSet = IS.fromList (map (`mod` 12) (tail pcs))
      tryTable table = case IM.lookup bassNorm table of
        Nothing -> Nothing
        Just entries ->
          listToMaybe [s | (upperSet, s) <- entries, upperSet `IS.isSubsetOf` restSet]
  in fromMaybe (defaultEnharm (mkPitchClass bassNorm))
       (tryTable layer1Table <|> tryTable layer2Table)

-- Layer 1: 3-set rules (bass PC → [(set of required upper PCs, spelling)])
-- Covers triads (maj/min/sus4/dim/aug) in all inversions,
-- plus maj7/min7 omit-5 and omit-3 in all inversions.
layer1Table :: IM.IntMap [(IS.IntSet, EnharmonicSpelling)]
layer1Table = IM.fromListWith (++) $ concatMap expandRoot
  --             bass offsets from root   spelling
  -- Root position major
  [ rootEntry  0 [4, 7]   FlatSpelling    -- C maj
  , rootEntry  1 [5, 8]   FlatSpelling    -- Db maj
  , rootEntry  2 [6, 9]   SharpSpelling   -- D maj
  , rootEntry  3 [7, 10]  FlatSpelling    -- Eb maj
  , rootEntry  4 [8, 11]  SharpSpelling   -- E maj
  , rootEntry  5 [9, 0]   FlatSpelling    -- F maj
  , rootEntry  6 [10, 1]  SharpSpelling   -- F# maj
  , rootEntry  7 [11, 2]  SharpSpelling   -- G maj
  , rootEntry  8 [0, 3]   FlatSpelling    -- Ab maj
  , rootEntry  9 [1, 4]   SharpSpelling   -- A maj
  , rootEntry 10 [2, 5]   FlatSpelling    -- Bb maj
  , rootEntry 11 [3, 6]   SharpSpelling   -- B maj
  -- 1st inversion major
  , rootEntry  4 [7, 0]   FlatSpelling    -- C maj 1st inv
  , rootEntry  5 [8, 1]   FlatSpelling    -- Db maj 1st inv
  , rootEntry  6 [9, 2]   SharpSpelling   -- D maj 1st inv
  , rootEntry  7 [10, 3]  FlatSpelling    -- Eb maj 1st inv
  , rootEntry  8 [11, 4]  SharpSpelling   -- E maj 1st inv
  , rootEntry  9 [0, 5]   FlatSpelling    -- F maj 1st inv
  , rootEntry 10 [1, 6]   SharpSpelling   -- F# maj 1st inv
  , rootEntry 11 [2, 7]   SharpSpelling   -- G maj 1st inv
  , rootEntry  0 [3, 8]   FlatSpelling    -- Ab maj 1st inv
  , rootEntry  1 [4, 9]   SharpSpelling   -- A maj 1st inv
  , rootEntry  2 [5, 10]  FlatSpelling    -- Bb maj 1st inv
  , rootEntry  3 [6, 11]  SharpSpelling   -- B maj 1st inv
  -- 2nd inversion major
  , rootEntry  7 [0, 4]   FlatSpelling    -- C maj 2nd inv
  , rootEntry  8 [1, 5]   FlatSpelling    -- Db maj 2nd inv
  , rootEntry  9 [2, 6]   SharpSpelling   -- D maj 2nd inv
  , rootEntry 10 [3, 7]   FlatSpelling    -- Eb maj 2nd inv
  , rootEntry 11 [4, 8]   SharpSpelling   -- E maj 2nd inv
  , rootEntry  0 [5, 9]   FlatSpelling    -- F maj 2nd inv
  , rootEntry  1 [6, 10]  SharpSpelling   -- F# maj 2nd inv
  , rootEntry  2 [7, 11]  SharpSpelling   -- G maj 2nd inv
  , rootEntry  3 [8, 0]   FlatSpelling    -- Ab maj 2nd inv
  , rootEntry  4 [9, 1]   SharpSpelling   -- A maj 2nd inv
  , rootEntry  5 [10, 2]  FlatSpelling    -- Bb maj 2nd inv
  , rootEntry  6 [11, 3]  SharpSpelling   -- B maj 2nd inv
  -- Root position minor
  , rootEntry  0 [3, 7]   FlatSpelling    -- C min
  , rootEntry  1 [4, 8]   SharpSpelling   -- C# min
  , rootEntry  2 [5, 9]   FlatSpelling    -- D min
  , rootEntry  3 [6, 10]  FlatSpelling    -- Eb min
  , rootEntry  4 [7, 11]  SharpSpelling   -- E min
  , rootEntry  5 [8, 0]   FlatSpelling    -- F min
  , rootEntry  6 [9, 1]   SharpSpelling   -- F# min
  , rootEntry  7 [10, 2]  FlatSpelling    -- G min
  , rootEntry  8 [11, 3]  SharpSpelling   -- G# min
  , rootEntry  9 [0, 4]   FlatSpelling    -- A min
  , rootEntry 10 [1, 5]   FlatSpelling    -- Bb min
  , rootEntry 11 [2, 6]   SharpSpelling   -- B min
  -- 1st inversion minor
  , rootEntry  3 [7, 0]   FlatSpelling    -- C min 1st inv
  , rootEntry  4 [8, 1]   SharpSpelling   -- C# min 1st inv
  , rootEntry  5 [9, 2]   FlatSpelling    -- D min 1st inv
  , rootEntry  6 [10, 3]  FlatSpelling    -- Eb min 1st inv
  , rootEntry  7 [11, 4]  SharpSpelling   -- E min 1st inv
  , rootEntry  8 [0, 5]   FlatSpelling    -- F min 1st inv
  , rootEntry  9 [1, 6]   SharpSpelling   -- F# min 1st inv
  , rootEntry 10 [2, 7]   FlatSpelling    -- G min 1st inv
  , rootEntry 11 [3, 8]   SharpSpelling   -- G# min 1st inv
  , rootEntry  0 [4, 9]   FlatSpelling    -- A min 1st inv
  , rootEntry  1 [5, 10]  FlatSpelling    -- Bb min 1st inv
  , rootEntry  2 [6, 11]  SharpSpelling   -- B min 1st inv
  -- 2nd inversion minor
  , rootEntry  7 [0, 3]   FlatSpelling    -- C min 2nd inv
  , rootEntry  8 [1, 4]   SharpSpelling   -- C# min 2nd inv
  , rootEntry  9 [2, 5]   FlatSpelling    -- D min 2nd inv
  , rootEntry 10 [3, 6]   FlatSpelling    -- Eb min 2nd inv
  , rootEntry 11 [4, 7]   SharpSpelling   -- E min 2nd inv
  , rootEntry  0 [5, 8]   FlatSpelling    -- F min 2nd inv
  , rootEntry  1 [6, 9]   SharpSpelling   -- F# min 2nd inv
  , rootEntry  2 [7, 10]  FlatSpelling    -- G min 2nd inv
  , rootEntry  3 [8, 11]  SharpSpelling   -- G# min 2nd inv
  , rootEntry  4 [9, 0]   FlatSpelling    -- A min 2nd inv
  , rootEntry  5 [10, 1]  FlatSpelling    -- Bb min 2nd inv
  , rootEntry  6 [11, 2]  SharpSpelling   -- B min 2nd inv
  -- Root position sus4
  , rootEntry  0 [5, 7]   FlatSpelling    -- C sus4
  , rootEntry  1 [6, 8]   FlatSpelling    -- Db sus4
  , rootEntry  2 [7, 9]   SharpSpelling   -- D sus4
  , rootEntry  3 [8, 10]  FlatSpelling    -- Eb sus4
  , rootEntry  4 [9, 11]  SharpSpelling   -- E sus4
  , rootEntry  5 [10, 0]  FlatSpelling    -- F sus4
  , rootEntry  6 [11, 1]  SharpSpelling   -- F# sus4
  , rootEntry  7 [0, 2]   SharpSpelling   -- G sus4
  , rootEntry  8 [1, 3]   FlatSpelling    -- Ab sus4
  , rootEntry  9 [2, 4]   SharpSpelling   -- A sus4
  , rootEntry 10 [3, 5]   FlatSpelling    -- Bb sus4
  , rootEntry 11 [4, 6]   SharpSpelling   -- B sus4
  -- Root position sus2
  , rootEntry  5 [7, 0]   FlatSpelling    -- F sus2
  , rootEntry  6 [8, 1]   FlatSpelling    -- Gb sus2
  , rootEntry  7 [9, 2]   SharpSpelling   -- G sus2
  , rootEntry  8 [10, 3]  FlatSpelling    -- Ab sus2
  , rootEntry  9 [11, 4]  SharpSpelling   -- A sus2
  , rootEntry 10 [0, 5]   FlatSpelling    -- Bb sus2
  , rootEntry 11 [1, 6]   SharpSpelling   -- B sus2
  , rootEntry  0 [2, 7]   SharpSpelling   -- C sus2
  , rootEntry  1 [3, 8]   FlatSpelling    -- Db sus2
  , rootEntry  2 [4, 9]   SharpSpelling   -- D sus2
  , rootEntry  3 [5, 10]  FlatSpelling    -- Eb sus2
  , rootEntry  4 [6, 11]  SharpSpelling   -- E sus2
  -- Root position 7sus4no5
  , rootEntry  7 [0, 5]   FlatSpelling    -- G 7sus4no5
  , rootEntry  8 [1, 6]   FlatSpelling    -- Ab 7sus4no5
  , rootEntry  9 [2, 7]   SharpSpelling   -- A 7sus4no5
  , rootEntry 10 [3, 8]   FlatSpelling    -- Bb 7sus4no5
  , rootEntry 11 [4, 9]   SharpSpelling   -- B 7sus4no5
  , rootEntry  0 [5, 10]  FlatSpelling    -- C 7sus4no5
  , rootEntry  1 [6, 11]  SharpSpelling   -- Db 7sus4no5
  , rootEntry  2 [7, 0]   SharpSpelling   -- D 7sus4no5
  , rootEntry  3 [8, 1]   FlatSpelling    -- Eb 7sus4no5
  , rootEntry  4 [9, 2]   SharpSpelling   -- E 7sus4no5
  , rootEntry  5 [10, 3]  FlatSpelling    -- F 7sus4no5
  , rootEntry  6 [11, 4]  SharpSpelling   -- Gb 7sus4no5
  -- Root position dim
  , rootEntry  0 [3, 6]   FlatSpelling    -- C dim
  , rootEntry  1 [4, 7]   SharpSpelling   -- C# dim
  , rootEntry  2 [5, 8]   FlatSpelling    -- D dim
  , rootEntry  3 [6, 9]   SharpSpelling   -- Eb dim
  , rootEntry  4 [7, 10]  FlatSpelling    -- E dim
  , rootEntry  5 [8, 11]  FlatSpelling    -- F dim
  , rootEntry  6 [9, 0]   SharpSpelling   -- F# dim
  , rootEntry  7 [10, 1]  FlatSpelling    -- G dim
  , rootEntry  8 [11, 2]  SharpSpelling   -- G# dim
  , rootEntry  9 [0, 3]   FlatSpelling    -- A dim
  , rootEntry 10 [1, 4]   SharpSpelling   -- Bb dim
  , rootEntry 11 [2, 5]   FlatSpelling    -- B dim
  -- Root position aug
  , rootEntry  0 [4, 8]   FlatSpelling    -- C aug
  , rootEntry  1 [5, 9]   SharpSpelling   -- C# aug
  , rootEntry  2 [6, 10]  FlatSpelling    -- D aug
  , rootEntry  3 [7, 11]  SharpSpelling   -- Eb aug
  , rootEntry  4 [8, 0]   FlatSpelling    -- E aug
  , rootEntry  5 [9, 1]   FlatSpelling    -- F aug
  , rootEntry  6 [10, 2]  SharpSpelling   -- F# aug
  , rootEntry  7 [11, 3]  FlatSpelling    -- G aug
  , rootEntry  8 [0, 4]   SharpSpelling   -- G# aug
  , rootEntry  9 [1, 5]   FlatSpelling    -- A aug
  , rootEntry 10 [2, 6]   FlatSpelling    -- Bb aug
  , rootEntry 11 [3, 7]   SharpSpelling   -- B aug
  -- Root position maj7 omit 5
  , rootEntry  0 [4, 11]  FlatSpelling    -- C maj7o5
  , rootEntry  1 [5, 0]   FlatSpelling    -- Db maj7o5
  , rootEntry  2 [6, 1]   SharpSpelling   -- D maj7o5
  , rootEntry  3 [7, 2]   FlatSpelling    -- Eb maj7o5
  , rootEntry  4 [8, 3]   SharpSpelling   -- E maj7o5
  , rootEntry  5 [9, 4]   FlatSpelling    -- F maj7o5
  , rootEntry  6 [10, 5]  SharpSpelling   -- F# maj7o5
  , rootEntry  7 [11, 6]  SharpSpelling   -- G maj7o5
  , rootEntry  8 [0, 7]   FlatSpelling    -- Ab maj7o5
  , rootEntry  9 [1, 8]   SharpSpelling   -- A maj7o5
  , rootEntry 10 [2, 9]   FlatSpelling    -- Bb maj7o5
  , rootEntry 11 [3, 10]  SharpSpelling   -- B maj7o5
  -- 1st inversion maj7 omit 5
  , rootEntry  4 [11, 0]  FlatSpelling    -- C maj7o5 1st inv
  , rootEntry  5 [0, 1]   FlatSpelling    -- Db maj7o5 1st inv
  , rootEntry  6 [1, 2]   SharpSpelling   -- D maj7o5 1st inv
  , rootEntry  7 [2, 3]   FlatSpelling    -- Eb maj7o5 1st inv
  , rootEntry  8 [3, 4]   SharpSpelling   -- E maj7o5 1st inv
  , rootEntry  9 [4, 5]   FlatSpelling    -- F maj7o5 1st inv
  , rootEntry 10 [5, 6]   SharpSpelling   -- F# maj7o5 1st inv
  , rootEntry 11 [6, 7]   SharpSpelling   -- G maj7o5 1st inv
  , rootEntry  0 [7, 8]   FlatSpelling    -- Ab maj7o5 1st inv
  , rootEntry  1 [8, 9]   SharpSpelling   -- A maj7o5 1st inv
  , rootEntry  2 [9, 10]  FlatSpelling    -- Bb maj7o5 1st inv
  , rootEntry  3 [10, 11] SharpSpelling   -- B maj7o5 1st inv
  -- 2nd inversion maj7 omit 5
  , rootEntry 11 [0, 4]   FlatSpelling    -- C maj7o5 2nd inv
  , rootEntry  0 [1, 5]   FlatSpelling    -- Db maj7o5 2nd inv
  , rootEntry  1 [2, 6]   SharpSpelling   -- D maj7o5 2nd inv
  , rootEntry  2 [3, 7]   FlatSpelling    -- Eb maj7o5 2nd inv
  , rootEntry  3 [4, 8]   SharpSpelling   -- E maj7o5 2nd inv
  , rootEntry  4 [5, 9]   FlatSpelling    -- F maj7o5 2nd inv
  , rootEntry  5 [6, 10]  SharpSpelling   -- F# maj7o5 2nd inv
  , rootEntry  6 [7, 11]  SharpSpelling   -- G maj7o5 2nd inv
  , rootEntry  7 [8, 0]   FlatSpelling    -- Ab maj7o5 2nd inv
  , rootEntry  8 [9, 1]   SharpSpelling   -- A maj7o5 2nd inv
  , rootEntry  9 [10, 2]  FlatSpelling    -- Bb maj7o5 2nd inv
  , rootEntry 10 [11, 3]  SharpSpelling   -- B maj7o5 2nd inv
  -- Root position maj7 omit 3
  , rootEntry  0 [7, 11]  FlatSpelling    -- C maj7o3
  , rootEntry  1 [8, 0]   FlatSpelling    -- Db maj7o3
  , rootEntry  2 [9, 1]   SharpSpelling   -- D maj7o3
  , rootEntry  3 [10, 2]  FlatSpelling    -- Eb maj7o3
  , rootEntry  4 [11, 3]  SharpSpelling   -- E maj7o3
  , rootEntry  5 [0, 4]   FlatSpelling    -- F maj7o3
  , rootEntry  6 [1, 5]   SharpSpelling   -- F# maj7o3
  , rootEntry  7 [2, 6]   SharpSpelling   -- G maj7o3
  , rootEntry  8 [3, 7]   FlatSpelling    -- Ab maj7o3
  , rootEntry  9 [4, 8]   SharpSpelling   -- A maj7o3
  , rootEntry 10 [5, 9]   FlatSpelling    -- Bb maj7o3
  , rootEntry 11 [6, 10]  SharpSpelling   -- B maj7o3
  -- 1st inversion maj7 omit 3
  , rootEntry  7 [11, 0]  FlatSpelling    -- C maj7o3 1st inv
  , rootEntry  8 [0, 1]   FlatSpelling    -- Db maj7o3 1st inv
  , rootEntry  9 [1, 2]   SharpSpelling   -- D maj7o3 1st inv
  , rootEntry 10 [2, 3]   FlatSpelling    -- Eb maj7o3 1st inv
  , rootEntry 11 [3, 4]   SharpSpelling   -- E maj7o3 1st inv
  , rootEntry  0 [4, 5]   FlatSpelling    -- F maj7o3 1st inv
  , rootEntry  1 [5, 6]   SharpSpelling   -- F# maj7o3 1st inv
  , rootEntry  2 [6, 7]   SharpSpelling   -- G maj7o3 1st inv
  , rootEntry  3 [7, 8]   FlatSpelling    -- Ab maj7o3 1st inv
  , rootEntry  4 [8, 9]   SharpSpelling   -- A maj7o3 1st inv
  , rootEntry  5 [9, 10]  FlatSpelling    -- Bb maj7o3 1st inv
  , rootEntry  6 [10, 11] SharpSpelling   -- B maj7o3 1st inv
  -- 2nd inversion maj7 omit 3
  , rootEntry 11 [0, 7]   FlatSpelling    -- C maj7o3 2nd inv
  , rootEntry  0 [1, 8]   FlatSpelling    -- Db maj7o3 2nd inv
  , rootEntry  1 [2, 9]   SharpSpelling   -- D maj7o3 2nd inv
  , rootEntry  2 [3, 10]  FlatSpelling    -- Eb maj7o3 2nd inv
  , rootEntry  3 [4, 11]  SharpSpelling   -- E maj7o3 2nd inv
  , rootEntry  4 [5, 0]   FlatSpelling    -- F maj7o3 2nd inv
  , rootEntry  5 [6, 1]   SharpSpelling   -- F# maj7o3 2nd inv
  , rootEntry  6 [7, 2]   SharpSpelling   -- G maj7o3 2nd inv
  , rootEntry  7 [8, 3]   FlatSpelling    -- Ab maj7o3 2nd inv
  , rootEntry  8 [9, 4]   SharpSpelling   -- A maj7o3 2nd inv
  , rootEntry  9 [10, 5]  FlatSpelling    -- Bb maj7o3 2nd inv
  , rootEntry 10 [11, 6]  SharpSpelling   -- B maj7o3 2nd inv
  -- Root position min7 omit 5
  , rootEntry  0 [3, 10]  FlatSpelling    -- C min7o5
  , rootEntry  1 [4, 11]  SharpSpelling   -- C# min7o5
  , rootEntry  2 [5, 0]   FlatSpelling    -- D min7o5
  , rootEntry  3 [6, 1]   FlatSpelling    -- Eb min7o5
  , rootEntry  4 [7, 2]   SharpSpelling   -- E min7o5
  , rootEntry  5 [8, 3]   FlatSpelling    -- F min7o5
  , rootEntry  6 [9, 4]   SharpSpelling   -- F# min7o5
  , rootEntry  7 [10, 5]  FlatSpelling    -- G min7o5
  , rootEntry  8 [11, 6]  SharpSpelling   -- G# min7o5
  , rootEntry  9 [0, 7]   FlatSpelling    -- A min7o5
  , rootEntry 10 [1, 8]   FlatSpelling    -- Bb min7o5
  , rootEntry 11 [2, 9]   SharpSpelling   -- B min7o5
  -- 1st inversion min7 omit 5
  , rootEntry  3 [10, 0]  FlatSpelling    -- C min7o5 1st inv
  , rootEntry  4 [11, 1]  SharpSpelling   -- C# min7o5 1st inv
  , rootEntry  5 [0, 2]   FlatSpelling    -- D min7o5 1st inv
  , rootEntry  6 [1, 3]   FlatSpelling    -- Eb min7o5 1st inv
  , rootEntry  7 [2, 4]   SharpSpelling   -- E min7o5 1st inv
  , rootEntry  8 [3, 5]   FlatSpelling    -- F min7o5 1st inv
  , rootEntry  9 [4, 6]   SharpSpelling   -- F# min7o5 1st inv
  , rootEntry 10 [5, 7]   FlatSpelling    -- G min7o5 1st inv
  , rootEntry 11 [6, 8]   SharpSpelling   -- G# min7o5 1st inv
  , rootEntry  0 [7, 9]   FlatSpelling    -- A min7o5 1st inv
  , rootEntry  1 [8, 10]  FlatSpelling    -- Bb min7o5 1st inv
  , rootEntry  2 [9, 11]  SharpSpelling   -- B min7o5 1st inv
  -- 2nd inversion min7 omit 5
  , rootEntry 10 [0, 3]   FlatSpelling    -- C min7o5 2nd inv
  , rootEntry 11 [1, 4]   SharpSpelling   -- C# min7o5 2nd inv
  , rootEntry  0 [2, 5]   FlatSpelling    -- D min7o5 2nd inv
  , rootEntry  1 [3, 6]   FlatSpelling    -- Eb min7o5 2nd inv
  , rootEntry  2 [4, 7]   SharpSpelling   -- E min7o5 2nd inv
  , rootEntry  3 [5, 8]   FlatSpelling    -- F min7o5 2nd inv
  , rootEntry  4 [6, 9]   SharpSpelling   -- F# min7o5 2nd inv
  , rootEntry  5 [7, 10]  FlatSpelling    -- G min7o5 2nd inv
  , rootEntry  6 [8, 11]  SharpSpelling   -- G# min7o5 2nd inv
  , rootEntry  7 [9, 0]   FlatSpelling    -- A min7o5 2nd inv
  , rootEntry  8 [10, 1]  FlatSpelling    -- Bb min7o5 2nd inv
  , rootEntry  9 [11, 2]  SharpSpelling   -- B min7o5 2nd inv
  -- Root position min7 omit 3
  , rootEntry  0 [7, 10]  FlatSpelling    -- C min7o3
  , rootEntry  1 [8, 11]  SharpSpelling   -- C# min7o3
  , rootEntry  2 [9, 0]   FlatSpelling    -- D min7o3
  , rootEntry  3 [10, 1]  FlatSpelling    -- Eb min7o3
  , rootEntry  4 [11, 2]  SharpSpelling   -- E min7o3
  , rootEntry  5 [0, 3]   FlatSpelling    -- F min7o3
  , rootEntry  6 [1, 4]   SharpSpelling   -- F# min7o3
  , rootEntry  7 [2, 5]   FlatSpelling    -- G min7o3
  , rootEntry  8 [3, 6]   SharpSpelling   -- G# min7o3
  , rootEntry  9 [4, 7]   FlatSpelling    -- A min7o3
  , rootEntry 10 [5, 8]   FlatSpelling    -- Bb min7o3
  , rootEntry 11 [6, 9]   SharpSpelling   -- B min7o3
  -- 1st inversion min7 omit 3
  , rootEntry  7 [10, 0]  FlatSpelling    -- C min7o3 1st inv
  , rootEntry  8 [11, 1]  SharpSpelling   -- C# min7o3 1st inv
  , rootEntry  9 [0, 2]   FlatSpelling    -- D min7o3 1st inv
  , rootEntry 10 [1, 3]   FlatSpelling    -- Eb min7o3 1st inv
  , rootEntry 11 [2, 4]   SharpSpelling   -- E min7o3 1st inv
  , rootEntry  0 [3, 5]   FlatSpelling    -- F min7o3 1st inv
  , rootEntry  1 [4, 6]   SharpSpelling   -- F# min7o3 1st inv
  , rootEntry  2 [5, 7]   FlatSpelling    -- G min7o3 1st inv
  , rootEntry  3 [6, 8]   SharpSpelling   -- G# min7o3 1st inv
  , rootEntry  4 [7, 9]   FlatSpelling    -- A min7o3 1st inv
  , rootEntry  5 [8, 10]  FlatSpelling    -- Bb min7o3 1st inv
  , rootEntry  6 [9, 11]  SharpSpelling   -- B min7o3 1st inv
  -- 2nd inversion min7 omit 3
  , rootEntry 10 [0, 7]   FlatSpelling    -- C min7o3 2nd inv
  , rootEntry 11 [1, 8]   SharpSpelling   -- C# min7o3 2nd inv
  , rootEntry  0 [2, 9]   FlatSpelling    -- D min7o3 2nd inv
  , rootEntry  1 [3, 10]  FlatSpelling    -- Eb min7o3 2nd inv
  , rootEntry  2 [4, 11]  SharpSpelling   -- E min7o3 2nd inv
  , rootEntry  3 [5, 0]   FlatSpelling    -- F min7o3 2nd inv
  , rootEntry  4 [6, 1]   SharpSpelling   -- F# min7o3 2nd inv
  , rootEntry  5 [7, 2]   FlatSpelling    -- G min7o3 2nd inv
  , rootEntry  6 [8, 3]   SharpSpelling   -- G# min7o3 2nd inv
  , rootEntry  7 [9, 4]   FlatSpelling    -- A min7o3 2nd inv
  , rootEntry  8 [10, 5]  FlatSpelling    -- Bb min7o3 2nd inv
  , rootEntry  9 [11, 6]  SharpSpelling   -- B min7o3 2nd inv
  ]
  where
    rootEntry bass upper s = (bass, upper, s)
    expandRoot (bass, upper, s) = [(bass, [(IS.fromList upper, s)])]

-- Layer 2: 2-set rules (bass PC → [(set of required upper PC, spelling)])
-- Covers major and minor double stops as intermediate fallback.
layer2Table :: IM.IntMap [(IS.IntSet, EnharmonicSpelling)]
layer2Table = IM.fromListWith (++) $ concatMap expandRoot
  -- Major double stop (root + major 3rd)
  [ rootEntry  0 [4]   FlatSpelling    -- C maj
  , rootEntry  1 [5]   FlatSpelling    -- Db maj
  , rootEntry  2 [6]   SharpSpelling   -- D maj
  , rootEntry  3 [7]   FlatSpelling    -- Eb maj
  , rootEntry  4 [8]   SharpSpelling   -- E maj
  , rootEntry  5 [9]   FlatSpelling    -- F maj
  , rootEntry  6 [10]  SharpSpelling   -- F# maj
  , rootEntry  7 [11]  SharpSpelling   -- G maj
  , rootEntry  8 [0]   FlatSpelling    -- Ab maj
  , rootEntry  9 [1]   SharpSpelling   -- A maj
  , rootEntry 10 [2]   FlatSpelling    -- Bb maj
  , rootEntry 11 [3]   SharpSpelling   -- B maj
  -- Minor double stop (root + minor 3rd)
  , rootEntry  0 [3]   FlatSpelling    -- C min
  , rootEntry  1 [4]   SharpSpelling   -- C# min
  , rootEntry  2 [5]   FlatSpelling    -- D min
  , rootEntry  3 [6]   FlatSpelling    -- Eb min
  , rootEntry  4 [7]   SharpSpelling   -- E min
  , rootEntry  5 [8]   FlatSpelling    -- F min
  , rootEntry  6 [9]   SharpSpelling   -- F# min
  , rootEntry  7 [10]  FlatSpelling    -- G min
  , rootEntry  8 [11]  SharpSpelling   -- G# min
  , rootEntry  9 [0]   FlatSpelling    -- A min
  , rootEntry 10 [1]   FlatSpelling    -- Bb min
  , rootEntry 11 [2]   SharpSpelling   -- B min
  ]
  where
    rootEntry bass upper s = (bass, upper, s)
    expandRoot (bass, upper, s) = [(bass, [(IS.fromList upper, s)])]

-- |Detect if a NoteName carries an explicit sharp/flat preference.
-- Sharp variants (C', D', F', G', A') → Just SharpSpelling
-- Flat variants (Db, Eb, Gb, Ab, Bb) → Just FlatSpelling
-- Natural notes (C, D, E, F, G, A, B) → Nothing (use inference)
noteNameImpliesSpelling :: NoteName -> Maybe EnharmonicSpelling
noteNameImpliesSpelling C' = Just SharpSpelling
noteNameImpliesSpelling D' = Just SharpSpelling
noteNameImpliesSpelling F' = Just SharpSpelling
noteNameImpliesSpelling G' = Just SharpSpelling
noteNameImpliesSpelling A' = Just SharpSpelling
noteNameImpliesSpelling Db = Just FlatSpelling
noteNameImpliesSpelling Eb = Just FlatSpelling
noteNameImpliesSpelling Gb = Just FlatSpelling
noteNameImpliesSpelling Ab = Just FlatSpelling
noteNameImpliesSpelling Bb = Just FlatSpelling
noteNameImpliesSpelling _  = Nothing

-- |Patterns that are enharmonically ambiguous mid-progression.
-- Only min7o3 (root + P5 + m7) at roots where major/minor triads differ in spelling.
-- Roots 1 (Db/C#), 2 (D), 7 (G), 8 (Ab/G#), 9 (A): major 3rd → one spelling, minor 3rd → different.
-- Roots 0, 3, 4, 5, 6, 10, 11: same spelling regardless of 3rd → NOT ambiguous.
ambiguousPatterns :: IM.IntMap [IS.IntSet]
ambiguousPatterns = IM.fromListWith (++)
  [ (bass `mod` 12, [IS.fromList (map (`mod` 12) uppers)])
  | root <- [1, 2, 7, 8, 9]
  , (bass, uppers) <- min7o3Inversions root
  ]
  where
    min7o3Inversions r =
      [ (r,    [r+7, r+10])
      , (r+7,  [r+10, r])
      , (r+10, [r, r+7])
      ]

-- |Check if a chord's absolute pitches match an ambiguous pattern.
isAmbiguousPattern :: [Int] -> Bool
isAmbiguousPattern [] = False
isAmbiguousPattern pcs =
  let bassNorm = head pcs `mod` 12
      restSet = IS.fromList (map (`mod` 12) (tail pcs))
  in case IM.lookup bassNorm ambiguousPatterns of
       Nothing -> False
       Just patterns -> any (`IS.isSubsetOf` restSet) patterns

-------------------------------------------------------------------------------
-- Functionality (Chord Quality as String)
-------------------------------------------------------------------------------

-- |Chord quality/functionality as a string (e.g., "maj", "min7", "dim")
-- Preserved as String for compatibility with legacy naming conventions
type Functionality = String

-- |Derive functionality name from a pitch set using the legacy triad nameFunc
-- This is for TRIADS (exactly 3 pitch classes)
toFunctionality :: [PitchClass] -> Functionality
toFunctionality ps = nameFuncTriad zeroFormFn ps ""
  where zeroFormFn = id  -- We expect input already in zero form

-- |Derive functionality name from a pitch set using the chord nameFunc
-- This is for CHORDS (any number of pitch classes, extended harmonies)
toFunctionalityChord :: [PitchClass] -> Functionality
toFunctionalityChord ps = nameFuncChord zeroFormFn ps ""
  where zeroFormFn = id  -- We expect input already in zero form

-------------------------------------------------------------------------------
-- Movement (Bass Motion)
-------------------------------------------------------------------------------

-- |Movement represents bass motion by a musical interval
data Movement 
  = Asc PitchClass   -- ^ Ascending by n semitones (1-5)
  | Desc PitchClass  -- ^ Descending by n semitones (1-5)
  | Unison           -- ^ Pedal (no movement)
  | Tritone          -- ^ Movement by 6 semitones
  | Empty            -- ^ Placeholder for invalid/missing movement
  deriving (Ord, Eq, Generic)

instance Show Movement where
  show :: Movement -> String
  show (Asc n)  = "asc " ++ show (unPitchClass n)
  show (Desc n) = "desc " ++ show (unPitchClass n)
  show Unison   = "pedal"
  show Tritone  = "tritone"
  show Empty    = "empty"

instance Read Movement where
  readsPrec :: Int -> ReadS Movement
  readsPrec _ s
    | s == "pedal"   = [(Unison, "")]
    | s == "asc 1"   = [(Asc (P 1), "")]
    | s == "asc 2"   = [(Asc (P 2), "")]
    | s == "asc 3"   = [(Asc (P 3), "")]
    | s == "asc 4"   = [(Asc (P 4), "")]
    | s == "asc 5"   = [(Asc (P 5), "")]
    | s == "tritone" = [(Tritone, "")]
    | s == "desc 5"  = [(Desc (P 5), "")]
    | s == "desc 4"  = [(Desc (P 4), "")]
    | s == "desc 3"  = [(Desc (P 3), "")]
    | s == "desc 2"  = [(Desc (P 2), "")]
    | s == "desc 1"  = [(Desc (P 1), "")]
    | otherwise      = [(Empty, "")]

-- |Convert two pitch classes to a Movement (bass motion direction/distance)
-- Ported from legacy MusicData.hs toMovement
-- 
-- Movement direction is determined by the shorter path around the pitch class circle.
-- Example: C(0) to G(7) = descending by 5 (shorter path is 5 semitones down via P 12)
--          G(7) to C(0) = ascending by 5 (shorter path is 5 semitones up to P 12 ≡ P 0)
toMovement :: PitchClass -> PitchClass -> Movement
toMovement from to
  | x < y           = Asc (P x)    -- shorter path is forwards = ascending
  | y < x           = Desc (P y)   -- shorter path is backwards = descending
  | x == 0 && y == 0 = Unison
  | otherwise       = Tritone
  where
    x = last $ zeroFormLegacy [unPitchClass from, unPitchClass to]
    y = last $ zeroFormLegacy [unPitchClass to, unPitchClass from]
    -- Legacy zeroForm: subtract the FIRST element, then sort
    zeroFormLegacy (h:hs) = sort $ map (\n -> (n - h) `mod` 12) (h:hs)
    zeroFormLegacy []     = []

-- |Convert Movement back to PitchClass interval
fromMovement :: Movement -> PitchClass
fromMovement (Asc n)  = n
fromMovement (Desc n) = P (12 - unPitchClass n)
fromMovement Unison   = P 0
fromMovement Tritone  = P 6
fromMovement Empty    = P 0

-------------------------------------------------------------------------------
-- Abstract Types
-------------------------------------------------------------------------------

-- |A Chord is an abstract pitch structure: root note name, functionality, 
-- and intervals from root (bass first, as integers for register info).
data Chord = Chord 
  { chordNoteName     :: NoteName
  , chordFunctionality :: Functionality
  , chordIntervals    :: [Integer]
  } deriving (Eq, Ord, Generic)

instance Show Chord where
  show (Chord noteName functionality _)
    | functionality == "N/A" = show "N/A"
    | otherwise              = show noteName ++ "_" ++ functionality

-- |A Cadence is a movement type combined with a target chord quality.
-- Abstract: defines "approach by descending 5th to a major chord" without
-- specifying which root pitch.
data Cadence = Cadence 
  { cadenceFunctionality :: Functionality
  , cadenceMovement      :: Movement
  , cadenceIntervals     :: [PitchClass]
  } deriving (Eq, Ord, Generic)

instance Show Cadence where
  show (Cadence functionality mvmt _) =
    "( " ++ show mvmt ++ " -> " ++ functionality ++ " )"

-------------------------------------------------------------------------------
-- Concrete State Types
-------------------------------------------------------------------------------

-- |A ChordState is a Chord with a concrete root pitch (anchored in pitch space).
data ChordState = ChordState
  { stateChord :: Chord
  , stateRoot  :: NoteName
  } deriving (Eq, Generic)

instance Show ChordState where
  show (ChordState chord root) = show root ++ ": " ++ show chord

-- |A CadenceState is a Cadence with a concrete root pitch and enharmonic spelling.
-- This represents "descend by 5th to G major (flat spelling)" rather than abstract.
-- The enharmonic spelling ensures consistent note naming throughout a progression.
data CadenceState = CadenceState 
  { stateCadence     :: Cadence
  , stateCadenceRoot :: NoteName
  , stateSpelling    :: EnharmonicSpelling
  } deriving (Eq, Generic)

instance Show CadenceState where
  show (CadenceState cadence root _) =
    "( " ++ show (cadenceMovement cadence) ++ " -> " ++ 
    show root ++ "_" ++ cadenceFunctionality cadence ++ " )"

-------------------------------------------------------------------------------
-- nameFuncTriad: VERBATIM PORT from legacy MusicData.hs (lines 443-479)
-- This function implements the TRIAD naming conventions exactly.
-- Used for exactly 3-pitch structures with inversion detection.
-------------------------------------------------------------------------------

-- |Triad naming function ported VERBATIM from legacy MusicData.hs
-- Takes a form function, pitch classes, and accumulator string.
-- Returns the functionality string (e.g., "maj", "min", "dim", etc.)
nameFuncTriad :: ([PitchClass] -> [PitchClass]) -> [PitchClass] -> String -> String
nameFuncTriad f xs =
  let
    zs = fromIntegral . unPitchClass <$> f xs :: [Int]
    chain =
      [if (elem 4 zs && all (`notElem` zs) [3,10,11]) && notElem 8 zs
        then ("maj"++) else (""++)
      ,if (elem 3 zs && notElem 4 zs) && notElem 6 zs
        then ("min"++) else (""++)
      ,if elem 9 zs then ("6"++) else (""++)
      ,if elem 10 zs then ("7"++) else (""++)
      ,if elem 11 zs then ("maj7"++) else (""++)
      ,if all (`elem` zs) [7,8] then ("b13"++) else (""++)
      -- sus2: has 2, has 5th (7), no 4th (5), no 3rd (3,4)
      ,if elem 2 zs && notElem 5 zs && all (`notElem` zs) [3,4] && elem 7 zs
        then ("sus2"++) else (""++)
      -- sus4: has 4th (5), has 5th (7), no 3rd, AND not already sus2 (must have 5, not just 2)
      ,if elem 5 zs && notElem 2 zs && all (`notElem` zs) [3,4] && elem 7 zs
        then ("sus4"++) else (""++)
      ,if all (`elem` zs) [2,5] then ("sus2/4"++) else (""++)
      ,if notElem 5 zs && elem 2 zs && all (`notElem` zs) [3,4]
        && notElem 7 zs then ("sus2"++) else (""++)
      ,if notElem 2 zs && elem 5 zs && all (`notElem` zs) [3,4]
        && notElem 7 zs then ("sus4"++) else (""++)
      ,if all (`elem` zs) [2,3] || all (`elem` zs) [2,4]
        then ("add9"++) else (""++)
      ,if all (`elem` zs) [5,3] || all (`elem` zs) [5,4]
        then ("add11"++) else (""++)
      ,if elem 1 zs then ("b9"++) else (""++)
      ,if all (`elem` zs) [3,4] then ("#9"++) else (""++)
      ,if elem 6 zs && notElem 5 zs && any (`elem` zs) [7,8]
        then ("#11"++) else (""++)
      ,if ((elem 6 zs && notElem 7 zs) || (elem 6 zs && notElem 8 zs))
        && notElem 3 zs && all (`notElem` zs) [7,8]
        then ("b5"++) else (""++)
      ,if ((elem 8 zs && notElem 7 zs) || all (`elem` zs) [8,9])
        && notElem 4 zs then ("#5"++) else (""++)
      ,if all (`notElem` zs) [2,3,4,5] then ("no3"++) else (""++)
      ,if all (`notElem` zs) [6,7,8] then ("no5"++) else (""++)
      ,if all (`elem` zs) [3,6] then ("dim"++) else (""++)
      ,if all (`elem` zs) [4,8] then ("aug"++) else (""++)]
   in foldr (.) id chain

-------------------------------------------------------------------------------
-- nameFuncChord: VERBATIM PORT from legacy MusicData.hs (lines 906-940)
-- This function implements CHORD naming for extended harmonies (4+ pitches).
-- Does NOT reduce to triads - names the full pitch content.
-------------------------------------------------------------------------------

-- |Chord naming function ported VERBATIM from legacy MusicData.hs
-- Different from triad naming - handles extended harmonies (7ths, 9ths, etc.)
nameFuncChord :: ([PitchClass] -> [PitchClass]) -> [PitchClass] -> String -> String
nameFuncChord f xs =
  let
    zs = fromIntegral . unPitchClass <$> f xs :: [Int]
    chain =
      [(""++)
      ,if all (`elem` zs) [0,4,7] && all (`notElem` zs) [1,2,3,5,6,8,9,10,11] 
        then ("maj"++) else (""++)
      ,if elem 3 zs && all (`notElem` zs) [4,10] then ("m"++) else (""++)
      ,if all (`elem` zs) [3,10] && notElem 4 zs then ("m7"++) else (""++)
      ,if elem 9 zs then ("6"++) else (""++)
      ,if elem 10 zs && (notElem 3 zs || all (`elem` zs) [3,4]) then ("7"++) else (""++)
      ,if elem 11 zs then ("maj7"++) else (""++)
      ,if all (`elem` zs) [2,5] && all (`notElem` zs) [3,4] then ("sus2/4"++) else (""++)
      ,if notElem 5 zs && elem 2 zs && all (`notElem` zs) [3,4]
        then ("sus2"++) else (""++)
      ,if notElem 2 zs && elem 5 zs && all (`notElem` zs) [3,4]
        then ("sus4"++) else (""++)
      ,if ((elem 6 zs && notElem 7 zs) && notElem 5 zs) 
        || (elem 5 zs && elem 6 zs) then ("b5"++) else (""++)
      ,if ((elem 8 zs && notElem 7 zs) || all (`elem` zs) [8,9]) then ("#5"++) else (""++)
      ,if all (`elem` zs) [2,3,5] || all (`elem` zs) [2,4,5]
        then ("add9/11"++) else (""++)
      ,if notElem 5 zs && (all (`elem` zs) [2,3] || all (`elem` zs) [2,4])
        then ("add9"++) else (""++)
      ,if notElem 2 zs && (all (`elem` zs) [5,3] || all (`elem` zs) [5,4])
        then ("add11"++) else (""++)
      ,if (elem 6 zs && notElem 5 zs) && (elem 7 zs && notElem 8 zs) then ("#11"++) else (""++)
      ,if elem 1 zs then ("b9"++) else (""++)
      ,if all (`elem` zs) [3,4] then ("#9"++) else (""++)
      ,if all (`elem` zs) [7,8] then ("b13"++) else (""++)
      ,if all (`notElem` zs) [2,3,4,5] then ("no3"++) else (""++)
      ,if all (`notElem` zs) [6,7,8] then ("no5"++) else (""++)]
   in foldr (.) id chain

-------------------------------------------------------------------------------
-- Triad Construction Functions (exactly 3 pitch classes)
-- If input has >3 pitch classes, reduces via mostConsonant
-------------------------------------------------------------------------------

-- |Build a Triad from an enharmonic function and integer list
-- TRIADS: exactly 3 pitch classes. If >3 input, reduces to best triad.
-- Uses elaborate inversion detection from legacy MusicData.hs
toTriad :: (PitchClass -> NoteName) -> [Int] -> Chord
toTriad enharm ps@(fund:_)
  | length (pcSetInts ps) > 3 = toTriad enharm $ mostConsonant $ possibleTriadsSimple enharm fund (tail ps)
  | otherwise = 
    let normalizedPs = normalizeWithFund fund ps
        pcs = map mkPitchClass normalizedPs
        bass = mkPitchClass (head normalizedPs)
        invs = inversions pcs
        headInv = head invs
        -- Get root offset from inversion pattern
        rootOffset = case headInv of
          [P 0, P 4, P 7] -> 0   -- Root position major
          [P 0, P 3, P 7] -> 0   -- Root position minor
          [P 0, P 3, P 8] -> 8   -- 1st inv major
          [P 0, P 4, P 9] -> 9   -- 1st inv minor
          [P 0, P 5, P 9] -> 5   -- 2nd inv major
          [P 0, P 5, P 8] -> 5   -- 2nd inv minor
          [P 0, P 3, P 6] -> 0   -- Root position dim
          [P 0, P 6, P 9] -> 6   -- 2nd inv dim
          [P 0, P 3, P 9] -> 9   -- 1st inv dim
          _ -> 0                 -- Fallback (includes all root position chords)
        rootPC = bass + P rootOffset
        -- Compute root-relative intervals for naming
        rootRelativePCs = sort $ map (\p -> p - rootPC) pcs
        functionality = nameFuncTriad zeroFormPC rootRelativePCs ""
        -- Get root name and inversion suffix
        invResult = detectInversion enharm normalizedPs
    in Chord (fst invResult) (functionality ++ snd invResult) (map fromIntegral ps)
  where
    pcSetInts xs = List.nub $ sort $ map (`mod` 12) xs
toTriad _ [] = Chord C "N/A" []

-- |Shortcut with flat spelling for triads
flatTriad :: [Int] -> Chord
flatTriad = toTriad flat

-- |Shortcut with sharp spelling for triads
sharpTriad :: [Int] -> Chord
sharpTriad = toTriad sharp

-------------------------------------------------------------------------------
-- Chord Construction Functions (preserves all pitch classes)
-- Does NOT reduce to triads - names the full pitch content
-------------------------------------------------------------------------------

-- |Build a Chord from an enharmonic function and integer list
-- CHORDS: preserves all pitches, uses chord-specific naming for extended harmonies
-- Ported from legacy MusicData.hs toChord (lines 896-941)
toChord :: (PitchClass -> NoteName) -> [Int] -> Chord
toChord enharm ps@(fund:tones) = 
  let chord = (+fund) <$> (sortedZeroForm $ fund : (reverse $ sort tones))
      functionality = nameFuncChord zeroFormPC (map mkPitchClass chord) ""
  in Chord (enharm $ mkPitchClass (head chord)) functionality (map fromIntegral $ map (`mod` 12) chord)
  where
    sortedZeroForm xs = 
      let m = minimum xs 
      in sort $ map (\n -> (n - m) `mod` 12) xs
toChord enharm [] = Chord (enharm (P 0)) "N/A" []

-- |Shortcut with flat spelling for chords
flatChord :: [Int] -> Chord
flatChord = toChord flat

-- |Shortcut with sharp spelling for chords
sharpChord :: [Int] -> Chord
sharpChord = toChord sharp

-- |Convert a pair of Chords to a Cadence
toCadence :: (Chord, Chord) -> Cadence
toCadence (fromChord, toChord') =
  let fromRoot = pitchClass (chordNoteName fromChord)
      toRoot = pitchClass (chordNoteName toChord')
      mvmt = toMovement fromRoot toRoot
      toIntervals = map mkPitchClass $ take 3 $ map fromIntegral (chordIntervals toChord')
  in Cadence (chordFunctionality toChord') mvmt (zeroFormPC toIntervals)

-------------------------------------------------------------------------------
-- State Conversion
-------------------------------------------------------------------------------

-- |Convert ChordState to the underlying Chord
fromChordState :: ChordState -> Chord
fromChordState = stateChord

-- |Convert CadenceState back to a Chord (applying the root and spelling)
fromCadenceState :: CadenceState -> Chord
fromCadenceState (CadenceState cadence root spelling) =
  let rootPC = pitchClass root
      enharm = enharmonicFunc spelling
      tones = cadenceIntervals cadence
      pitches = map (\t -> unPitchClass (t + rootPC)) tones
  in toTriad enharm pitches

-- |Initialize a CadenceState from movement, note name, and quality intervals.
-- Enharmonic spelling is inferred from the chord's absolute pitch content.
initCadenceState :: Int -> String -> [Int] -> CadenceState
initCadenceState movement note quality =
  let approach = toMovement (P 0) (mkPitchClass movement)
      from = flatTriad [0]
      toIntervals = map (+ unPitchClass (fromMovement approach)) $ zeroFormInts quality
      to = flatTriad toIntervals
      root = readNoteName note
      cad = toCadence (from, to)
      -- Infer spelling from absolute pitches
      rootPC = pitchClass root
      tones = cadenceIntervals cad
      absolutePitches = map (\t -> unPitchClass (t + rootPC)) tones
      inferredSpelling = inferSpelling absolutePitches
      spelling = case noteNameImpliesSpelling root of
        Just explicit -> explicit
        Nothing       -> inferredSpelling
  in CadenceState cad root spelling
  where
    zeroFormInts xs = let m = minimum xs in sort $ map (\n -> (n - m) `mod` 12) xs

-------------------------------------------------------------------------------
-- Tracing (for maximum verbosity diagnostics)
-------------------------------------------------------------------------------

-- |Trace of toTriad transformation steps
data ToTriadTrace = ToTriadTrace
  { tttRawDbIntervals   :: String    -- ^ Raw zero-form intervals from DB: "[P 0,P 4,P 7]"
  , tttRawDbMovement    :: String    -- ^ Raw movement from DB: "desc 3"
  , tttRawDbFunctionality :: String  -- ^ Raw stored functionality from DB: "maj"
  , tttRootPC           :: Int       -- ^ Root pitch class (0-11)
  , tttRootNoteName     :: String    -- ^ Root note name before transform
  , tttTones            :: [Int]     -- ^ Raw cadence intervals as Ints before transposition
  , tttTransposedPitches:: [Int]     -- ^ Pitches after adding rootPC to tones (STEP 2)
  , tttNormalizedPs     :: [Int]     -- ^ Result of normalizeWithFund (STEP 3)
  , tttZeroForm         :: [Int]     -- ^ Result of zeroFormPC on normalized pitches (STEP 4)
  , tttDetectedRoot     :: String    -- ^ Root from detectInversion
  , tttFunctionality    :: String    -- ^ Result of nameFuncTriad
  , tttFinalChord       :: String    -- ^ Final rendered chord (root + functionality)
  , tttStoredFunc       :: String    -- ^ Original functionality stored in cadence
  } deriving (Show, Eq)

-- |Convert CadenceState to Chord with full transformation trace.
-- Used for maximum verbosity diagnostics (gen''). Includes raw DB data plus all transformation stages.
fromCadenceStateTraced :: CadenceState -> (Chord, ToTriadTrace)
fromCadenceStateTraced (CadenceState cadence root spelling) =
  let rootPC = pitchClass root
      rootPCInt = unPitchClass rootPC
      enharm = enharmonicFunc spelling
      tones = cadenceIntervals cadence
      tonesInts = map unPitchClass tones
      pitches = map (\t -> unPitchClass (t + rootPC)) tones
      
      -- Trace toTriad internals (matching toTriad logic for correct functionality)
      fund = head pitches
      normalizedPs = normalizeWithFund fund pitches
      pcs = map mkPitchClass normalizedPs
      bass = mkPitchClass (head normalizedPs)
      invs = inversions pcs
      headInv = head invs
      -- Get root offset from inversion pattern (same as toTriad lines 452-465)
      rootOffset = case headInv of
        [P 0, P 4, P 7] -> 0   -- Root position major
        [P 0, P 3, P 7] -> 0   -- Root position minor
        [P 0, P 3, P 8] -> 8   -- 1st inv major
        [P 0, P 4, P 9] -> 9   -- 1st inv minor
        [P 0, P 5, P 9] -> 5   -- 2nd inv major
        [P 0, P 5, P 8] -> 5   -- 2nd inv minor
        [P 0, P 3, P 6] -> 0   -- Root position dim
        [P 0, P 6, P 9] -> 6   -- 2nd inv dim
        [P 0, P 3, P 9] -> 9   -- 1st inv dim
        _ -> 0                 -- Fallback (includes all root position chords)
      rootPC_computed = bass + P rootOffset
      -- Compute root-relative intervals for naming (same as toTriad line 468)
      rootRelativePCs = sort $ map (\p -> p - rootPC_computed) pcs
      zeroForm = map unPitchClass $ zeroFormPC pcs
      invResult = detectInversion enharm normalizedPs
      functionality = nameFuncTriad zeroFormPC rootRelativePCs ""
      finalChord = Chord (fst invResult) (functionality ++ snd invResult) (map fromIntegral pitches)
      
      -- Raw DB data
      dbIntervals = show tones
      dbMovement = show (cadenceMovement cadence)
      dbFunctionality = cadenceFunctionality cadence
      
      trace = ToTriadTrace
        { tttRawDbIntervals = dbIntervals
        , tttRawDbMovement = dbMovement
        , tttRawDbFunctionality = dbFunctionality
        , tttRootPC = rootPCInt
        , tttRootNoteName = show root
        , tttTones = tonesInts
        , tttTransposedPitches = pitches
        , tttNormalizedPs = normalizedPs
        , tttZeroForm = zeroForm
        , tttDetectedRoot = show (fst invResult)
        , tttFunctionality = functionality
        , tttFinalChord = show (fst invResult) ++ " " ++ functionality ++ snd invResult
        , tttStoredFunc = dbFunctionality
        }
  in (finalChord, trace)

-------------------------------------------------------------------------------
-- DB Serialization (compatible with Neo4j format)
-------------------------------------------------------------------------------

-- |Construct a Cadence from DB text format (movement string, chord string)
-- 
-- Example:
--   constructCadence ("desc 3", "[P 0,P 3,P 8]")
--   => Cadence { cadenceFunctionality = "maj_1stInv", cadenceMovement = Desc (P 3), ... }
constructCadence :: (String, String) -> Cadence
constructCadence (movementStr, chordStr) =
  let pitches = read chordStr :: [PitchClass]
      functionality = toFunctionality pitches
      movement = read movementStr :: Movement
  in Cadence functionality movement pitches

-- |Deconstruct a Cadence to DB text format (movement, pitch class list)
-- Returns the Movement and [PitchClass] for serialization
deconstructCadence :: Cadence -> (Movement, [PitchClass])
deconstructCadence (Cadence _ mvmt intervals) = (mvmt, intervals)

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- |Extract the root note from a Chord
rootNote :: Chord -> NoteName
rootNote = chordNoteName

-- |Generate all cyclic inversions of a pitch set (in zero form)
-- Each inversion is transformed to zero form for proper comparison
inversions :: [PitchClass] -> [[PitchClass]]
inversions ps = map (zeroFormPC . map (P . fromIntegral)) $ simpleRotations (map unPitchClass ps)
  where
    simpleRotations xs = take (length xs) $ iterate rotate xs
    rotate ys = tail ys ++ [head ys]

-- |Normalize a pitch set to its most compact form
-- Ported from legacy MusicData.hs normalForm
normalForm :: [PitchClass] -> [PitchClass]
normalForm xs
  | length pitches == 1 = [P 0]
  | length pitches == 2 = head $ filterByLast $ invs
  | length pitches == 3 = head $ filterBySecondLast $ filterByLast invs
  | otherwise           = head $ filterBySecondLast $ filterByLast invs
  where
    pitches = List.nub xs
    invs = inversions pitches
    -- Convert to integers for comparison
    toInts ys = map unPitchClass ys
    filterByLast xss = filter (\x -> last (toInts x) == minimum (map (last . toInts) xss)) xss
    filterBySecondLast xss = 
      let penultimate ys = if length ys >= 2 then toInts ys !! (length ys - 2) else 0
      in filter (\x -> penultimate x == minimum (map penultimate xss)) xss

-- |Compute prime form (transposition and inversion invariant)
-- Ported from legacy MusicData.hs primeForm
primeForm :: [PitchClass] -> [PitchClass]
primeForm xs = map P $ primeInts
  where
    -- Get normal form as integers
    cmpt = map unPitchClass $ normalForm xs
    -- Invert: 12 - each element, then get normal form of that
    inverted = map (\n -> (12 - n) `mod` 12) cmpt
    invCmpt = map unPitchClass $ normalForm (map P inverted)
    -- Pick the one with smaller sum
    primeInts = head $ sortBy (compare `on` sum) [cmpt, invCmpt]

-------------------------------------------------------------------------------
-- Internal Helpers
-------------------------------------------------------------------------------

-- |Read a note name from string
readNoteName :: String -> NoteName
readNoteName "C"  = C
readNoteName "C#" = C'
readNoteName "Db" = Db
readNoteName "D"  = D
readNoteName "D#" = D'
readNoteName "Eb" = Eb
readNoteName "E"  = E
readNoteName "F"  = F
readNoteName "F#" = F'
readNoteName "Gb" = Gb
readNoteName "G"  = G
readNoteName "G#" = G'
readNoteName "Ab" = Ab
readNoteName "A"  = A
readNoteName "A#" = A'
readNoteName "Bb" = Bb
readNoteName "B"  = B
readNoteName s    = error $ "readNoteName: unknown note name: " ++ show s

-- Note: enharmFromNoteName is imported from Harmonic.Core.Pitch

-- |Zero-form for pitch classes
-- Subtracts the FIRST element (not minimum) then sorts, matching legacy behavior
-- |Normalize pitch class set to zero form by subtracting first element then sorting.
-- Examples:
--   [P 4, P 7, P 11] → [P 0, P 3, P 7]  (maj7 in root position)
--   [P 8, P 11, P 3] → [P 0, P 3, P 7]  (maj7 in 2nd inversion)
-- Used by all Cadence construction paths (DB, toCadence, fallback) for semantic consistency.
zeroFormPC :: [PitchClass] -> [PitchClass]
zeroFormPC [] = []
zeroFormPC xs@(P first:_) = 
  sort $ map (\(P n) -> P ((n - first) `mod` 12)) xs

-- |Normalize pitches with respect to a fundamental
normalizeWithFund :: Int -> [Int] -> [Int]
normalizeWithFund fund ps = 
  let ps' = map (`mod` 12) ps
      adjusted = fund : sort (filter (/= fund `mod` 12) ps')
  in adjusted

-- |Simple possible triads generation (for chord construction)
possibleTriadsSimple :: (PitchClass -> NoteName) -> Int -> [Int] -> [[Int]]
possibleTriadsSimple _ fund ps =
  let fundPC = fund `mod` 12
      otherPCs = List.nub $ filter (/= fundPC) $ map (`mod` 12) ps
      pairs = [[a, b] | a <- otherPCs, b <- otherPCs, a < b]
  in map (fundPC :) pairs

-- |Select most consonant option from a list of pitch sets.
-- Inline replica of Dissonance.mostConsonant (Layer B cannot import Layer C).
-- Includes perfect fifth bonus (-1) and degenerate set penalty (27).
mostConsonant :: [[Int]] -> [Int]
mostConsonant [] = [0, 4, 7]  -- Default to major triad
mostConsonant xs = snd . head . sortBy (compare `on` fst) $ map dissonanceLevel xs
  where
    dissonanceLevel ys
      | countElem iVect 0 == 5 = (27 :: Int, ys)
      | hasPerfectFifth ys     = (baseDiss - 1, ys)
      | otherwise              = (baseDiss, ys)
      where
        iVect    = intervalVector ys
        baseDiss = sum $ zipWith (*) [16,8,4,2,1,24] iVect
    intervalVector ys = [countInterval i ys | i <- [1..6]]
    countInterval i ys = length [() | a <- ys, b <- ys, a < b, ((b - a) `mod` 12) `elem` [i, 12-i]]
    countElem xs' x = length (filter (== x) xs')
    hasPerfectFifth [] = False
    hasPerfectFifth (root:rest) =
      let p5 = (root + 7) `mod` 12
      in any (\x -> x `mod` 12 == p5) rest

-- |Detect chord inversion by pattern matching against known inversion forms
-- Returns (root_note, inversion_suffix) where suffix is "", "_1stInv", or "_2ndInv"
-- Uses mathematical computation of root instead of index-based lookup
detectInversion :: (PitchClass -> NoteName) -> [Int] -> (NoteName, String)
detectInversion enharm xs
  | null xs = (enharm (P 0), "")
  | length xs < 3 = (enharm (mkPitchClass (head xs)), "")
  | otherwise =
    let pcs = map mkPitchClass xs
        bass = mkPitchClass (head xs)
        invs = inversions pcs
        headInv = head invs
        -- Compute root from bass + interval offset based on pattern
        rootFromOffset offset = enharm (bass + P offset)
    in case headInv of
      -- Root position major: root is bass
      [P 0, P 4, P 7] -> (rootFromOffset 0, "")
      -- Root position minor: root is bass
      [P 0, P 3, P 7] -> (rootFromOffset 0, "")
      -- 1st inversion major: [0,3,8] means root is 8 semitones above bass
      [P 0, P 3, P 8] -> (rootFromOffset 8, "_1stInv")
      -- 1st inversion minor: [0,4,9] means root is 9 semitones above bass
      [P 0, P 4, P 9] -> (rootFromOffset 9, "_1stInv")
      -- 2nd inversion major: [0,5,9] means root is 5 semitones above bass
      [P 0, P 5, P 9] -> (rootFromOffset 5, "_2ndInv")
      -- 2nd inversion minor: [0,5,8] means root is 5 semitones above bass
      [P 0, P 5, P 8] -> (rootFromOffset 5, "_2ndInv")
      -- Root position dim
      [P 0, P 3, P 6] -> (rootFromOffset 0, "")
      -- 2nd inversion dim: root is 6 semitones above bass
      [P 0, P 6, P 9] -> (rootFromOffset 6, "_2ndInv")
      -- 1st inversion dim: root is 9 semitones above bass
      [P 0, P 3, P 9] -> (rootFromOffset 9, "_1stInv")
      -- Fallback: assume root position
      _ -> (rootFromOffset 0, "")

-- |Check if a cadence is an inversion (1st or 2nd) based on zero-form intervals.
-- Patterns match those in 'detectInversion'.
isInversion :: Cadence -> Bool
isInversion cad = case cadenceIntervals cad of
  [P 0, P 3, P 8]  -> True  -- 1st inversion major
  [P 0, P 4, P 9]  -> True  -- 1st inversion minor
  [P 0, P 5, P 9]  -> True  -- 2nd inversion major
  [P 0, P 5, P 8]  -> True  -- 2nd inversion minor
  [P 0, P 6, P 9]  -> True  -- 2nd inversion dim
  [P 0, P 3, P 9]  -> True  -- 1st inversion dim
  _                 -> False
