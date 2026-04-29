-- |
-- Module      : Harmonic.Rules.Types.ProgressionContext
-- Description : Three-layer progression value (triad / strata / mode) for the genP paradigm
--
-- A 'ProgressionContext' bundles three bar-aligned 'Progression' layers —
-- triads, pentatonic strata, and diatonic modes — together with optional
-- per-bar provenance tracking which tristrata and strata each bar was drawn
-- from. The legacy 'gen' paradigm produces contexts where all three layers
-- duplicate the same triad progression and provenance is 'Nothing'; the new
-- 'genP' paradigm (introduced by the octatripentatonic framework) produces
-- contexts with distinct strata/mode layers and 'Just' provenance.

module Harmonic.Rules.Types.ProgressionContext
  ( Layer(..)
  , ProgressionContext(..)
  , layer
  , pcLength
  , fromProgression
  , liftPC
  ) where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Harmonic.Rules.Types.Progression (Progression(..), progLength)
import Harmonic.Rules.Types.Scale (Tristrata, StrataLabel)

-- |Layer tag selecting one of the three progression layers at a call site.
--
-- * 'T' — Triad layer (cadential harmony, the Rules.Types.Progression result).
-- * 'S' — Strata layer (5-note pentatonic chroma, one stratum per bar).
-- * 'M' — Mode layer (7-note diatonic chroma, the pair-union mode).
--
-- @M@ replaces the plan's @D@ to avoid clashing with 'Harmonic.Rules.Types.Pitch.NoteName'
-- (@D@ natural), which is re-exported via 'Harmonic.Lib' for live-coding use.
data Layer = T | S | M
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- |Three bar-aligned progression layers with optional per-bar provenance.
--
-- Invariant: @progLength triadLayer == progLength strataLayer == progLength modeLayer@,
-- and when @pcProvenance = Just seq@, @Seq.length seq == progLength triadLayer@.
data ProgressionContext = ProgressionContext
  { triadLayer   :: Progression
  , strataLayer  :: Progression
  , modeLayer    :: Progression
  , pcProvenance :: Maybe (Seq (Tristrata, StrataLabel))
  } deriving (Eq)

-- Source-compatible with the legacy 'Progression' display; verbose triadic layout only.
instance Show ProgressionContext where
  show = show . triadLayer

-- |Project a 'ProgressionContext' to a single 'Progression' by layer tag.
layer :: Layer -> ProgressionContext -> Progression
layer T = triadLayer
layer S = strataLayer
layer M = modeLayer

-- |Bar count — equal across all three layers by invariant.
pcLength :: ProgressionContext -> Int
pcLength = progLength . triadLayer

-- |Wrap a single 'Progression' as a 'ProgressionContext' by duplicating it
-- into all three layers with no provenance. Used by the legacy 'gen' paradigm
-- and by test fixtures migrating to the widened type.
fromProgression :: Progression -> ProgressionContext
fromProgression p = ProgressionContext
  { triadLayer   = p
  , strataLayer  = p
  , modeLayer    = p
  , pcProvenance = Nothing
  }

-- |Apply a 'Progression'-transforming function pointwise across all three
-- layers. Drops provenance — specific Class 1 combinators that preserve bar
-- alignment should permute provenance directly rather than going through
-- 'liftPC'.
liftPC :: (Progression -> Progression) -> ProgressionContext -> ProgressionContext
liftPC f pc = ProgressionContext
  { triadLayer   = f (triadLayer pc)
  , strataLayer  = f (strataLayer pc)
  , modeLayer    = f (modeLayer pc)
  , pcProvenance = Nothing
  }

instance Semigroup ProgressionContext where
  a <> b = ProgressionContext
    { triadLayer   = triadLayer  a <> triadLayer  b
    , strataLayer  = strataLayer a <> strataLayer b
    , modeLayer    = modeLayer   a <> modeLayer   b
    , pcProvenance = case (pcProvenance a, pcProvenance b) of
                       (Just sa, Just sb) -> Just (sa Seq.>< sb)
                       _                  -> Nothing
    }

instance Monoid ProgressionContext where
  mempty = ProgressionContext mempty mempty mempty Nothing
