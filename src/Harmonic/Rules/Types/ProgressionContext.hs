-- |
-- Module      : Harmonic.Rules.Types.ProgressionContext
-- Description : Three-layer progression value (triad \/ strata \/ mode) for the genP paradigm
--
-- A 'ProgressionContext' bundles three bar-aligned 'Progression' layers —
-- triads, pentatonic strata, and diatonic modes — together with optional
-- per-bar provenance tracking which tristrata and strata each bar was drawn
-- from. The legacy 'Harmonic.Framework.Builder.gen' paradigm produces contexts where all three layers
-- duplicate the same triad progression and provenance is 'Nothing'; the new
-- 'Harmonic.Framework.Builder.genP' paradigm (introduced by the octatripentatonic framework) produces
-- contexts with distinct strata\/mode layers and 'Just' provenance.

module Harmonic.Rules.Types.ProgressionContext
  ( Layer(..)
  , Family(..)
  , ProgressionContext(..)
  , layer
  , pcLength
  , fromProgression
  , liftPC
  , pcSplice
  ) where

import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

import qualified Harmonic.Rules.Types.Progression as Prog
import Harmonic.Rules.Types.Progression (Progression(..), progLength)
import qualified Harmonic.Rules.Types.Harmony as H
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

-- |Generation family a progression belongs to. Families never mix:
-- regeneration ('Harmonic.Framework.Builder.genFrom') always reproduces
-- the source's family, and @fuse@ of differing families downgrades to
-- 'FTriad'. Family drives dispatch in @genFrom@ and in the walking-bass
-- interface: 'FJazz' contexts walk with a per-bar bass vocabulary
-- (restored fifths, passing extensions, avoid-tone protection), 'FStrata'
-- walks via provenance chroma, and everything else — including
-- downgraded mixes — walks the plain per-bar tone sets at whatever
-- cardinality each bar carries.
--
-- * 'FTriad'    — plain triadic walk ('Harmonic.Framework.Builder.gen').
-- * 'FExtended' — uniform 4-note fusion family ('Harmonic.Framework.Builder.genE').
-- * 'FStrata'   — strata-first family ('Harmonic.Framework.Builder.genP'\/@genI@; carries provenance).
-- * 'FJazz'     — jazz Change-graph walk ('Harmonic.Framework.Builder.genJ'; variable arity 3-6).
data Family = FTriad | FExtended | FStrata | FJazz
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- |Three bar-aligned progression layers with optional per-bar provenance.
--
-- Invariant: @progLength triadLayer == progLength strataLayer == progLength modeLayer@,
-- and when @pcProvenance = Just sq@, @Seq.length sq == progLength triadLayer@.
data ProgressionContext = ProgressionContext
  { triadLayer   :: Progression
  , strataLayer  :: Progression
  , modeLayer    :: Progression
  , pcProvenance :: Maybe (Seq (Tristrata, StrataLabel))
  , pcFamily     :: Family
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
-- into all three layers with no provenance. Used by the legacy 'Harmonic.Framework.Builder.gen' paradigm
-- and by test fixtures migrating to the widened type.
fromProgression :: Progression -> ProgressionContext
fromProgression p = ProgressionContext
  { triadLayer   = p
  , strataLayer  = p
  , modeLayer    = p
  , pcProvenance = Nothing
  , pcFamily     = inferFamilyFromProgression p
  }

-- |Cardinality-based family inference for progressions arriving without
-- an explicit stamp (hand-built, deserialised, or legacy callers):
-- uniform 4-note bars are 'FExtended'; anything else 'FTriad'. Strata
-- and jazz producers stamp 'pcFamily' explicitly instead.
inferFamilyFromProgression :: Progression -> Family
inferFamilyFromProgression p =
  let sizes = [ length (H.cadenceIntervals (H.stateCadence cs))
              | cs <- toList (Prog.unProgression p) ]
  in if not (null sizes) && all (== 4) sizes then FExtended else FTriad

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
  , pcFamily     = pcFamily pc
  }

instance Semigroup ProgressionContext where
  a <> b = ProgressionContext
    { triadLayer   = triadLayer  a <> triadLayer  b
    , strataLayer  = strataLayer a <> strataLayer b
    , modeLayer    = modeLayer   a <> modeLayer   b
    , pcProvenance = case (pcProvenance a, pcProvenance b) of
                       (Just sa, Just sb) -> Just (sa Seq.>< sb)
                       _                  -> Nothing
    , pcFamily     = if pcFamily a == pcFamily b then pcFamily a else FTriad
    }

instance Monoid ProgressionContext where
  mempty = ProgressionContext mempty mempty mempty Nothing FTriad

-- |Splice a range of bars within a 'ProgressionContext', replacing the
-- triad \/ strata \/ mode layers and the 'pcProvenance' sequence in lockstep.
--
-- Range is 1-indexed and wrap-aware (mirrors 'Prog.spliceProgression'):
--
-- * Non-wrapping (@start <= end@): replaces positions @start..end@.
-- * Wrapping (@start > end@): replaces @start..N@ and @1..end@.
--
-- The triad layer's movement seam is fixed via 'Prog.spliceProgression';
-- strata \/ mode layers don't carry meaningful Movement state (their
-- 'Harmonic.Rules.Types.Harmony.Cadence' is built with @Movement = Unison@ — see @mkChromaCS@ in
-- 'Harmonic.Framework.Builder') so they use a plain sequence splice.
--
-- Provenance follows the same geometry. When either side has
-- @pcProvenance = Nothing@, the result is 'Nothing' (the splice can't
-- reconstruct provenance from a layer-only source).
--
-- Caller is responsible for ensuring the inserted 'ProgressionContext'
-- has length equal to the range size.
pcSplice :: ProgressionContext -> Int -> Int -> ProgressionContext -> ProgressionContext
pcSplice src start end ins =
  let triad' = Prog.spliceProgression
                 (triadLayer src) start end
                 (toList (Prog.unProgression (triadLayer ins)))
      strata' = Progression
                  (spliceSeq (Prog.unProgression (strataLayer src))
                             start end
                             (Prog.unProgression (strataLayer ins)))
      mode'   = Progression
                  (spliceSeq (Prog.unProgression (modeLayer src))
                             start end
                             (Prog.unProgression (modeLayer ins)))
      prov'   = case (pcProvenance src, pcProvenance ins) of
                  (Just s, Just i) -> Just (spliceSeq s start end i)
                  _                -> Nothing
  in ProgressionContext triad' strata' mode' prov' (pcFamily src)

-- |Plain sequence splice with 1-indexed wrap-aware semantics. Mirrors the
-- geometry of 'Prog.spliceProgression' but without movement-fix
-- (used for non-cadence layers and the provenance sequence).
spliceSeq :: Seq a -> Int -> Int -> Seq a -> Seq a
spliceSeq sq start end ins =
  let n = Seq.length sq
  in if start <= end then
       -- Non-wrapping
       let prefix = Seq.take (start - 1) sq
           suffix = Seq.drop end sq
       in prefix >< ins >< suffix
     else
       -- Wrapping: replaced = [start..N] ++ [1..end], kept = [end+1..start-1]
       let kept       = Seq.take (start - end - 1) (Seq.drop end sq)
           headCount  = n - start + 1
           newAtEnd   = Seq.take headCount ins
           newAtStart = Seq.drop headCount ins
       in newAtStart >< kept >< newAtEnd
