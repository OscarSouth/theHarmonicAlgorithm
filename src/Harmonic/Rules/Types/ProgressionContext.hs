-- |
-- Module      : Harmonic.Rules.Types.ProgressionContext
-- Description : Three-layer progression value (triad \/ strata \/ mode) for the genP paradigm
--
-- A 'ProgressionContext' bundles three bar-aligned 'Progression' layers —
-- triads, pentatonic strata, and diatonic modes — together with optional
-- per-bar provenance tracking which tristrata and strata each bar was drawn
-- from. Every family fills the layers: 'Harmonic.Framework.Builder.genP' walks curated strata\/mode
-- chroma and carries 'Just' provenance; 'Harmonic.Framework.Builder.genE' fills partner triads; plain
-- 'Harmonic.Framework.Builder.gen' and 'Harmonic.Framework.Builder.genJ' derive a pentatonic S and a mode M by chordscale
-- key-area analysis ('Harmonic.Evaluation.Analysis.KeyArea.chordscale'),
-- provenance 'Nothing'. Only raw 'fromProgression' output (hand-built
-- material not passed through @chordscale@) still duplicates one
-- progression across all three layers.

module Harmonic.Rules.Types.ProgressionContext
  ( Layer(..)
  , Family(..)
  , ProgressionContext(..)
  , layer
  , pcLength
  , fromProgression
  , liftPC
  , liftPCAligned
  , liftPCSubst
  , normalizeFamily
  , pcSplice
  ) where

import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.List (transpose)

import qualified Harmonic.Rules.Types.Progression as Prog
import Harmonic.Rules.Types.Progression (Progression(..), progLength)
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as P
import Harmonic.Rules.Types.Scale (Tristrata, StrataLabel)

-- |Layer tag selecting one of the three progression layers — or a combination
-- of them — at a call site.
--
-- Single layers:
--
-- * 'T' — Triad layer (cadential harmony, the Rules.Types.Progression result).
-- * 'S' — Strata layer (genP: 5-note pentatonic chroma; genE: the less
--   dissonant partner triad).
-- * 'M' — Mode layer (genP: 7-note diatonic chroma; genE: the more dissonant
--   partner triad).
--
-- Combinations (synthesized per bar by 'layer'):
--
-- * 'TS' \/ 'TM' \/ 'SM' — pointwise pitch-class union of two layers.
-- * 'TSM' — union of all three.
-- * 'PT' — pivot tones: the pitch classes common to ALL three layers.
--
-- A merged bar is rooted on its LOWEST constituent layer's bar root
-- (T before S before M) — the foundation owns the bass whenever it is
-- present. On a polytonal (genE) context TS\/TM always union to 4 tones,
-- TSM to 5, and PT holds 2 tones on common-dyad bars or the single hub
-- tone on base-anchored bars. On a chordscale-derived gen\/genJ context
-- S is the bar's pentatonic, M its mode, and PT the chord tones the
-- pentatonic keeps. On contexts whose layers still duplicate one
-- progression (hand-built material without
-- 'Harmonic.Evaluation.Analysis.KeyArea.chordscale') every combination
-- degrades to that progression.
--
-- @M@ replaces the plan's @D@ to avoid clashing with 'Harmonic.Rules.Types.Pitch.NoteName'
-- (@D@ natural), which is re-exported via 'Harmonic.Lib' for live-coding use.
data Layer = T | S | M | TS | TM | SM | TSM | PT
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
-- * 'FExtended' — uniform 4-note hand-built material ('Harmonic.Interface.Tidal.Arranger.lead'' cues,
--   hand-spliced progressions); arrives only by inference, never stamped.
-- * 'FStrata'   — strata-first family ('Harmonic.Framework.Builder.genP'\/@genI@; carries provenance).
-- * 'FJazz'     — jazz Change-graph walk ('Harmonic.Framework.Builder.genJ'; variable arity 3-6).
-- * 'FPoly'     — polytonal three-triad family ('Harmonic.Framework.Builder.genE'):
--   T carries the foundation walk, S\/M carry partner triad chains sharing
--   tones with it per bar. Stamped explicitly by the producer; inference
--   never yields it.
data Family = FTriad | FExtended | FStrata | FJazz | FPoly
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
-- Single tags project a stored layer; combination tags synthesize per-bar
-- unions (rooted on the lowest constituent layer's bar — T before S before
-- M, so the foundation owns the merged bass whenever present); 'PT'
-- synthesizes the per-bar intersection of all three layers. Total for every
-- family; on duplicated-layer contexts (the raw 'fromProgression' fallback)
-- every combination collapses to the stored progression.
layer :: Layer -> ProgressionContext -> Progression
layer T   = triadLayer
layer S   = strataLayer
layer M   = modeLayer
layer TS  = mergeLayers [triadLayer, strataLayer]
layer TM  = mergeLayers [triadLayer, modeLayer]
layer SM  = mergeLayers [strataLayer, modeLayer]
layer TSM = mergeLayers [triadLayer, strataLayer, modeLayer]
layer PT  = pivotLayer

-- Absolute pitch classes sounded by one bar.
barAbsPCs :: H.CadenceState -> [Int]
barAbsPCs cs =
  let r = P.unPitchClass (P.pitchClass (H.stateCadenceRoot cs))
  in [ (r + P.unPitchClass iv) `mod` 12
     | iv <- H.cadenceIntervals (H.stateCadence cs) ]

-- Rebuild a bar over a given anchor bar's root, movement and spelling from
-- an absolute pitch-class set. 'H.mkCadenceStatePCs' keeps the zero-form
-- invariant (sorted, deduped, 0 re-inserted) at any cardinality.
barFromPCs :: H.CadenceState -> [Int] -> H.CadenceState
barFromPCs anchor pcs =
  let rootPC = P.unPitchClass (P.pitchClass (H.stateCadenceRoot anchor))
      ivs    = [ (p - rootPC) `mod` 12 | p <- pcs ]
      built  = H.mkCadenceStatePCs (H.stateCadenceRoot anchor)
                 (H.cadenceMovement (H.stateCadence anchor)) ivs
  in built { H.stateSpelling = H.stateSpelling anchor }

-- Pointwise union of the selected layers; the first selector is the lowest
-- layer and anchors each merged bar.
mergeLayers :: [ProgressionContext -> Progression] -> ProgressionContext -> Progression
mergeLayers sels pc =
  let cols = [ toList (Prog.unProgression (sel pc)) | sel <- sels ]
      mergeBar bars = case bars of
        (anchor : _) -> barFromPCs anchor (foldr (unionPCs . barAbsPCs) [] bars)
        []           -> error "mergeLayers: empty bar column"
      unionPCs xs ys = foldr (\x acc -> if x `elem` acc then acc else x : acc) ys xs
  in Prog.fromCadenceStates (map mergeBar (transpose cols))

-- Per-bar intersection of all three layers. Bars whose intersection equals
-- the T bar's own set pass the T bar through unchanged (the duplicated-layer
-- degrade); on chordscale-derived gen\/genJ bars the intersection is the
-- chord tones the pentatonic keeps — an anchor-tone selection. Otherwise
-- the surviving tones are rooted by fifth orientation —
-- a dyad roots on the tone whose partner sits within a tritone above it, a
-- single hub tone roots on itself.
pivotLayer :: ProgressionContext -> Progression
pivotLayer pc =
  let ts = Prog.unProgression (triadLayer pc)
      ss = Prog.unProgression (strataLayer pc)
      ms = Prog.unProgression (modeLayer pc)
      pivotBar tBar sBar mBar =
        let common = [ p | p <- barAbsPCs tBar
                         , p `elem` barAbsPCs sBar, p `elem` barAbsPCs mBar ]
        in if length common == length (barAbsPCs tBar)
             then tBar
             else barFromPCs (anchorOn tBar common) common
      anchorOn tBar common =
        let root = case common of
              [a, b] -> if (b - a) `mod` 12 <= 6 then a else b
              [a]    -> a
              _      -> P.unPitchClass (P.pitchClass (H.stateCadenceRoot tBar))
        in tBar { H.stateCadenceRoot =
                    H.enharmonicFunc (H.stateSpelling tBar) (P.mkPitchClass root) }
  in Progression (Seq.zipWith3 pivotBar ts ss ms)

-- |Bar count — equal across all three layers by invariant.
pcLength :: ProgressionContext -> Int
pcLength = progLength . triadLayer

-- |Wrap a single 'Progression' as a 'ProgressionContext' by duplicating it
-- into all three layers with no provenance — the RAW constructor. The gen
-- and genJ producers post-process the result with
-- 'Harmonic.Evaluation.Analysis.KeyArea.chordscale' to derive real S\/M
-- layers; hand-built material stays duplicated until passed through the
-- same combinator.
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
-- uniformly extended bars (every bar 4+ tones) are 'FExtended';
-- anything else 'FTriad'. Strata
-- and jazz producers stamp 'pcFamily' explicitly instead.
inferFamilyFromProgression :: Progression -> Family
inferFamilyFromProgression p =
  let sizes = [ length (H.cadenceIntervals (H.stateCadence cs))
              | cs <- toList (Prog.unProgression p) ]
  in if not (null sizes) && all (>= 4) sizes then FExtended else FTriad

-- |Apply a 'Progression'-transforming function pointwise across all three
-- layers. Drops provenance, and with it any 'FStrata' claim — a strata
-- context without provenance is no longer strata material, so the family
-- normalizes in step. Bar-alignment-preserving combinators should use
-- 'liftPCAligned' instead, which carries provenance (and the family)
-- through.
liftPC :: (Progression -> Progression) -> ProgressionContext -> ProgressionContext
liftPC f pc = normalizeFamily ProgressionContext
  { triadLayer   = f (triadLayer pc)
  , strataLayer  = f (strataLayer pc)
  , modeLayer    = f (modeLayer pc)
  , pcProvenance = Nothing
  , pcFamily     = pcFamily pc
  }

-- |Class 1 combinator support: apply ONE bar-order permutation\/replication
-- to all three layers AND the provenance sequence in lockstep. The rank-2
-- argument cannot inspect elements (parametricity), so it can only
-- rearrange or duplicate whole bars — per-bar pairings (bar ↔ provenance,
-- foundation ↔ partners) survive intact and the family stamp stays true.
-- Chordscale-derived S\/M layers (gen \/ genJ) permute with their bars, so
-- each bar keeps its own mode\/pentatonic; the key BOUNDARIES they encode
-- reflect the pre-permutation whole-progression analysis — re-apply
-- 'Harmonic.Evaluation.Analysis.KeyArea.chordscale' for a fresh reading.
liftPCAligned :: (forall a. Seq a -> Seq a) -> ProgressionContext -> ProgressionContext
liftPCAligned f pc = ProgressionContext
  { triadLayer   = onProg (triadLayer pc)
  , strataLayer  = onProg (strataLayer pc)
  , modeLayer    = onProg (modeLayer pc)
  , pcProvenance = fmap f (pcProvenance pc)
  , pcFamily     = pcFamily pc
  }
  where onProg = Progression . f . Prog.unProgression

-- |Bar substitution breaks both the provenance-backed ('FStrata') and the
-- structural ('FPoly') family invariants — the result walks and voices as
-- plain material, and the tag must not claim otherwise. Chordscale-derived
-- S\/M layers are mapped like any other bars and go stale the same way —
-- re-apply 'Harmonic.Evaluation.Analysis.KeyArea.chordscale' after
-- bar-substituting edits.
liftPCSubst :: (Progression -> Progression) -> ProgressionContext -> ProgressionContext
liftPCSubst f pc =
  let out = liftPC f pc   -- already normalized (provenance-less FStrata → FTriad)
  in out { pcFamily = case pcFamily out of
                        FPoly -> FTriad
                        fam   -> fam }

-- |'FStrata' is meaningless without the provenance that defines it; the
-- normalized form keeps 'Harmonic.Framework.Builder.genFrom' dispatch and
-- voicing routing honest. Invariant established type-wide:
-- @pcFamily == FStrata ⟹ pcProvenance == Just _@.
normalizeFamily :: ProgressionContext -> ProgressionContext
normalizeFamily pc
  | pcFamily pc == FStrata, Nothing <- pcProvenance pc = pc { pcFamily = FTriad }
  | otherwise = pc

instance Semigroup ProgressionContext where
  a <> b
    -- The empty context is a true identity: fuse of a singleton or pair
    -- must not meet-downgrade against mempty (Monoid law; Seq's Eq
    -- rejects on length in O(1) so the check is cheap).
    | a == mempty = b
    | b == mempty = a
    | otherwise = normalizeFamily ProgressionContext
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
