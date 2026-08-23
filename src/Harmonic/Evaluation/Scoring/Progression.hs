-- |
-- Module      : Harmonic.Evaluation.Scoring.Progression
-- Description : Whole-progression scoring for rank-and-select generation
--
-- Composable score of a 'PC.ProgressionContext' along four axes:
--
--   * 'psRootMotion'   — per-edge root motion smoothness (Hindemith-derived)
--   * 'psVoiceLeading' — cyclic voice-leading cost over the triad layer
--   * 'psCadenceFav'   — Neo4j-backed cadence transition favourability
--                        (0.0 if computed offline; online callers override)
--   * 'psModeValidity' — fraction of bars whose mode layer carries the
--                        expected 7-PC chroma (Phase 1 invariant guarantees
--                        walk-generated progressions score 1.0)
--
-- Each component lives in @[0, 1]@ with higher = better. 'totalScore'
-- combines components via a weighted sum; 'defaultWeights' makes
-- cadence-favourability the dominant axis (the user's stated preference).
--
-- The normalisations applied to per-component raw measurements are
-- intentionally simple placeholders — to be refined from observed
-- distributions once multi-attempt generation is live (see the project's
-- data-driven tuning memory). The function signatures are stable; only the
-- internal transforms inside @scoreRootMotion@ \/ @scoreVoiceLeading@ \/
-- @scoreModeValidity@ are subject to retuning.
module Harmonic.Evaluation.Scoring.Progression
  ( -- * Score record
    ProgressionScore(..)
  , ProgressionScoreWeights(..)
    -- * Default weights
  , defaultWeights
  , defaultWeightsOffline
    -- * Scoring (offline)
  , scoreProgression
  , totalScore
    -- * Cadence-favourability (online + pure helper)
  , TransitionMap
  , cadenceFavFromMap
  , scoreProgressionOnline
  , computeCadenceFav
  ) where

import           Control.Monad (forM)
import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.List (nub, sort)

import qualified Database.Bolt as Bolt

import qualified Harmonic.Rules.Types.Progression as Prog
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as P
import qualified Harmonic.Evaluation.Scoring.Dissonance as D
import qualified Harmonic.Evaluation.Scoring.VoiceLeading as VL
import qualified Harmonic.Evaluation.Database.Query as Q

-------------------------------------------------------------------------------
-- Score record
-------------------------------------------------------------------------------

-- |Per-progression scoring breakdown. Each component is in @[0, 1]@; higher
-- is better.
data ProgressionScore = ProgressionScore
  { psRootMotion   :: !Double
  , psVoiceLeading :: !Double
  , psCadenceFav   :: !Double
  , psModeValidity :: !Double
  } deriving (Show, Eq)

-- |Weights applied to each scoring axis in 'totalScore'. Conventionally
-- sum to 1.0 so the resulting total stays in @[0, 1]@.
data ProgressionScoreWeights = ProgressionScoreWeights
  { wRootMotion   :: !Double
  , wVoiceLeading :: !Double
  , wCadenceFav   :: !Double
  , wModeValidity :: !Double
  } deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Default weights
-------------------------------------------------------------------------------

-- |Cadence-favourability dominant: @0.4@ fav, @0.2@ each on root motion,
-- voice leading, mode validity.
defaultWeights :: ProgressionScoreWeights
defaultWeights = ProgressionScoreWeights
  { wRootMotion   = 0.2
  , wVoiceLeading = 0.2
  , wCadenceFav   = 0.4
  , wModeValidity = 0.2
  }

-- |Offline-mode weights — cadence favourability dropped, remaining three
-- renormalised to @1\/3@ each.
defaultWeightsOffline :: ProgressionScoreWeights
defaultWeightsOffline = ProgressionScoreWeights
  { wRootMotion   = 1 / 3
  , wVoiceLeading = 1 / 3
  , wCadenceFav   = 0
  , wModeValidity = 1 / 3
  }

-------------------------------------------------------------------------------
-- Scoring
-------------------------------------------------------------------------------

-- |Pure score: computes root-motion, voice-leading, and mode-validity
-- components. 'psCadenceFav' is left at @0.0@ — online callers override
-- this field after consulting the graph.
scoreProgression :: PC.ProgressionContext -> ProgressionScore
scoreProgression pc = ProgressionScore
  { psRootMotion   = scoreRootMotion   (PC.triadLayer pc)
  , psVoiceLeading = scoreVoiceLeading (PC.triadLayer pc)
  , psCadenceFav   = 0.0
  , psModeValidity = scoreModeValidity pc
  }

-- |Average per-edge root-motion score, cyclic (includes wrap-around from
-- last to first bar), mapped from Hindemith penalty space @[1, 6]@ to
-- @[0, 1]@ where 1 = perfect (every edge a P4 or P5).
scoreRootMotion :: Prog.Progression -> Double
scoreRootMotion prog =
  let states  = toList (Prog.unProgression prog)
      n       = length states
  in if n < 2 then 1.0
     else
       let rootPC s = P.unPitchClass (P.pitchClass (H.stateCadenceRoot s))
           rootPCs  = map rootPC states
           edges    = zip rootPCs (drop 1 rootPCs) ++ [(last rootPCs, head rootPCs)]
           rawSum   = sum [ fromIntegral (D.rootMotionScore ((b - a) `mod` 12))
                          | (a, b) <- edges
                          ] :: Double
           avgRaw   = rawSum / fromIntegral (length edges)
       in clamp01 ((6.0 - avgRaw) / 5.0)

-- |Cyclic voice-leading cost over an HONEST voicing extraction (sorted
-- absolute PCs per bar, full cardinality), linearly mapped from per-edge
-- cost @[vlLowAnchorCal, vlHighAnchorCal]@ to score @[1, 0]@.
--
-- Anchor calibration (2026-08-20, data-driven): 36-sample online probe —
-- gen \/ gen4 \/ genVI × len {8, 16} × entropy {0.2, 0.4, 0.6}, two runs
-- each (genVI cued in-strata; its random-cue empties excluded). Observed
-- per-edge cyclic costs: gen 2.5–5.75, gen4 3.25–12.0, genVI 3.0–7.1;
-- combined p10 ≈ 3.0, p90 ≈ 7.1. Anchors set at 3.0 (excellent) \/ 8.0
-- (poor) — p90 plus margin, so only genuinely rough runs bottom out.
-- (The previous 10\/30 anchors were calibrated against the old
-- unsorted\/mod-12-wrapped measurement artefact and do not transfer.)
vlLowAnchorCal, vlHighAnchorCal :: Double
vlLowAnchorCal  = 3.0
vlHighAnchorCal = 8.0

scoreVoiceLeading :: Prog.Progression -> Double
scoreVoiceLeading prog
  -- Fewer than two bars: no edges exist to measure — explicit neutral
  -- score (previously an accidental perfect 1.0 via the clamp).
  | length states < 2 = 0.5
  | otherwise =
      let voicings = map honestVoicing states
          n        = max 1 (length voicings)
          cost     = fromIntegral (VL.cyclicCost voicings) :: Double
          perEdge  = cost / fromIntegral n
      in clamp01 ((vlHighAnchor - perEdge) / (vlHighAnchor - vlLowAnchor))
  where
    states = toList (Prog.unProgression prog)
    -- Honest per-bar extraction: sorted absolute PCs at full cardinality.
    -- Replaces 'Prog.literalVoicing', which returned UNSORTED,
    -- mod-12-wrapped pseudo-voicings via the toTriad reduction (an
    -- A-rooted [0,4,7] bar read as [9,1,4], so C→A root motion measured
    -- 9 semitones instead of 3 — the old 10\/30 anchors were calibrated
    -- against that artefact). gen4 chains now score the heard 4-note
    -- surface; mixed-cardinality bars score real alignment costs.
    honestVoicing cs =
      let r = P.unPitchClass (P.pitchClass (H.stateCadenceRoot cs))
      in sort [ (P.unPitchClass i + r) `mod` 12
              | i <- H.cadenceIntervals (H.stateCadence cs) ]
    -- Anchors recalibrated 2026-08-20 against the honest measurement:
    -- online probe gen\/genVI\/gen4 × len 8\/16 × entropy {0.2,0.4,0.6},
    -- per-edge cyclic costs of sorted absolute-PC voicings. See the
    -- calibration values below (updated by the probe in the VL pass).
    vlLowAnchor  = vlLowAnchorCal
    vlHighAnchor = vlHighAnchorCal

-- |Fraction of bars whose mode-layer cardinality is 7 (i.e. 'Harmonic.Rules.Types.Scale.ModeOk' shape).
--
-- For walk-generated 'Harmonic.Framework.Builder.genP' contexts (pcProvenance = Just) the Phase 1
-- invariant guarantees @1.0@. For legacy 'Harmonic.Framework.Builder.gen' contexts (pcProvenance =
-- Nothing) the mode layer duplicates the triad layer (3 PCs), so this
-- check is not meaningful — returns @1.0@.
scoreModeValidity :: PC.ProgressionContext -> Double
scoreModeValidity pc =
  case PC.pcProvenance pc of
    Nothing -> 1.0
    Just _  ->
      let states = toList (Prog.unProgression (PC.modeLayer pc))
          n      = length states
      in if n == 0 then 1.0
         else let ok = length [ () | s <- states
                                   , length (H.cadenceIntervals (H.stateCadence s)) == 7
                                   ]
              in fromIntegral ok / fromIntegral n

-- |Weighted sum of components. Conventional weights sum to 1.0 → total
-- in @[0, 1]@.
totalScore :: ProgressionScoreWeights -> ProgressionScore -> Double
totalScore w ps =
    wRootMotion   w * psRootMotion   ps
  + wVoiceLeading w * psVoiceLeading ps
  + wCadenceFav   w * psCadenceFav   ps
  + wModeValidity w * psModeValidity ps

-------------------------------------------------------------------------------
-- Cadence-favourability aggregation (pure helper for Phase 5)
-------------------------------------------------------------------------------

-- |A pre-fetched, composer-blend-resolved map from source cadence (keyed by
-- its 'show' representation) to its outgoing transitions. Each transition
-- carries the destination 'Harmonic.Rules.Types.Harmony.Cadence' and the blended weight (output of
-- 'Query.applyComposerBlend').
type TransitionMap = Map Text [(H.Cadence, Double)]

-- |Compute 'psCadenceFav' from a pre-fetched transition map. Pure — no IO.
--
-- The progression is treated as a /cyclic/ loop: edges are
-- @(C₀ → C₁), …, (C_{N-2} → C_{N-1}), (C_{N-1} → C₀)@. Each edge is
-- scored by @edgeScore@ (hybrid presence + share). The per-progression
-- score is the mean of per-edge scores — length-independent and in
-- @[0, 1]@.
--
-- Matching by 'show' (not 'Eq') matches the DB's identity convention
-- (@MATCH (c:Cadence {show: $show})@ at @Query.hs:113-137@). The DB-side
-- 'Harmonic.Rules.Types.Harmony.Cadence' is reconstructed via @constructCadence (movement, chord)@,
-- which may differ from a generated 'Harmonic.Rules.Types.Harmony.Cadence's @cadenceIntervals@ field;
-- the 'show' instance projects to @(movement, functionality)@ only.
cadenceFavFromMap :: TransitionMap -> Prog.Progression -> Double
cadenceFavFromMap srcMap prog =
  -- Project each bar through the gen4 walk shadow ('walkTriadCadence',
  -- identity for triads) so 4-note chains score the same corpus edges the
  -- walk actually followed; without this a fused chain's keys miss the
  -- map entirely and psCadenceFav collapses to 0.
  let cads = map (H.walkTriadCadence . H.stateCadence)
                 (toList (Prog.unProgression prog))
      n = length cads
  in if n < 2 then 0
     else
       let edges = zip cads (drop 1 cads ++ [head cads])
           perEdge = map (edgeScore srcMap) edges
       in sum perEdge / fromIntegral (length perEdge)

-- |Per-edge favourability — hybrid of corpus presence and within-source share.
--
-- Returns @0@ when:
--   * the source cadence isn't in the corpus (e.g. fallback-generated), OR
--   * the source is in the corpus but the destination doesn't appear in its
--     outgoing transitions under the active composer blend.
--
-- Returns @0.5 + 0.5 * (w_dst \/ totalW)@ otherwise — i.e. the edge always
-- earns @0.5@ for being /present/ in the corpus, plus up to a further
-- @0.5@ proportional to its empirical share among the source's outgoing
-- transitions.
--
-- Rationale (from the data probe): pure per-source-prior values cluster
-- at @[0.005, 0.13]@ for typical corpus-rooted progressions because each
-- source has many valid outgoing transitions, so any single one carries
-- a low empirical probability. With the @0.4@ weight on 'psCadenceFav',
-- that compressed range neutralises the axis — the weighted contribution
-- becomes vanishingly small. The hybrid rewards /presence/ (the
-- progression follows a path the corpus has actually walked under the
-- chosen blend) plus a smaller share-of-source signal for commonness.
edgeScore :: TransitionMap -> (H.Cadence, H.Cadence) -> Double
edgeScore srcMap (src, dst) =
  let srcKey = T.pack (show src)
      dstKey = T.pack (show dst)
  in case Map.lookup srcKey srcMap of
       Nothing           -> 0
       Just transitions  ->
         let totalW   = sum (map snd transitions)
             matched  = sum [ w | (c, w) <- transitions
                                , T.pack (show c) == dstKey ]
         in if totalW <= 0 || matched <= 0
              then 0
              else 0.5 + 0.5 * (matched / totalW)

-------------------------------------------------------------------------------
-- Online scoring (Neo4j-backed)
-------------------------------------------------------------------------------

-- |Online variant of 'scoreProgression'. Pure components match the offline
-- version exactly; 'psCadenceFav' is populated from Neo4j edge weights
-- under the composer blend parsed from the supplied seek string.
--
-- Runs inside 'Bolt.BoltActionT IO' so the caller controls connection
-- lifecycle (typically a single shared pipe across a multi-attempt loop).
scoreProgressionOnline
  :: Text                          -- ^ Seek string (composer blend; same format as @_gcSeek@).
  -> PC.ProgressionContext
  -> Bolt.BoltActionT IO ProgressionScore
scoreProgressionOnline seekStr pc = do
  let basePure = scoreProgression pc
  cf <- computeCadenceFav seekStr (PC.triadLayer pc)
  pure basePure { psCadenceFav = cf }

-- |Cyclic per-edge favourability mean, computed against Neo4j. Builds the
-- 'TransitionMap' by fetching each unique source cadence's outgoing
-- transitions once, applying the composer blend, then delegating to the
-- pure 'cadenceFavFromMap'.
--
-- Number of graph queries = number of distinct source-cadence 'show' keys
-- in the progression (≤ N for an N-bar progression).
computeCadenceFav
  :: Text
  -> Prog.Progression
  -> Bolt.BoltActionT IO Double
computeCadenceFav seekStr prog = do
  -- Walk-shadow projection: see 'cadenceFavFromMap'.
  let cads      = map (H.walkTriadCadence . H.stateCadence) (toList (Prog.unProgression prog))
      srcKeys   = nub (map (T.pack . show) cads)
      blend     = Q.parseComposerWeights seekStr
  pairs <- forM srcKeys $ \k -> do
    raw <- Q.fetchTransitions k
    let resolved = Q.resolveWeights blend raw   -- [(Cadence, Double)]
    pure (k, resolved)
  let srcMap = Map.fromList pairs
  pure (cadenceFavFromMap srcMap prog)

-------------------------------------------------------------------------------
-- Internal helpers
-------------------------------------------------------------------------------

clamp01 :: Double -> Double
clamp01 x
  | x < 0     = 0
  | x > 1     = 1
  | otherwise = x
