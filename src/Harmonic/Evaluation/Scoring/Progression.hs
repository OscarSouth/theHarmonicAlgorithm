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
-- internal transforms inside 'scoreRootMotion' / 'scoreVoiceLeading' /
-- 'scoreModeValidity' are subject to retuning.
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
import           Data.List (nub)

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
-- renormalised to @1/3@ each.
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

-- |Cyclic voice-leading cost over a literal-voicing extraction, linearly
-- mapped from per-edge cost @[vlLowAnchor, vlHighAnchor]@ to score
-- @[1, 0]@.
--
-- The anchors are calibrated against observed per-edge costs across
-- 'genVI' (natural-walk, strata-constrained, ~11–25) and 'gen' (legacy,
-- root-motion-prioritised, ~22–29) on length-8 / length-16 progressions
-- at varying entropies. See the data-driven tuning memory.
scoreVoiceLeading :: Prog.Progression -> Double
scoreVoiceLeading prog =
  let voicings = Prog.literalVoicing prog
      n        = max 1 (length voicings)
      cost     = fromIntegral (VL.cyclicCost voicings) :: Double
      perEdge  = cost / fromIntegral n
  in clamp01 ((vlHighAnchor - perEdge) / (vlHighAnchor - vlLowAnchor))
  where
    vlLowAnchor  = 10.0  -- per-edge cost considered "excellent"
    vlHighAnchor = 30.0  -- per-edge cost considered "poor"

-- |Fraction of bars whose mode-layer cardinality is 7 (i.e. 'ModeOk' shape).
--
-- For walk-generated 'genP' contexts (pcProvenance = Just) the Phase 1
-- invariant guarantees @1.0@. For legacy 'gen' contexts (pcProvenance =
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
-- carries the destination 'Cadence' and the blended weight (output of
-- 'Query.applyComposerBlend').
type TransitionMap = Map Text [(H.Cadence, Double)]

-- |Compute 'psCadenceFav' from a pre-fetched transition map. Pure — no IO.
--
-- The progression is treated as a /cyclic/ loop: edges are
-- @(C₀ → C₁), …, (C_{N-2} → C_{N-1}), (C_{N-1} → C₀)@. Each edge is
-- scored by 'edgeScore' (hybrid presence + share). The per-progression
-- score is the mean of per-edge scores — length-independent and in
-- @[0, 1]@.
--
-- Matching by 'show' (not 'Eq') matches the DB's identity convention
-- (@MATCH (c:Cadence {show: $show})@ at @Query.hs:113-137@). The DB-side
-- 'Cadence' is reconstructed via @constructCadence (movement, chord)@,
-- which may differ from a generated 'Cadence's 'cadenceIntervals' field;
-- the 'show' instance projects to @(movement, functionality)@ only.
cadenceFavFromMap :: TransitionMap -> Prog.Progression -> Double
cadenceFavFromMap srcMap prog =
  let cads = map H.stateCadence
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
-- Returns @0.5 + 0.5 * (w_dst / totalW)@ otherwise — i.e. the edge always
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
  let cads      = map H.stateCadence (toList (Prog.unProgression prog))
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
