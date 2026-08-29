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
  , scoreProgressionJazz
    -- * Polytonal scoring (genE)
  , PolyScore(..)
  , polyLayerWeights
  , polyDivergenceWeight
  , polyDivergence
  , scorePoly
  , scorePolyOnline
  , jazzChangeKey
  , TransitionCache
  , newTransitionCache
  , resolveJazzBlend
  , cadenceFavFromKeys
  , computeCadenceFav
  ) where

import           Control.Monad (forM)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import           Data.Bifunctor (first)
import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.List (nub, sort)

import           Harmonic.Database (DbActionT)

import qualified Harmonic.Rules.Types.Progression as Prog
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as P
import qualified Harmonic.Rules.Import.Jazz as J
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

-- |Cadence-favourability dominant: @0.5@ fav, @0.25@ each on root motion
-- and voice leading. Mode validity carries NO weight: it is structurally
-- @1.0@ on every walk-generated progression (proven — see CLAUDE.md's
-- mode-invariant note), so weighting it only compressed the usable score
-- range to @[0.2, 1.0]@. It remains a hard viability /gate/
-- (@psModeValidity >= 1.0@) in the attempt loop, which is the one job it
-- can actually do. The three live weights are the old @0.2\/0.2\/0.4@
-- renormalised — rankings are order-identical to the previous scale via
-- @new = (old − 0.2) \/ 0.8@.
defaultWeights :: ProgressionScoreWeights
defaultWeights = ProgressionScoreWeights
  { wRootMotion   = 0.25
  , wVoiceLeading = 0.25
  , wCadenceFav   = 0.5
  , wModeValidity = 0
  }

-- |Offline-mode weights — cadence favourability dropped (no graph), mode
-- validity gate-only as in 'defaultWeights', the two live axes
-- renormalised. Offline totals now span the full @[0, 1]@ instead of
-- having a @1\/3@ floor.
defaultWeightsOffline :: ProgressionScoreWeights
defaultWeightsOffline = ProgressionScoreWeights
  { wRootMotion   = 0.5
  , wVoiceLeading = 0.5
  , wCadenceFav   = 0
  , wModeValidity = 0
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
           edges    = zip rootPCs (drop 1 rootPCs ++ take 1 rootPCs)
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
-- gen \/ genE \/ genVI × len {8, 16} × entropy {0.2, 0.4, 0.6}, two runs
-- each (genVI cued in-strata; its random-cue empties excluded). The genE
-- column predates the polytonal rewrite (it measured the retired fused
-- 4-note surface; today's genE scores its foundation, a plain classical
-- walk in gen's band) — anchors stand pending re-measurement. Observed
-- per-edge cyclic costs: gen 2.5–5.75, genE(fused) 3.25–12.0, genVI 3.0–7.1;
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
    -- against that artefact). Hand-built 4-note chains score the heard
    -- surface; mixed-cardinality bars score real alignment costs.
    honestVoicing cs =
      let r = P.unPitchClass (P.pitchClass (H.stateCadenceRoot cs))
      in sort [ (P.unPitchClass i + r) `mod` 12
              | i <- H.cadenceIntervals (H.stateCadence cs) ]
    -- Anchors recalibrated 2026-08-20 against the honest measurement:
    -- online probe gen\/genVI\/genE(fused, pre-rewrite) × len 8\/16 × entropy {0.2,0.4,0.6},
    -- per-edge cyclic costs of sorted absolute-PC voicings. See the
    -- calibration values below (updated by the probe in the VL pass).
    vlLowAnchor  = vlLowAnchorCal
    vlHighAnchor = vlHighAnchorCal

-- |Fraction of bars whose mode-layer cardinality is 7 (i.e. 'Harmonic.Rules.Types.Scale.ModeOk' shape).
--
-- For walk-generated 'Harmonic.Framework.Builder.genP' contexts (pcProvenance = Just) the Phase 1
-- invariant guarantees @1.0@. The gate is PROVENANCE, not layer content:
-- contexts without it (gen\/genJ chordscale-derived layers — 7-PC by
-- construction — genE partner triads, hand-built material) return @1.0@
-- because the strata mode-validity contract only exists where a strata
-- walk produced the layers.
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
-- Polytonal scoring (genE)
-------------------------------------------------------------------------------

-- |Composite score for a polytonal ('PC.FPoly') context. The other three
-- families are one progression wearing three hats; genE is genuinely three
-- progressions, so ranking its attempts on the foundation alone would leave
-- the partner chains — the whole point of the family — unselected.
data PolyScore = PolyScore
  { pyT          :: !ProgressionScore  -- ^ Foundation layer, scored alone
  , pyS          :: !ProgressionScore  -- ^ First partner chain
  , pyM          :: !ProgressionScore  -- ^ Second partner chain
  , pyQuality    :: !Double            -- ^ Weighted blend of the three, @[0, 1]@
  , pyDivergence :: !Double            -- ^ How far apart the layers stand, @[0, 1]@
  , pyTotal      :: !Double            -- ^ Ranking value, @[0, 1]@
  } deriving (Show, Eq)

-- |Layer weights for 'pyQuality': foundation, then each partner. The
-- foundation owns the bass and the harmonic spine, so it carries half; the
-- partners colour it and split the rest.
polyLayerWeights :: (Double, Double, Double)
polyLayerWeights = (0.5, 0.25, 0.25)

-- |Share of 'pyTotal' carried by 'pyDivergence' rather than 'pyQuality'.
--
-- A weighted sum rather than a multiplier: quality still leads, and
-- divergence discriminates between walks that are otherwise equally good.
--
-- Calibrated 2026-08-29 against the requirement that raising @attempt@'s K
-- must raise BOTH the mean quality and the mean divergence of the selection.
-- Two independent offline pools of 60 genE walks (len 6, entropy 0.4), 20k
-- selection trials per cell, means of the selected attempt:
--
-- @
-- weight   divergence K=1 -> 4 -> 12      quality gain K=1->12
--  0.00    0.685  0.696  0.682  (FALLS)          +0.143
--  0.25    0.690  0.714  0.730  (+0.040)         +0.137
--  0.40    0.689  0.733  0.752  (+0.063)         +0.128
--  0.50    0.691  0.744  0.764  (+0.074)         +0.118
-- @
--
-- At zero the selection's divergence actually falls as K rises — quality
-- alone decides, which is the failure this axis exists to prevent. 0.25
-- rises but flattens between K=4 and K=12. 0.40 is the knee: it buys 57%
-- more divergence gain than 0.25 for less than a hundredth of quality gain,
-- and both means climb monotonically. Past 0.50 the quality gain erodes
-- faster than divergence improves.
polyDivergenceWeight :: Double
polyDivergenceWeight = 0.40

-- |Absolute pitch classes of a bar, deduplicated.
barPCs :: H.CadenceState -> [Int]
barPCs cs =
  let r   = P.unPitchClass (P.pitchClass (H.stateCadenceRoot cs))
      ivs = map P.unPitchClass (H.cadenceIntervals (H.stateCadence cs))
  in nub [ (iv + r) `mod` 12 | iv <- ivs ]

-- |Bar root as an absolute pitch class.
barRootPC :: H.CadenceState -> Int
barRootPC = P.unPitchClass . P.pitchClass . H.stateCadenceRoot

-- |How far a polytonal context's three layers stand apart, @[0, 1]@,
-- averaged over bars. Two equally-weighted readings:
--
-- * GEOMETRY — the partners sharing exactly two tones is the common-dyad
--   shape; anything else is base-anchored, where S and M sound the full
--   pentad against each other. Deliberately the same test as @PolyGen@'s
--   @pdGeometry@, so the number the ranker optimises and the label the
--   diagnostics print can never disagree.
--
-- * ROOT SPREAD — summed interval-class distance between the three roots,
--   normalised by its maximum. Three triads on distant roots read as more
--   polytonal than three sharing a neighbourhood.
polyDivergence :: PC.ProgressionContext -> Double
polyDivergence pc =
  let bars = zip3 (toList (Prog.unProgression (PC.triadLayer  pc)))
                  (toList (Prog.unProgression (PC.strataLayer pc)))
                  (toList (Prog.unProgression (PC.modeLayer   pc)))
  in case bars of
       [] -> 0.0
       _  -> sum (map barDivergence bars) / fromIntegral (length bars)

-- |Per-bar term of 'polyDivergence'.
barDivergence :: (H.CadenceState, H.CadenceState, H.CadenceState) -> Double
barDivergence (t, s, m) =
  let sPCs   = barPCs s
      shared = length (filter (`elem` barPCs m) sPCs)
      geom   = if shared == 2 then 0.0 else 1.0
      ic a b = let d = abs (a - b) in fromIntegral (min d (12 - d)) :: Double
      spread = (ic (barRootPC t) (barRootPC s)
              + ic (barRootPC t) (barRootPC m)
              + ic (barRootPC s) (barRootPC m)) / 18.0
  in 0.5 * geom + 0.5 * spread

-- |A layer read as a progression in its own right, so the ordinary scorers
-- apply to it unchanged.
layerAlone :: (PC.ProgressionContext -> Prog.Progression)
           -> PC.ProgressionContext -> PC.ProgressionContext
layerAlone sel = PC.fromProgression . sel

-- |Assemble a 'PolyScore' from three per-layer scores and a divergence.
assemblePoly :: ProgressionScoreWeights
             -> ProgressionScore -> ProgressionScore -> ProgressionScore
             -> Double -> PolyScore
assemblePoly w t s m d =
  let (wt, ws, wm) = polyLayerWeights
      q  = wt * totalScore w t + ws * totalScore w s + wm * totalScore w m
      wd = polyDivergenceWeight
  in PolyScore
       { pyT = t, pyS = s, pyM = m
       , pyQuality = q
       , pyDivergence = d
       , pyTotal = (1 - wd) * q + wd * d
       }

-- |Pure polytonal score — all three layers on the offline components.
scorePoly :: ProgressionScoreWeights -> PC.ProgressionContext -> PolyScore
scorePoly w pc =
  assemblePoly w
    (scoreProgression (layerAlone PC.triadLayer  pc))
    (scoreProgression (layerAlone PC.strataLayer pc))
    (scoreProgression (layerAlone PC.modeLayer   pc))
    (polyDivergence pc)

-- |Online polytonal score. Each partner bar is a real graph continuation of
-- its own chain, so cadence favourability is as meaningful for the partners
-- as for the foundation and all three are fetched. The shared
-- 'TransitionCache' keeps the extra passes cheap across a K-attempt loop.
scorePolyOnline :: TransitionCache -> Text -> ProgressionScoreWeights
                -> PC.ProgressionContext -> DbActionT PolyScore
scorePolyOnline cache blend w pc = do
  t <- scoreProgressionOnline cache blend (layerAlone PC.triadLayer  pc)
  s <- scoreProgressionOnline cache blend (layerAlone PC.strataLayer pc)
  m <- scoreProgressionOnline cache blend (layerAlone PC.modeLayer   pc)
  pure (assemblePoly w t s m (polyDivergence pc))

-------------------------------------------------------------------------------
-- Cadence-favourability aggregation (pure helper for Phase 5)
-------------------------------------------------------------------------------

-- |A pre-fetched, composer-blend-resolved map from source cadence (keyed by
-- its 'show' representation) to its outgoing transitions. Each transition
-- carries the destination 'Harmonic.Rules.Types.Harmony.Cadence' and the blended weight (output of
-- 'Query.applyComposerBlend').
type TransitionMap = Map Text [(Text, Double)]

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
  -- Project each bar through the triad walk shadow ('walkTriadCadence',
  -- identity for triads) so 4-note chains score the same corpus edges the
  -- walk actually followed; without this a hand-built extended chain's keys miss the
  -- map entirely and psCadenceFav collapses to 0.
  cadenceFavFromKeys srcMap
    (map (T.pack . show . H.walkTriadCadence . H.stateCadence)
         (toList (Prog.unProgression prog)))

-- |Key-level core of 'cadenceFavFromMap': cyclic per-edge mean over an
-- arbitrary node-key sequence. The jazz scorer passes jazz node keys
-- (whose 'show' identity is the @Change@ graph's) through the same
-- arithmetic.
cadenceFavFromKeys :: TransitionMap -> [Text] -> Double
cadenceFavFromKeys srcMap keys =
  let n = length keys
  in if n < 2 then 0
     else
       let edges = zip keys (drop 1 keys ++ take 1 keys)
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
edgeScore :: TransitionMap -> (Text, Text) -> Double
edgeScore srcMap (srcKey, dstKey) =
  case Map.lookup srcKey srcMap of
       Nothing           -> 0
       Just transitions  ->
         let totalW   = sum (map snd transitions)
             matched  = sum [ w | (k, w) <- transitions, k == dstKey ]
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
-- Runs inside 'DbActionT' so the caller controls connection
-- lifecycle (typically a single shared pipe across a multi-attempt loop).
scoreProgressionOnline
  :: TransitionCache               -- ^ Cross-attempt fetch cache ('newTransitionCache').
  -> Text                          -- ^ Seek string (composer blend; same format as @_gcSeek@).
  -> PC.ProgressionContext
  -> DbActionT ProgressionScore
scoreProgressionOnline cache seekStr pc = do
  let basePure = scoreProgression pc
  cf <- computeCadenceFav cache seekStr (PC.triadLayer pc)
  pure basePure { psCadenceFav = cf }

-- |Mutable fetch cache threaded through a multi-attempt loop: source key →
-- blend-resolved outgoing transitions. Attempts share source cadences
-- heavily (same cue, same key, same filters), so attempts 2..K score
-- almost query-free. Valid for ONE blend — create a fresh cache per
-- @generateBest@ call, never share across seek strings.
type TransitionCache = IORef TransitionMap

-- |A fresh, empty 'TransitionCache'.
newTransitionCache :: IO TransitionCache
newTransitionCache = newIORef Map.empty

-- |Cyclic per-edge favourability mean, computed against Neo4j. Builds the
-- 'TransitionMap' by fetching each unique source cadence's outgoing
-- transitions once, applying the composer blend, then delegating to the
-- pure 'cadenceFavFromMap'.
--
-- Number of graph queries = number of distinct source-cadence 'show' keys
-- in the progression NOT already in the cache (≤ N for an N-bar
-- progression on the first attempt; near zero on later attempts).
computeCadenceFav
  :: TransitionCache
  -> Text
  -> Prog.Progression
  -> DbActionT Double
computeCadenceFav cache seekStr prog = do
  -- Walk-shadow projection: see 'cadenceFavFromMap'.
  let cads      = map (H.walkTriadCadence . H.stateCadence) (toList (Prog.unProgression prog))
      srcKeys   = nub (map (T.pack . show) cads)
      blend     = Q.parseComposerWeights seekStr
  -- Wildcard blend: project the pre-aggregated r.confidence (identical to
  -- resolveWeights over the full weights map, by the ingestion invariant)
  -- instead of parsing every edge's weights JSON. edgeScore is
  -- share-of-total, so ordering is immaterial either way.
  pairs <- forM srcKeys $ \k -> do
    resolved <- fetchCached cache k $ do
      raw <- if Map.null blend
               then Q.fetchTransitionsAggregate k
               else Q.resolveWeights blend <$> Q.fetchTransitions k
      pure [ (T.pack (show c), w) | (c, w) <- raw ]
    pure (k, resolved)
  let srcMap = Map.fromList pairs
  pure (cadenceFavFromMap srcMap prog)

-- Cache-through fetch: hit returns the stored transitions, miss runs the
-- supplied fetch and stores its result.
fetchCached :: TransitionCache -> Text -> DbActionT [(Text, Double)] -> DbActionT [(Text, Double)]
fetchCached cache k fetch = do
  m <- liftIO (readIORef cache)
  case Map.lookup k m of
    Just v  -> pure v
    Nothing -> do
      v <- fetch
      liftIO (modifyIORef' cache (Map.insert k v))
      pure v

-- |Jazz analogue of 'H.walkTriadCadence': project a bar onto its
-- Change-graph node key. Identity for walk-generated bars (their
-- functionality was already set by the jazz namer); maps a lead-produced
-- cue onto the node the walk itself started from, so the cyclic
-- favourability score no longer charges both cue edges as misses. Sets
-- outside the jazz vocabulary keep their raw key and score 0 — honest.
jazzChangeKey :: H.CadenceState -> Text
jazzChangeKey cs =
  let cad = H.stateCadence cs
      set = map P.unPitchClass (H.cadenceIntervals cad)
  in case J.jazzFunctionality set of
       Just nm -> "( " <> T.pack (show (H.cadenceMovement cad)) <> " -> " <> nm <> " )"
       Nothing -> T.pack (show cad)

-- |Jazz-family variant of 'scoreProgressionOnline': pure components are
-- identical (they are arity-agnostic); 'psCadenceFav' is computed against
-- the @Change@ graph under the seek spec's JAZZ half — the spec is split
-- by 'Q.splitSeekByCorpus' exactly as the genJ walk splits it, so an
-- attempt loop ranks by the same musical priorities the walk sampled
-- under. Node keys are the bars' own 'show' identities (jazz names) —
-- no triad projection.
scoreProgressionJazz
  :: TransitionCache               -- ^ Cross-attempt fetch cache ('newTransitionCache').
  -> Q.ComposerWeights             -- ^ Resolved jazz blend ('resolveJazzBlend'); empty = wildcard.
  -> PC.ProgressionContext
  -> DbActionT ProgressionScore
scoreProgressionJazz cache jazzBlend pc = do
  let basePure = scoreProgression pc
      prog     = PC.triadLayer pc
      keys     = map jazzChangeKey (toList (Prog.unProgression prog))
  pairs <- forM (nub keys) $ \k -> do
    resolved <- fetchCached cache k $
      if Map.null jazzBlend
        then map (first Q.ccShow) <$> Q.fetchChangeAggregate k
        else map (first Q.ccShow) . Q.resolveWeights jazzBlend
               <$> Q.fetchChangeTransitions k
    pure (k, resolved)
  let cf = cadenceFavFromKeys (Map.fromList pairs) keys
  pure basePure { psCadenceFav = cf }

-- |Resolve a seek string's jazz half once (one 'Q.fetchJazzComposers'
-- round trip), for threading through a multi-attempt loop. An empty seek
-- blend (wildcard) resolves to the empty map without touching the graph.
resolveJazzBlend :: Text -> DbActionT Q.ComposerWeights
resolveJazzBlend seekStr = do
  let blend = Q.parseComposerWeights seekStr
  if Map.null blend
    then pure Map.empty
    else do
      jazzKeys <- Q.fetchJazzComposers
      pure (fst (Q.splitSeekByCorpus jazzKeys blend))

-------------------------------------------------------------------------------
-- Internal helpers
-------------------------------------------------------------------------------

clamp01 :: Double -> Double
clamp01 x
  | x < 0     = 0
  | x > 1     = 1
  | otherwise = x
