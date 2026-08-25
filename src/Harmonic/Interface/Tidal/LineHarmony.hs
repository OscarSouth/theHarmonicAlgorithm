-- |
-- Module      : Harmonic.Interface.Tidal.LineHarmony
-- Description : TidalCycles walking-bass interface
--
-- Wraps 'Harmonic.Traversal.WalkingBass.walkLine' in the cache + innerJoin
-- pattern used by 'Harmonic.Interface.Tidal.Bridge.arrange':
--
--   * The chord-selection pattern is resolved into a PERFORMED bar sequence
--     at eval time ('resolvePerformedSeq'); the walk runs over the bars in
--     performed order, so warp\/rep reorderings and repeats are walked as
--     the audience hears them. Non-periodic selections fall back to stored
--     order with a printed notice.
--   * Progressions seen on 'kProg' are pre-materialised once into walking-
--     bass lines keyed by (triads, chroma sources, performed sequence),
--     forced eagerly so synthesis stays off the audio thread. The forcing
--     horizon is 'Arc 0 1000' — a key first seen beyond cycle 1000 is
--     synthesised on demand (with a printed notice).
--   * 'innerJoin' switches lines reactively when the form changes progression.
--   * The input pattern list is dispatched by 'kinPick': [0,1] is partitioned
--     into N equal windows (N = length of the list) and only the pattern
--     whose window contains the current kinetics signal plays. Output is
--     scaled by 'kDynamic' and the user-supplied dynamics scalar (applied
--     once here — launchers must not multiply by the same dynamic again).
--
-- Each integer in a pattern selects a 1-indexed beat position (1..4).
-- Values outside [1..4] shift by full octaves, matching the div\/mod
-- convention of 'Harmonic.Interface.Tidal.Bridge.arrange': 5 → beat 1 +12, 0 → beat 4 −12, -1 → beat 3 −12.
--
-- In a launcher, the walking line is an ordinary block alongside the
-- orchestral ones:
--
-- @
-- walk f k d = p \"walk\" $ f
--   $ lineHarmony k [\"1 2 3 4\"] |* vel d
-- @
--
-- Because each integer picks a beat position, the pattern controls /which/
-- beats of the generated line sound — @\"1 ~ 3 ~\"@ plays a half-time
-- version of the same line rather than a different line.
module Harmonic.Interface.Tidal.LineHarmony
  ( lineHarmony
  , resolvePerformedSeq
  ) where

import qualified Harmonic.Rules.Types.Progression as P
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as Pt
import Harmonic.Interface.Tidal.Form (Kinetics(..), IK, kinPick)
import Harmonic.Interface.Tidal.Bridge (VoiceFunction, lookupChordAt, forceAll)
import Harmonic.Traversal.WalkingBass
  ( walkLineDyn, walkLinePDyn, ChromaSources(..), beatsPerBar )

import Data.List (nub)
import Data.Maybe (isJust, listToMaybe)
import Data.Foldable (toList)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Sound.Tidal.Context hiding (voice)

-- | Empirical offset between 'Harmonic.Traversal.WalkingBass.walkLine' absolute MIDI and the downstream
-- synth's note-0 pitch on the default patch. Subtract before emitting so the
-- E1..C3 range is audibly faithful without manual @|- oct n@ compensation.
tidalNoteOffset :: Int
tidalNoteOffset = 48

-- | Walking-bass arrangement with kinetics gating.
--
-- Fixed to the double-bass register (E1..C3, MIDI 28..48) inside 'Harmonic.Traversal.WalkingBass.walkLine';
-- the emitted Tidal @note@ values are pre-shifted by @tidalNoteOffset@ so
-- this range is audibly true at default synth tuning — no @|- oct n@
-- compensation needed. Runtime register shifts via @|+ oct n@ \/ @|- oct n@
-- on the launcher side still compose normally.
--
-- For octatripentatonic progressions (@pcProvenance@ = 'Just'), the Pass-3
-- connector pool is reweighted: strata pitches (5 PCs) are most preferred,
-- overlap (cyclic union of adjacent chord-PCs) is neutral, mode pitches
-- (7 PCs) are admissible with a mild penalty, and chromatic ±1 approaches
-- outside any of those sets are removed entirely. For 'Harmonic.Framework.Builder.gen' (legacy)
-- progressions the line is byte-identical to the previous behaviour.
--
-- Entropy is derived internally from the progression's harmonic character.
lineHarmony
  :: Pattern Double       -- ^ Dynamics scalar (amp multiplier)
  -> IK                    -- ^ Performance context (kinetics + chord-selection)
  -> VoiceFunction         -- ^ Beat-1 voicing (fund or root)
  -> [Pattern Int]         -- ^ Polyphonic layers (1-indexed beat positions)
  -> Pattern ValueMap
lineHarmony dyn (kin, chordPat) voiceFn pats =
  let stacked     = kinPick (kin, chordPat) pats
      ctxPat      = kProg kin
      -- Resolve the PERFORMED bar sequence from the chord-selection
      -- pattern: the walk then runs over the bars in the order (and with
      -- the duplications) the audience actually hears, so approach tones
      -- aim at true performed successors and repeated bars walk as
      -- neighbours. Non-periodic selections fall back to stored order.
      performedVals0 = resolvePerformedSeq chordPat
      -- Couple the walk to the piece's dynamics: the eval-time-sampleable
      -- dynamic signal (launcher scalar x form-node dynamic) is quantised
      -- per bar; if the (chord, dynamic) pair sequence is periodic within
      -- the cap, the walked period extends to cover one full dynamic cycle
      -- and the per-bar levels steer the beat-1 register arc. Live control
      -- signals resolve to their defaults at eval time, so live-driven
      -- dynamics stay walk-neutral by construction.
      dynSig = dyn * kDynamic kin
      (performedVals, dynTiers) = resolveDynTiers dynSig performedVals0
      keyPat      = fmap (walkKey performedVals dynTiers) ctxPat
      -- Exact cache domain from the form's own progression list — no
      -- time-window sampling (see 'Harmonic.Interface.Tidal.Bridge.arrange').
      uniqueKeys  = nub (map (walkKey performedVals dynTiers) (kProgs kin))
      -- Build the cache and deeply force each entry so the 3-pass
      -- walking-bass synthesis runs at REPL evaluation time, not on the
      -- audio thread. Mirrors the eager-forcing pattern in 'Harmonic.Interface.Tidal.Bridge.arrange'.
      cache       = [ (k, buildCacheKey voiceFn k) | k <- uniqueKeys ]
      cacheForced = foldr (\(_, (b, _)) acc -> forceAll b `seq` acc) () cache
      lookupCache k = case lookup k cache of
        Just hit -> hit
        Nothing  -> trace "walk: uncached progression - synthesising line on demand"
                          (let pair@(bars, _) = buildCacheKey voiceFn k
                           in forceAll bars `seq` pair)
  in cacheForced `seq` (|* pF "amp" (kDynamic kin)) $
     (|* pF "amp" dyn) $
       innerJoin $ fmap (\k ->
         renderWalk (lookupCache k) chordPat stacked (isJust performedVals)
       ) keyPat

-- | Quantise the dynamic signal per bar (mean of four in-bar samples,
-- eighth-step grid) and extend the performed period so one walked cycle
-- covers both the chord sequence and the dynamic envelope: the smallest
-- multiple k*P <= 64 bars at which the (chord, tier) pair sequence repeats.
-- Returns the (possibly extended) performed values and the aligned tiers;
-- a dynamic signal with no such period leaves the chord period unchanged
-- and the walk dynamics-blind.
resolveDynTiers :: Pattern Double -> Maybe [Int] -> (Maybe [Int], Maybe [Int])
resolveDynTiers _   Nothing     = (Nothing, Nothing)
resolveDynTiers sig (Just vals) =
  case mTiers of
    Nothing    -> (Just vals, Nothing)
    Just tiers ->
      let p      = length vals
          pairs  = zip (cycle vals) tiers
          maxP   = 64
          exts   = [ k * p | k <- [1 ..], k * p <= maxP ]
          fits n = and [ pairs !! i == pairs !! (i + n)
                       | i <- [0 .. length pairs - n - 1] ]
      in case filter fits exts of
           (n:_) -> ( Just (take n (cycle vals))
                    , Just (take n tiers) )
           []    -> (Just vals, Nothing)
  where
    horizon = 128
    barTier k =
      let pointVal t = case queryArc sig (Arc t t) of
                         [ev] -> Just (value ev)
                         _    -> Nothing
          samples = [ pointVal (4 * fromIntegral k + off)
                    | off <- [0.5, 1.5, 2.5, 3.5 :: Time] ]
      in case sequence samples of
           Just vs -> Just (round (8 * sum vs / 4) :: Int)
           Nothing -> Nothing
    mTiers = traverse barTier [0 .. horizon - 1 :: Int]

-- | Resolve the performed bar sequence from a chord-selection pattern.
-- Succeeds when the pattern is bar-quantised (one constant value per
-- 4-cycle bar window) and periodic with the smallest period P <= 64 bars;
-- the result is the raw 1-indexed selector values of one period, in
-- performed order. 'rep s 1' resolves to [1..n]; 'rep s N' to each value
-- N times; 'warp "[..]\/k"' to the written sequence. Degraded (@?@),
-- euclidean, and sub-bar selections return Nothing, and the walk falls
-- back to stored bar order with a one-line notice.
resolvePerformedSeq :: Pattern Int -> Maybe [Int]
resolvePerformedSeq pat =
  case mVals >>= findPeriod of
    Just vs -> Just vs
    Nothing -> trace "walk: non-periodic chord selection - walking stored bar order"
                     Nothing
  where
    maxPeriod = 64
    horizon   = 2 * maxPeriod
    barVal k =
      let pointVal t = case queryArc pat (Arc t t) of
                         [ev] -> Just (value ev)
                         _    -> Nothing
          samples = [ pointVal (4 * fromIntegral k + off)
                    | off <- [0.5, 1.5, 2.5, 3.5 :: Time] ]
      in case sequence samples of
           Just (v:vs) | all (== v) vs -> Just v
           _                           -> Nothing
    mVals = traverse barVal [0 .. horizon - 1 :: Int]
    findPeriod vs =
      listToMaybe
        [ take p vs
        | p <- [1 .. maxPeriod]
        , and [ vs !! k == vs !! (k + p) | k <- [0 .. length vs - p - 1] ]
        ]

-- | Cache key carrying the triad layer plus, when the source is an
-- octatripentatonic ProgressionContext, the per-bar 'ChromaSources' that
-- drive 'Harmonic.Traversal.WalkingBass.walkLineP'. Two contexts with identical triads but different
-- strata walks produce different lines; without the 'ChromaSources' in
-- the key the cache would silently collide. The third component is the
-- resolved performed sequence (raw selector values), so two launches
-- differing only in warp\/rep never collide either; the fourth is the
-- per-bar dynamic tier vector steering the register arc.
type WalkKey = (P.Progression, Maybe [ChromaSources], Maybe [Int], Maybe [Int])

-- | Project a 'Harmonic.Rules.Types.ProgressionContext.ProgressionContext' into the cache key. For 'Harmonic.Framework.Builder.gen'
-- (no provenance) the second component is 'Nothing' — legacy 'Harmonic.Traversal.WalkingBass.walkLine'
-- handles those. For 'Harmonic.Framework.Builder.genP' it carries the per-bar 5-PC strata and 7-PC
-- mode chroma read directly off the auxiliary layers, which after the
-- 3-5-7 fix carry the full chroma rooted on each bar's harmonic root.
walkKey :: Maybe [Int] -> Maybe [Int] -> PC.ProgressionContext -> WalkKey
walkKey performedVals dynTiers ctx =
  ( PC.triadLayer ctx
  , case PC.pcProvenance ctx of
      Nothing -> Nothing
      Just _  -> Just (chromaSourcesFor ctx)
  , performedVals
  , dynTiers
  )

-- | Read per-bar (strata, mode) absolute-PC sets from the strata \/ mode
-- auxiliary layers. After the Builder fix these carry 5 \/ 7 PCs as
-- intervals from the bar's harmonic root, so we add the root back to
-- recover absolute pitch classes.
chromaSourcesFor :: PC.ProgressionContext -> [ChromaSources]
chromaSourcesFor ctx =
  let strataCSs = toList (P.unProgression (PC.strataLayer ctx))
      modeCSs   = toList (P.unProgression (PC.modeLayer   ctx))
      pcsAbs cs =
        let r = Pt.unPitchClass (Pt.pitchClass (H.stateCadenceRoot cs))
            ints = map Pt.unPitchClass (H.cadenceIntervals (H.stateCadence cs))
        in Set.fromList [ (i + r) `mod` 12 | i <- ints ]
  in [ ChromaSources (pcsAbs s) (pcsAbs m)
     | (s, m) <- zip strataCSs modeCSs ]

-- | Pre-compute walking line for a single cache key; convert to 'Note',
-- shifted by @tidalNoteOffset@ so absolute MIDI from 'Harmonic.Traversal.WalkingBass.walkLine' aligns with
-- Tidal's @note@ convention. When the key carries a performed sequence,
-- the stored bars (and ChromaSources, for genP) are reordered\/duplicated
-- into performed order first and the walk runs over THAT progression —
-- the cached line is then indexed by performed ordinal in 'renderWalk'.
-- Dispatches on the 'ChromaSources' presence:
-- 'Just' → 'Harmonic.Traversal.WalkingBass.walkLineP'; 'Nothing' → legacy 'Harmonic.Traversal.WalkingBass.walkLine'.
buildCacheKey :: VoiceFunction -> WalkKey -> ([[Note]], Int)
buildCacheKey voiceFn (prog, mChromas, mVals, mTiers) =
  let barsL = toList (P.unProgression prog)
      n     = length barsL
      mIdxs = case mVals of
                Just vals | n > 0 -> Just [ (v - 1) `mod` n | v <- vals ]
                _                 -> Nothing
      (prog', mChromas') = case mIdxs of
        Nothing -> (prog, mChromas)
        Just is -> ( P.fromCadenceStates [ barsL !! i | i <- is ]
                   , fmap (\chs -> [ chs !! i | i <- is ]) mChromas )
      mDyn = fmap (map (\t -> fromIntegral t / 8)) mTiers
      line = case mChromas' of
               Nothing      -> walkLineDyn  mDyn voiceFn prog'
               Just chromas -> walkLinePDyn mDyn voiceFn prog' chromas
  in (map (map (\m -> fromIntegral (m - tidalNoteOffset))) line, length line)

-- | Map stacked beat-position events through the cached walking line.
-- In performed mode the cached line is already in performed order, one
-- bar per performed ordinal: the bar index is the global bar count
-- (onset \/ 4) modulo the period. In stored-order fallback the bar index
-- comes from the chord-selection pattern's value at the onset.
renderWalk
  :: ([[Note]], Int)
  -> Pattern Int            -- ^ Bar-selection pattern (1-indexed)
  -> Pattern Int            -- ^ Stacked beat-index input
  -> Bool                   -- ^ Performed mode (line is in performed order)
  -> Pattern ValueMap
renderWalk (bars, nBars) chordPat stacked performed
  | nBars == 0 = silence
  | otherwise =
      let chordIdx = fmap (\i -> (i - 1) `mod` nBars) chordPat

          mapped = Pattern (\st ->
            let noteEvs = query stacked st
            in concatMap (\nEv -> case whole nEv of
                 Nothing   -> []
                 Just wArc ->
                   let onsetT  = start wArc
                       ci      = if performed
                                 then floor (onsetT / 4)
                                 else lookupChordAt onsetT chordIdx
                       bar     = bars !! (ci `mod` nBars)
                       noteVal = value nEv
                       vShift  = noteVal - 1            -- 1-indexed → 0-indexed
                       idx     = vShift `mod` beatsPerBar
                       octave  = vShift `div` beatsPerBar
                   in [nEv { value = (bar !! idx) + fromIntegral (octave * 12) }]
               ) noteEvs
            ) Nothing Nothing
      in note mapped
