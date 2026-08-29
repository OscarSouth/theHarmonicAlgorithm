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
--   $ lineHarmony d k fund [\"1 2 3 4\"]
-- @
--
-- (dynamics scalar, context, beat-1 voicing, then the beat-position
-- patterns; the scalar multiplies the form's 'kDynamic' internally — do
-- not also apply @|* vel d@.)
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
  ( walkLineDyn, walkLinePDyn, walkLineJDyn, ChromaSources(..), beatsPerBar )
import qualified Harmonic.Rules.Import.Jazz as J

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
-- The walk adapts to the generation family of the context it is given.
-- For octatripentatonic progressions (@pcProvenance@ = 'Just'), the Pass-3
-- connector pool is reweighted: strata pitches (5 PCs) are most preferred,
-- overlap (cyclic union of adjacent chord-PCs) is neutral, mode pitches
-- (7 PCs) are admissible with a mild penalty, and chromatic ±1 approaches
-- outside any of those sets are removed entirely. For jazz progressions
-- (@pcFamily@ = 'Harmonic.Rules.Types.ProgressionContext.FJazz') each bar
-- carries a 'Harmonic.Rules.Import.Jazz.BassVocab' derived from its chord
-- symbol, so the line reads the harmony as a bass player does — the fifth
-- a 13th chord omits is restored, an altered dominant's \#5 replaces the
-- natural 5, and notated colour alterations stay off strong beats.
-- 'Harmonic.Framework.Builder.gen' and 'Harmonic.Framework.Builder.genE'
-- progressions walk their plain per-bar tone sets (for genE that is the
-- foundation layer — the layer that owns the bass); connector tones draw
-- on the key-area palette
-- ('Harmonic.Evaluation.Analysis.KeyArea.barPalettes') — the same
-- analysis behind the chordscale S\/M layers.
--
-- Entropy is derived internally from the progression's harmonic character.
lineHarmony
  :: Pattern Double       -- ^ Dynamics scalar (amp multiplier)
  -> IK                    -- ^ Performance context (kinetics + chord-selection)
  -> VoiceFunction         -- ^ Beat-1 voicing (@fund@ or @root@). Only the bass pitch class of each bar is read, so a solving strategy buys nothing here: @grid@ returns the root by its own invariant, at the price of a voicing solve
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
      -- Cache keyed on the CONTEXT, not on the derived walk key. The form
      -- can only ever emit the progressions in 'kProgs', so each one's key
      -- and line are derived exactly once, here. Deriving the key inside
      -- the query instead (@fmap (walkKey …) ctxPat@) rebuilt the jazz bass
      -- vocabulary — and the genP chroma sets — for every bar on every
      -- query, i.e. once per audio tick per bar.
      --
      -- Entries are deeply forced so the 3-pass walking-bass synthesis runs
      -- at REPL evaluation time rather than on the audio thread. Mirrors
      -- the eager-forcing pattern in 'Harmonic.Interface.Tidal.Bridge.arrange'.
      lineFor ctx = buildCacheKey voiceFn (walkKey performedVals dynTiers ctx)
      cache       = [ (ctx, lineFor ctx) | ctx <- nub (kProgs kin) ]
      cacheForced = foldr (\(_, (b, _)) acc -> forceAll b `seq` acc) () cache
      -- Unreachable for both kinetics constructors ('formK' emits only
      -- values drawn from its node list, 'lK' a single one), so this is a
      -- safety net, not a path. Silent by design: it runs inside the query,
      -- where a Debug.Trace write would take the stderr lock on the clock
      -- thread every tick and turn a miss into a permanent dropout.
      lookupLine ctx = case lookup ctx cache of
        Just hit -> hit
        Nothing  -> let pair@(bars, _) = lineFor ctx
                    in forceAll bars `seq` pair
  in cacheForced `seq` (|* pF "amp" (kDynamic kin)) $
     (|* pF "amp" dyn) $
       innerJoin $ fmap (\ctx ->
         renderWalk (lookupLine ctx) chordPat stacked (isJust performedVals)
       ) ctxPat

-- | Quantise the dynamic signal per bar (mean of four in-bar samples,
-- eighth-step grid) and extend the performed period so one walked cycle
-- covers both the chord sequence and the dynamic envelope: the smallest
-- multiple k*P <= 64 bars at which the (chord, tier) pair sequence repeats.
-- Returns the (possibly extended) performed values and the aligned tiers;
-- a dynamic signal with no such period leaves the chord period unchanged
-- and the walk dynamics-blind.
resolveDynTiers :: Pattern Double -> Maybe [Int] -> (Maybe [Int], Maybe [Int])
resolveDynTiers _    Nothing     = (Nothing, Nothing)
-- No performed values means no period to extend, and `cycle []` diverges.
resolveDynTiers _    (Just [])   = (Just [], Nothing)
resolveDynTiers sigP (Just vals) =
  case mTiers of
    Nothing    -> (Just vals, Nothing)
    Just tiers ->
      let p      = length vals
          pairs  = zip (cycle vals) tiers
          maxP   = 64
          -- takeWhile, NOT a filter guard. `[k * p | k <- [1..], k * p <= maxP]`
          -- draws from an infinite source and simply stops yielding once the
          -- guard fails — it never ends. `filter fits` over that hangs forever
          -- whenever no extension fits, which made the `[]` branch below
          -- unreachable and could freeze a live set outright. Any multi-node
          -- form with ramping dynamics produces a non-periodic tier vector and
          -- reaches exactly that case.
          exts   = takeWhile (<= maxP) [ k * p | k <- [1 ..] ]
          fits m = and [ pairs !! i == pairs !! (i + m)
                       | i <- [0 .. length pairs - m - 1] ]
      in case filter fits exts of
           (m:_) -> ( Just (take m (cycle vals))
                    , Just (take m tiers) )
           []    -> (Just vals, Nothing)
  where
    horizon = 128
    barTier k =
      let pointVal t = case queryArc sigP (Arc t t) of
                         [ev] -> Just (value ev)
                         _    -> Nothing
          beatVals = [ pointVal (4 * fromIntegral k + beatOff)
                     | beatOff <- [0.5, 1.5, 2.5, 3.5 :: Time] ]
      in case sequence beatVals of
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
          beatVals = [ pointVal (4 * fromIntegral k + beatOff)
                     | beatOff <- [0.5, 1.5, 2.5, 3.5 :: Time] ]
      in case sequence beatVals of
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
-- the key the cache would silently collide. The third is the per-bar
-- jazz bass vocabulary, which distinguishes an FJazz context from an
-- FTriad one carrying a byte-identical triad layer. The fourth is the
-- resolved performed sequence (raw selector values), so two launches
-- differing only in warp\/rep never collide either; the fifth is the
-- per-bar dynamic tier vector steering the register arc.
type WalkKey =
  ( P.Progression
  , Maybe [ChromaSources]   -- genP strata/mode chroma
  , Maybe [J.BassVocab]     -- genJ per-bar bass vocabulary
  , Maybe [Int]             -- performed sequence
  , Maybe [Int]             -- dynamic tiers
  )

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
  , jazzVocabFor ctx
  , performedVals
  , dynTiers
  )

-- | Per-bar walking-bass vocabulary for jazz-family contexts: corpus tone
-- sets are working voicings (13th chords omit the 5th and 11th; altered
-- qualities replace the 5th; notated colours are not strong-beat targets),
-- so FJazz bars walk from 'J.bassVocabFor' over their stored intervals
-- rather than the raw shapes. Other families return 'Nothing' and walk
-- exactly as before.
jazzVocabFor :: PC.ProgressionContext -> Maybe [J.BassVocab]
jazzVocabFor ctx
  | PC.pcFamily ctx /= PC.FJazz = Nothing
  | otherwise = Just
      [ J.bassVocabFor
          (map Pt.unPitchClass (H.cadenceIntervals (H.stateCadence cs)))
      | cs <- toList (P.unProgression (PC.triadLayer ctx)) ]

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
  in [ ChromaSources (pcsAbs sc) (pcsAbs mc)
     | (sc, mc) <- zip strataCSs modeCSs ]

-- | Pre-compute walking line for a single cache key; convert to 'Note',
-- shifted by @tidalNoteOffset@ so absolute MIDI from 'Harmonic.Traversal.WalkingBass.walkLine' aligns with
-- Tidal's @note@ convention. When the key carries a performed sequence,
-- the stored bars (and ChromaSources, for genP) are reordered\/duplicated
-- into performed order first and the walk runs over THAT progression —
-- the cached line is then indexed by performed ordinal in 'renderWalk'.
-- Dispatches on the two family side-channels, chroma first: strata
-- chroma → 'Harmonic.Traversal.WalkingBass.walkLineP'; jazz vocabulary →
-- 'Harmonic.Traversal.WalkingBass.walkLineJ'; neither → legacy
-- 'Harmonic.Traversal.WalkingBass.walkLine'. The two are mutually
-- exclusive in practice — genP carries provenance and genJ does not — so
-- the order only fixes a case that cannot currently arise.
--
-- Performed order, deliberately: the walk analyses (and walks) the bar
-- sequence AS PLAYED — under a non-identity chord selection its key-area
-- palettes come from a fresh analysis of the performed sequence, not a
-- permutation of the stored one that the chordscale S\/M layers carry.
-- Same theory, two applications (generation-time annotation vs runtime
-- line), and the walk adds chromatic approach tones beyond either. Ruled
-- 2026-08-29.
buildCacheKey :: VoiceFunction -> WalkKey -> ([[Note]], Int)
buildCacheKey voiceFn (prog, mChromas, mVocab, mVals, mTiers) =
  let barsL = toList (P.unProgression prog)
      nBars = length barsL
      mIdxs = case mVals of
                Just vals | nBars > 0 -> Just [ (v - 1) `mod` nBars | v <- vals ]
                _                 -> Nothing
      (prog', mChromas', mVocab') = case mIdxs of
        Nothing -> (prog, mChromas, mVocab)
        Just is -> ( P.fromCadenceStates [ barsL !! i | i <- is ]
                   , fmap (\chs -> [ chs !! i | i <- is ]) mChromas
                   , fmap (\vs  -> [ vs  !! i | i <- is ]) mVocab )
      mDyn = fmap (map (\t -> fromIntegral t / 8)) mTiers
      line = case (mChromas', mVocab') of
               (Just chromas, _) -> walkLinePDyn mDyn voiceFn prog' chromas
               (_, Just vocabs)  -> walkLineJDyn mDyn voiceFn prog' vocabs
               _                 -> walkLineDyn  mDyn voiceFn prog'
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
                       octv    = vShift `div` beatsPerBar
                   in [nEv { value = (bar !! idx) + fromIntegral (octv * 12) }]
               ) noteEvs
            ) Nothing Nothing
      in note mapped
