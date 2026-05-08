-- |
-- Module      : Harmonic.Interface.Tidal.LineHarmony
-- Description : TidalCycles walking-bass interface
--
-- Wraps 'Harmonic.Traversal.WalkingBass.walkLine' in the cache + innerJoin
-- pattern used by 'Harmonic.Interface.Tidal.Bridge.arrange':
--
--   * Progressions seen on 'kProg' are pre-materialised once into walking-
--     bass lines keyed by (progression, entropy).
--   * 'innerJoin' switches lines reactively when the form changes progression.
--   * The input pattern list is dispatched by 'kinPick': [0,1] is partitioned
--     into N equal windows (N = length of the list) and only the pattern
--     whose window contains the current kinetics signal plays. Output is
--     scaled by 'kDynamic' and the user-supplied dynamics scalar.
--
-- Each integer in a pattern selects a 1-indexed beat position (1..4).
-- Values outside [1..4] shift by full octaves, matching the div/mod convention
-- of 'arrange' (e.g. 5 → beat 1 +12, -1 → beat 4 -12).
module Harmonic.Interface.Tidal.LineHarmony
  ( lineHarmony
  ) where

import qualified Harmonic.Rules.Types.Progression as P
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Pitch as Pt
import Harmonic.Interface.Tidal.Form (Kinetics(..), IK, kinPick)
import Harmonic.Interface.Tidal.Bridge (VoiceFunction, lookupChordAt, forceAll)
import Harmonic.Traversal.WalkingBass
  ( walkLine, walkLineP, ChromaSources(..), beatsPerBar )

import Data.List (nub)
import Data.Foldable (toList)
import qualified Data.Set as Set
import Sound.Tidal.Context hiding (voice)

-- | Empirical offset between 'walkLine' absolute MIDI and the downstream
-- synth's note-0 pitch on the default patch. Subtract before emitting so the
-- E1..C3 range is audibly faithful without manual @|- oct n@ compensation.
tidalNoteOffset :: Int
tidalNoteOffset = 48

-- | Walking-bass arrangement with kinetics gating.
--
-- Fixed to the double-bass register (E1..C3, MIDI 28..48) inside 'walkLine';
-- the emitted Tidal @note@ values are pre-shifted by 'tidalNoteOffset' so
-- this range is audibly true at default synth tuning — no @|- oct n@
-- compensation needed. Runtime register shifts via @|+ oct n@ / @|- oct n@
-- on the launcher side still compose normally.
--
-- For octatripentatonic progressions ('pcProvenance' = 'Just'), the Pass-3
-- connector pool is reweighted: strata pitches (5 PCs) are most preferred,
-- overlap (cyclic union of adjacent chord-PCs) is neutral, mode pitches
-- (7 PCs) are admissible with a mild penalty, and chromatic ±1 approaches
-- outside any of those sets are removed entirely. For 'gen' (legacy)
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
      keyPat      = fmap walkKey ctxPat
      progPat     = fmap PC.triadLayer ctxPat
      allEvents   = queryArc keyPat (Arc 0 1000)
      uniqueKeys  = nub (map value allEvents)
      -- Build the cache and deeply force each entry so the 3-pass
      -- walking-bass synthesis runs at REPL evaluation time, not on the
      -- audio thread. Mirrors the eager-forcing pattern in 'arrange'.
      cache       = [ (k, let pair@(bars, _) = buildCacheKey voiceFn k
                          in forceAll bars `seq` pair)
                    | k <- uniqueKeys ]
      cacheForced = foldr (\(_, (b, _)) acc -> forceAll b `seq` acc) () cache
      lookupCache k = case lookup k cache of
        Just hit -> hit
        Nothing  -> let pair@(bars, _) = buildCacheKey voiceFn k
                    in forceAll bars `seq` pair
  in cacheForced `seq` (|* pF "amp" (kDynamic kin)) $
     (|* pF "amp" dyn) $
       innerJoin $ fmap (\k ->
         renderWalk (lookupCache k) chordPat stacked
       ) keyPat

-- | Cache key carrying the triad layer plus, when the source is an
-- octatripentatonic ProgressionContext, the per-bar 'ChromaSources' that
-- drive 'walkLineP'. Two contexts with identical triads but different
-- strata walks produce different lines; without the 'ChromaSources' in
-- the key the cache would silently collide.
type WalkKey = (P.Progression, Maybe [ChromaSources])

-- | Project a 'ProgressionContext' into the cache key. For 'gen'
-- (no provenance) the second component is 'Nothing' — legacy 'walkLine'
-- handles those. For 'genP' it carries the per-bar 5-PC strata and 7-PC
-- mode chroma read directly off the auxiliary layers, which after the
-- 3-5-7 fix carry the full chroma rooted on each bar's harmonic root.
walkKey :: PC.ProgressionContext -> WalkKey
walkKey ctx =
  ( PC.triadLayer ctx
  , case PC.pcProvenance ctx of
      Nothing -> Nothing
      Just _  -> Just (chromaSourcesFor ctx)
  )

-- | Read per-bar (strata, mode) absolute-PC sets from the strata / mode
-- auxiliary layers. After the Builder fix these carry 5 / 7 PCs as
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
-- shifted by 'tidalNoteOffset' so absolute MIDI from 'walkLine' aligns with
-- Tidal's @note@ convention. Dispatches on the 'ChromaSources' presence:
-- 'Just' → 'walkLineP'; 'Nothing' → legacy 'walkLine'.
buildCacheKey :: VoiceFunction -> WalkKey -> ([[Note]], Int)
buildCacheKey voiceFn (prog, mChromas) =
  let line = case mChromas of
               Nothing      -> walkLine  voiceFn prog
               Just chromas -> walkLineP voiceFn prog chromas
  in (map (map (\m -> fromIntegral (m - tidalNoteOffset))) line, length line)

-- | Map stacked beat-position events through the cached walking line.
renderWalk
  :: ([[Note]], Int)
  -> Pattern Int            -- ^ Bar-selection pattern (1-indexed)
  -> Pattern Int            -- ^ Stacked beat-index input
  -> Pattern ValueMap
renderWalk (bars, nBars) chordPat stacked
  | nBars == 0 = silence
  | otherwise =
      let chordIdx = fmap (\i -> (i - 1) `mod` nBars) chordPat

          mapped = Pattern (\st ->
            let noteEvs = query stacked st
            in concatMap (\nEv -> case whole nEv of
                 Nothing   -> []
                 Just wArc ->
                   let onsetT  = start wArc
                       ci      = lookupChordAt onsetT chordIdx
                       bar     = bars !! (ci `mod` nBars)
                       noteVal = value nEv
                       vShift  = noteVal - 1            -- 1-indexed → 0-indexed
                       idx     = vShift `mod` beatsPerBar
                       octave  = vShift `div` beatsPerBar
                   in [nEv { value = (bar !! idx) + fromIntegral (octave * 12) }]
               ) noteEvs
            ) Nothing Nothing
      in note mapped
