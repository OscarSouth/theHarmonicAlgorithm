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
--   * Events are masked by the kinetics signal in (0.1, 1) and scaled by
--     'kDynamic' and the user-supplied dynamics scalar.
--
-- Each integer in a pattern selects a 1-indexed beat position (1..4).
-- Values outside [1..4] shift by full octaves, matching the div/mod convention
-- of 'arrange' (e.g. 5 → beat 1 +12, -1 → beat 4 -12).
module Harmonic.Interface.Tidal.LineHarmony
  ( lineHarmony
  ) where

import qualified Harmonic.Rules.Types.Progression as P
import Harmonic.Interface.Tidal.Form (Kinetics(..), IK)
import Harmonic.Interface.Tidal.Bridge (VoiceFunction, lookupChordAt)
import Harmonic.Traversal.WalkingBass
  ( walkLine, beatsPerBar )

import Data.List (nub)
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
-- Entropy is derived internally from the progression's harmonic character.
lineHarmony
  :: Pattern Double       -- ^ Dynamics scalar (amp multiplier)
  -> IK                    -- ^ Performance context (kinetics + chord-selection)
  -> VoiceFunction         -- ^ Beat-1 voicing (fund or root)
  -> [Pattern Int]         -- ^ Polyphonic layers (1-indexed beat positions)
  -> Pattern ValueMap
lineHarmony dyn (kin, chordPat) voiceFn pats =
  let stacked     = stack pats
      allEvents   = queryArc (kProg kin) (Arc 0 1000)
      uniqueProgs = nub (map value allEvents)
      cache       = [ (p, buildCache voiceFn p) | p <- uniqueProgs ]
      lookupCache prog = case lookup prog cache of
        Just hit -> hit
        Nothing  -> buildCache voiceFn prog
  in (|* pF "amp" (kDynamic kin)) $
     (|* pF "amp" dyn) $
     mask (fmap (\x -> x >= 0.1 && x <= 1) (kSignal kin)) $
       innerJoin $ fmap (\prog ->
         renderWalk (lookupCache prog) chordPat stacked
       ) (kProg kin)

-- | Pre-compute walking line for a single progression; convert to 'Note',
-- shifted by 'tidalNoteOffset' so absolute MIDI from 'walkLine' aligns with
-- Tidal's @note@ convention.
buildCache :: VoiceFunction -> P.Progression -> ([[Note]], Int)
buildCache voiceFn prog =
  let line = walkLine voiceFn prog
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
