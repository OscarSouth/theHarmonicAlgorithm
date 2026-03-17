{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Harmonic.Interface.Tidal.BridgeDev
-- Description : Patterned chord selection for TidalCycles
--
-- Clean-sheet reimplementation of chord-to-pattern mapping using
-- @Pattern Int@ for chord selection instead of @Pattern Time@ repetitions.
--
-- Three variants:
--
-- * 'arrangeDev' — onset-join: each note maps through the chord active at
--   its onset time. Sustained notes keep their pitch across chord boundaries.
--
-- * 'squeezeDev' — squeeze: each chord slot gets the full input pattern
--   compressed to fit. Pattern restarts per chord.
--
-- * 'arrangeStrict' — innerJoin: creates new note-ons at chord boundaries.
--   Preserves current @arrange@ behaviour with patterned chord selection.

module Harmonic.Interface.Tidal.BridgeDev
  ( -- * Primary arrangement functions
    arrangeDev
  , squeezeDev
  , arrangeStrict

    -- * Chord pattern helpers
  , lookupChordAt
  , repToChordPat
  ) where

import qualified Harmonic.Rules.Types.Progression as P
import Harmonic.Interface.Tidal.Bridge (VoiceFunction, voiceRange)

import Sound.Tidal.Context

-------------------------------------------------------------------------------
-- Core: lookupChordAt
-------------------------------------------------------------------------------

-- |Point-query a chord selection pattern at a specific time.
-- Returns the chord index (0-indexed) active at time @t@.
-- Uses an epsilon-width query to handle exact boundary times.
-- Falls back to chord 0 if no events found.
lookupChordAt :: Time -> Pattern Int -> Int
lookupChordAt t cpat =
  case queryArc cpat (Arc t (t + 1/10000000)) of
    []    -> 0
    (e:_) -> value e

-------------------------------------------------------------------------------
-- arrangeDev: onset-join (solves all 3 limitations)
-------------------------------------------------------------------------------

-- |Map notes through chords using onset-time lookup.
--
-- Each note event is mapped through the chord that is active at its onset
-- time. Notes that sustain through chord boundaries keep their onset-time
-- pitch — no spurious re-triggering.
--
-- @chordPat@ is 1-indexed: @\"[1 2 3 4]/4\"@ selects chords 1–4.
-- Indices wrap modulo the number of chords in the progression.
--
-- Example:
--
-- @
-- let chordSel = parseBP_E \"[1 2 3 4]/4\" :: Pattern Int
-- arrangeDev flow prog chordSel (-24,24) [\"[0 1 2 3]\"]
-- @
arrangeDev :: VoiceFunction
           -> P.Progression
           -> Pattern Int        -- ^ Chord selection pattern (1-indexed)
           -> (Int, Int)         -- ^ MIDI note range filter
           -> [Pattern Int]      -- ^ Input patterns to harmonize
           -> Pattern ValueMap
arrangeDev voiceFunc prog chordPat register pats
  | null voicings = silence
  | otherwise =
      let chordIdx = fmap (\i -> (i - 1) `mod` nChords) chordPat

          mapped = Pattern (\st ->
            let noteEvs = query ranged st
            in concatMap (\nEv -> case whole nEv of
              Nothing -> []
              Just wArc ->
                let onsetT  = start wArc
                    ci      = lookupChordAt onsetT chordIdx
                    sc      = scales !! (ci `mod` nChords)
                    noteVal = value nEv
                    scLen   = max 1 (length sc)
                    octave  = noteVal `div` scLen
                    idx     = noteVal `mod` scLen
                in [nEv { value = (sc !! idx) + fromIntegral (octave * 12) }]
              ) noteEvs
            ) Nothing Nothing

      in note mapped
  where
    voicings = voiceFunc prog
    scales   = map (map fromIntegral) voicings :: [[Note]]
    nChords  = length voicings
    stacked  = stack pats
    ranged   = voiceRange register stacked

-------------------------------------------------------------------------------
-- squeezeDev: squeeze variant (solves limitations 1 & 3)
-------------------------------------------------------------------------------

-- |Map notes through chords using squeeze (pattern restart per chord).
--
-- Each chord slot gets the full input pattern compressed to fit its
-- time span. Patterns restart for each chord — no temporal distortion,
-- and each chord sees the complete pattern.
--
-- @chordPat@ is 1-indexed, same as 'arrangeDev'.
squeezeDev :: VoiceFunction
           -> P.Progression
           -> Pattern Int        -- ^ Chord selection pattern (1-indexed)
           -> (Int, Int)         -- ^ MIDI note range filter
           -> [Pattern Int]      -- ^ Input patterns to harmonize
           -> Pattern ValueMap
squeezeDev voiceFunc prog chordPat register pats
  | null voicings = silence
  | otherwise =
      let chordIdx  = fmap (\i -> (i - 1) `mod` nChords) chordPat
          chordPats = map (\sc -> note (toScale sc ranged)) scales
      in squeeze chordIdx chordPats
  where
    voicings = voiceFunc prog
    scales   = map (map fromIntegral) voicings :: [[Note]]
    nChords  = length voicings
    stacked  = stack pats
    ranged   = voiceRange register stacked

-------------------------------------------------------------------------------
-- arrangeStrict: innerJoin variant (current behaviour, new paradigm)
-------------------------------------------------------------------------------

-- |Map notes through chords using innerJoin (re-trigger at boundaries).
--
-- Creates new note-ons at chord boundaries — the same behaviour as the
-- legacy @arrange@ function, but with patterned chord selection instead
-- of @rep@.
--
-- @chordPat@ is 1-indexed, same as 'arrangeDev'.
arrangeStrict :: VoiceFunction
              -> P.Progression
              -> Pattern Int        -- ^ Chord selection pattern (1-indexed)
              -> (Int, Int)         -- ^ MIDI note range filter
              -> [Pattern Int]      -- ^ Input patterns to harmonize
              -> Pattern ValueMap
arrangeStrict voiceFunc prog chordPat register pats
  | null voicings = silence
  | otherwise =
      let chordIdx = fmap (\i -> (i - 1) `mod` nChords) chordPat
          nested   = fmap (\ci -> note (toScale (scales !! (ci `mod` nChords)) ranged)) chordIdx
      in innerJoin nested
  where
    voicings = voiceFunc prog
    scales   = map (map fromIntegral) voicings :: [[Note]]
    nChords  = length voicings
    stacked  = stack pats
    ranged   = voiceRange register stacked

-------------------------------------------------------------------------------
-- repToChordPat: backwards compatibility helper
-------------------------------------------------------------------------------

-- |Convert old-style @(nChords, rep)@ to a chord selection pattern.
--
-- @repToChordPat 4 1@ produces the equivalent of @\"[1 2 3 4]/4\"@:
-- each of 4 chords gets 1 cycle.
--
-- @repToChordPat 4 0.5@ is equivalent to @\"[1 2 3 4]/2\"@:
-- chords cycle twice as fast.
repToChordPat :: Int -> Pattern Time -> Pattern Int
repToChordPat n rep = slow (fromIntegral n * rep) $ fastcat $ map pure [1..n]
