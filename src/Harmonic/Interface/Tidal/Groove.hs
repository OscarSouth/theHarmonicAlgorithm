{-# LANGUAGE OverloadedStrings #-}

module Harmonic.Interface.Tidal.Groove
  ( fund
  , subKick
  ) where

import qualified Harmonic.Rules.Types.Pitch as Pitch
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Progression as P
import Harmonic.Interface.Tidal.Form (Kinetics(..), ki)
import Data.Foldable (toList)
import Sound.Tidal.Context

-- | Extract harmonic roots regardless of inversion.
-- Always returns the fundamental root note from CadenceState.
fund :: P.Progression -> [[Int]]
fund prog =
  let cadenceStates = toList (P.unProgression prog)
  in map fundToInt cadenceStates
  where
    fundToInt :: H.CadenceState -> [Int]
    fundToInt cs =
      let chord = H.fromCadenceState cs
          rootNoteName = H.chordNoteName chord
          rootPc = Pitch.pitchClass rootNoteName
      in [Pitch.unPitchClass rootPc]

-- | Normalize pitch classes to C3-B3 range (MIDI 48-59) for MPC sub program.
-- Empty list returns 47 (B2, where no sample is assigned = silence).
-- Pitch classes [0-11] map to MIDI [48-59] (C3-B3).
normalizeToSubRange :: [Int] -> Int
normalizeToSubRange [] = 47  -- B2: no sample (silence)
normalizeToSubRange (pc:_) = 48 + (pc `mod` 12)

-- | Groove interface using patterned chord selection with kinetics gating.
--
-- CC64 sustain mechanism with chord selection via @Pattern Int@.
-- Sub on\/off patterns and kick pattern are bar-relative:
-- @\"[1]/2\"@ = one onset every 2 bars, @\"1*4\"@ = 4 kicks per bar.
--
-- Chord selection uses 'innerJoin' — we WANT new note-ons when the
-- chord changes (unlike melodic instruments where sustain across
-- boundaries is desirable).
--
-- The progression is read from @kProg k@ via @innerJoin@.
-- Sub is gated at @(0.1, 1)@ and kick at @(0.2, 1)@ via @ki@.
--
-- @chordPat@ is 1-indexed: @\"[1 2 3 4]/4\"@ selects chords 1-4.
subKick :: (P.Progression -> [[Int]])  -- ^ Voice strategy (fund or bass)
        -> Pattern Int                  -- ^ Chord selection pattern (1-indexed)
        -> Pattern Double               -- ^ Dynamics pattern (> 0 = sub active)
        -> Kinetics                     -- ^ Form context
        -> (Time,                       -- ^ Max sub duration before auto-off
            String,                     -- ^ Sub note on pattern string
            String,                     -- ^ Manual note off pattern string
            String)                     -- ^ Kick placement pattern string
        -> Pattern ValueMap
subKick voiceFunc chordPat dyn k (maxDur, subOnStr, subOffStr, kickStr) =
  innerJoin $ fmap (\prog ->
    subKickCore voiceFunc prog chordPat dyn k (maxDur, subOnStr, subOffStr, kickStr)
  ) (kProg k)

-- |Internal: subKick logic with ki gating on sub/kick groups.
subKickCore :: (P.Progression -> [[Int]])
            -> P.Progression
            -> Pattern Int
            -> Pattern Double
            -> Kinetics
            -> (Time, String, String, String)
            -> Pattern ValueMap
subKickCore voiceFunc prog chordPat dyn k (maxDur, subOnStr, subOffStr, kickStr)
  | null normPitches = silence
  | otherwise =
  let
    -- Parse pattern strings
    subOnPat  = slow 4 $ parseBP_E subOnStr
    subOffPat = slow 4 $ parseBP_E subOffStr
    kickPat   = slow 4 $ parseBP_E kickStr

    -- CC helper
    midiCC num val = midicmd "control" # ctlNum num # control val

    -- Convert dynamics to boolean gate for mask
    dynGate = fmap (> 0) dyn

    -- MIDI routing: channel 7 (0-indexed = midichan 6) on "thru" device
    thru = s "thru" # midichan 6

    -- 0-indexed chord index from 1-indexed input, wrapping modulo nChords
    chordIdx = fmap (\i -> (i - 1) `mod` nChords) chordPat

    -- Sub pattern: note-ons gated by dynamics and structured by subOnPat
    subPattern = mask dynGate $ struct subOnPat $
      innerJoin (fmap (\ci ->
        midinote (pure $ fromIntegral (normPitches !! (ci `mod` nChords)))
        # sustain 0.01 # amp dyn
      ) chordIdx)

    -- Kick pattern: fixed C4 (MIDI 60), one-shot
    kickPattern = struct kickPat $ midinote 60 # sustain 0.01 # amp 1

    -- Sustain pedal: CC 64 = 127 continuous background
    -- 1/128 offset avoids timestamp collision with note-on events
    sustainOn = (1/128) ~> segment 64 (midiCC 64 127)

    -- Auto note-off: CC 64 = 0 shifted by maxDur after each note-on
    autoOff
      | maxDur >= 1 = silence
      | otherwise   = struct ((pure (maxDur * 4)) ~> subOnPat) $ midiCC 64 0

    -- Manual note-off: CC 64 = 0 at user-specified boundaries
    manualOff = struct subOffPat $ midiCC 64 0

    -- LED feedback for Keith McMillen 12 Step foot controller
    ledCC num val = midicmd "control"
                  # ctlNum (fromIntegral num)
                  # control (fromIntegral val)

    subLedOn = (1/128) ~> (mask dynGate $ struct subOnPat $
      innerJoin (fmap (\ci ->
        ledCC (normPitches !! (ci `mod` nChords) - 28) 1
      ) chordIdx))

    subLedAutoOff
      | maxDur >= 1 = silence
      | otherwise   = struct ((pure (maxDur * 4)) ~> subOnPat) $
          stack [ledCC cc 0 | cc <- [20..31]]

    subLedManualOff = struct subOffPat $
      stack [ledCC cc 0 | cc <- [20..31]]

    kickLedOn = struct kickPat $ ledCC 32 1

    kickLedOff = struct ((pure (1/32)) ~> kickPat) $ ledCC 32 0

    -- Sub group: sub pattern + CC64 sustain + sub LEDs
    subGroup = ki (0.1, 1) k $ stack
      [ subPattern # thru
      , sustainOn # thru, autoOff # thru, manualOff # thru
      , subLedOn # thru, subLedAutoOff # thru, subLedManualOff # thru
      ]

    -- Kick group: kick pattern + kick LEDs
    kickGroup = ki (0.2, 1) k $ stack
      [ kickPattern # thru
      , kickLedOn # thru, kickLedOff # thru
      ]

  in stack [subGroup, kickGroup]
  where
    rawPitches  = voiceFunc prog
    normPitches = map normalizeToSubRange rawPitches
    nChords     = length normPitches
