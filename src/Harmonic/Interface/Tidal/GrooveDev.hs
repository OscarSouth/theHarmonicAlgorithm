{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Interface.Tidal.GrooveDev
-- Description : Chord-pattern-based sub/kick groove interface
--
-- Reimplementation of 'subKick' using @Pattern Int@ for chord selection
-- instead of @Pattern Time@ repetitions. No @slow (4 * rep)@ / @fast rep@
-- / @cat@ anywhere — chord selection is driven by 'innerJoin' on the
-- chord selection pattern.

module Harmonic.Interface.Tidal.GrooveDev
  ( subKickDev
  ) where

import qualified Harmonic.Rules.Types.Progression as P
import Sound.Tidal.Context

-------------------------------------------------------------------------------
-- subKickDev: chord-pattern-based subKick
-------------------------------------------------------------------------------

-- |Groove interface using patterned chord selection.
--
-- Same CC64 sustain mechanism as 'subKick', but chord selection via
-- @Pattern Int@ instead of @slow (4 * rep) $ cat@.
--
-- Sub on/off patterns and kick pattern run at natural speed.
-- Chord selection uses 'innerJoin' — we WANT new note-ons when the
-- chord changes (unlike melodic instruments where sustain across
-- boundaries is desirable).
--
-- @chordPat@ is 1-indexed: @\"[1 2 3 4]/4\"@ selects chords 1–4.
subKickDev :: (P.Progression -> [[Int]])  -- ^ Voice strategy (fund or bass)
           -> P.Progression               -- ^ Progression
           -> Pattern Int                  -- ^ Chord selection pattern (1-indexed)
           -> Pattern Double               -- ^ Dynamics pattern (> 0 = sub active)
           -> (Time,                       -- ^ Max sub duration before auto-off
               String,                     -- ^ Sub note on pattern string
               String,                     -- ^ Manual note off pattern string
               String)                     -- ^ Kick placement pattern string
           -> Pattern ValueMap
subKickDev voiceFunc prog chordPat dyn (maxDur, subOnStr, subOffStr, kickStr)
  | null normPitches = silence
  | otherwise =
  let
    -- Parse pattern strings
    subOnPat  = parseBP_E subOnStr
    subOffPat = parseBP_E subOffStr
    kickPat   = parseBP_E kickStr

    -- CC helper
    midiCC num val = midicmd "control" # ctlNum num # control val

    -- Convert dynamics to boolean gate for mask
    dynGate = fmap (> 0) dyn

    -- MIDI routing: channel 7 (0-indexed = midichan 6) on "thru" device
    thru = s "thru" # midichan 6

    -- 0-indexed chord index from 1-indexed input, wrapping modulo nChords
    chordIdx = fmap (\i -> (i - 1) `mod` nChords) chordPat

    -- Sub pitch pattern: innerJoin maps chord index to MIDI note
    pitchPat = fmap (\ci -> fromIntegral (normPitches !! (ci `mod` nChords))) chordIdx

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
      | otherwise   = struct ((pure maxDur) ~> subOnPat) $ midiCC 64 0

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
      | otherwise   = struct ((pure maxDur) ~> subOnPat) $
          stack [ledCC cc 0 | cc <- [20..31]]

    subLedManualOff = struct subOffPat $
      stack [ledCC cc 0 | cc <- [20..31]]

    kickLedOn = struct kickPat $ ledCC 32 1

    kickLedOff = struct ((pure (1/32)) ~> kickPat) $ ledCC 32 0

  in stack [ subPattern # thru, kickPattern # thru
           , sustainOn # thru, autoOff # thru, manualOff # thru
           , subLedOn # thru, subLedAutoOff # thru
           , subLedManualOff # thru, kickLedOn # thru, kickLedOff # thru
           ]
  where
    rawPitches  = voiceFunc prog
    normPitches = map normalizeToSubRange rawPitches
    nChords     = length normPitches

-- |Normalize pitch classes to C3-B3 range (MIDI 48-59) for MPC sub program.
normalizeToSubRange :: [Int] -> Int
normalizeToSubRange [] = 47  -- B2: no sample (silence)
normalizeToSubRange (pc:_) = 48 + (pc `mod` 12)
