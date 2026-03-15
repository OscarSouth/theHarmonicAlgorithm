{-# LANGUAGE OverloadedStrings #-}

module Harmonic.Interface.Tidal.Groove
  ( fund
  , subKick
  ) where

import qualified Harmonic.Rules.Types.Pitch as Pitch
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Progression as P
import Harmonic.Interface.Tidal.Utils (onset)
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

-- | Groove interface for MPC drum/sub program (MIDI channel 1).
-- C3-B3 (MIDI 48-59): Sub bass (sustained, note on/off controlled)
-- C4 (MIDI 60): Kick drum (one-shot)
--
-- Takes:
-- - Voice strategy (bass or fund)
-- - Progression state
-- - Repetitions/cycle length (patternable, e.g. 1, "1 0.5")
-- - Velocity/dynamics
-- - (max sub duration, sub note on pattern, sub note off pattern, kick pattern)
subKick :: (P.Progression -> [[Int]])  -- Voice strategy (bass or fund)
        -> P.Progression                -- State progression
        -> Pattern Time                 -- Repetitions/cycle length (patternable)
        -> Pattern Double               -- Dynamics pattern (> 0 = sub active, 0 = sub gated)
        -> (Time,                       -- Note duration before off (1/1 = 1 cycle, 1/2 = half cycle)
            String,                     -- Sub note on pattern string (e.g., "[1(3,8)]")
            String,                     -- Manual note off boundary pattern string (e.g., "[0 1]*2")
            String)                     -- Kick placement pattern string (e.g., "1*4")
        -> Pattern ValueMap
subKick voiceFunc prog rep dyn (maxDur, subOnStr, subOffStr, kickStr) =
  let
    -- Extract and normalize pitches
    rawPitches = voiceFunc prog
    normPitches = map normalizeToSubRange rawPitches

    -- Parse pattern strings
    subOnPat  = parseBP_E subOnStr
    subOffPat = parseBP_E subOffStr
    kickPat   = parseBP_E kickStr

    -- CC helper using proven midicmd approach (ccn/ccv doesn't work)
    midiCC num val = midicmd "control" # ctlNum num # control val

    -- Compensate inner patterns for rep factor so pattern speed
    -- stays constant per bar regardless of rep (matches arrange behavior).
    -- fast rep counteracts the rep portion of slow (4 * rep).
    repFast p = fast rep p

    -- Convert dynamics pattern to boolean gate for mask.
    -- Allows rhythmic gating: dyn "1 0 1 0" plays sub on beats 1,3 only.
    dynGate = fmap (> 0) dyn

    -- MIDI routing: channel 7 (0-indexed = midichan 6) on "thru" device
    thru = s "thru" # midichan 6

    -- Sub pattern: cat cycles through chords (mirrors applyProg/arrange).
    -- Each chord gets an equal segment of the cycle.
    -- Wrapped with onset to ensure multi-cycle struct patterns produce
    -- proper note-onsets in every cat window.
    -- Gated by dynGate: note-ons only fire where dyn > 0.
    -- Note-offs (autoOff/manualOff) always fire to ensure correct note kills.
    subPattern = mask dynGate $ slow (4 * rep) $ cat $
      map (\pitch -> onset $ struct (repFast subOnPat) $
        midinote (pure $ fromIntegral pitch) # sustain 0.01 # amp (fmap (/127) dyn)
      ) normPitches

    -- Kick pattern: fixed C4 (MIDI 60) with short sustain (one-shot, unaffected by CC 64)
    kickPattern = slow (4 * rep) $ struct (repFast kickPat) $
                  midinote 60 # sustain 0.01 # amp 1

    -- Sustain pedal: CC 64 = 127 continuous background stream.
    -- 1/128 offset places CC64=127 at odd multiples of 1/128 (1/128, 3/128, ...),
    -- which never collide with note-on, autoOff, or manualOff events (all at
    -- even fractions). This eliminates the timestamp race condition where
    -- CC64=127 and CC64=0 (or note-on) share a logical timestamp, causing
    -- nondeterministic MIDI event ordering.
    -- NOT gated by dynGate — continuous background regardless of dynamics.
    sustainOn = (1/128) ~> segment 64 (midiCC 64 127)

    -- Auto note-off: CC 64 = 0 pulse at maxDur after each note-on.
    -- Shift (maxDur/rep) in cat-item coordinates gives constant outer delay
    -- of 4*maxDur cycles. Clamped to (1 - 1/128) to prevent wrap-around
    -- when rep <= maxDur (note sustains for full chord in that case).
    -- When maxDur >= 1, notes sustain for full segment (no auto-off needed).
    -- Always fires regardless of dynGate to ensure notes are killed.
    autoOff
      | maxDur >= 1 = silence
      | otherwise   = slow (4 * rep) $ cat $
          map (\_ -> onset $ struct (fmap (\r -> min (maxDur / r) (1 - 1/128)) rep ~> repFast subOnPat) $
            midiCC 64 0
          ) normPitches

    -- Manual note-off: CC 64 = 0 at user-specified boundaries.
    -- subOffStr controls explicit cut points (e.g., "[0 1]" = cut on beat 3).
    -- Always fires regardless of dynGate to ensure notes are killed.
    manualOff = slow (4 * rep) $ struct (repFast subOffPat) $
                midiCC 64 0

    -- LED feedback for Keith McMillen 12 Step foot controller.
    -- CCs 20-32 control key LEDs (value 0 = off, 1 = on).
    -- CC = midiNote - 28 (MIDI 48 -> CC 20, MIDI 60 -> CC 32).
    -- Routed via MPC (same port/channel as notes); MPC forwards to 12 Step.
    ledCC num val = midicmd "control"
                  # ctlNum (fromIntegral num)
                  # control (fromIntegral val)

    subLedOn = (1/128) ~> (mask dynGate $ slow (4 * rep) $ cat $
      map (\pitch -> onset $ struct (repFast subOnPat) $
        ledCC (pitch - 28) 1
      ) normPitches)

    subLedAutoOff
      | maxDur >= 1 = silence
      | otherwise   = slow (4 * rep) $ cat $
          map (\_ -> onset $ struct (fmap (\r -> min (maxDur / r) (1 - 1/128)) rep ~> repFast subOnPat) $
            stack [ledCC n 0 | n <- [20..31]]
          ) normPitches

    subLedManualOff = slow (4 * rep) $ struct (repFast subOffPat) $
      stack [ledCC n 0 | n <- [20..31]]

    kickLedOn = slow (4 * rep) $ struct (repFast kickPat) $
      ledCC 32 1

    kickLedOff = slow (4 * rep) $ struct ((pure (1/32)) ~> repFast kickPat) $
      ledCC 32 0

  in stack [ subPattern # thru, kickPattern # thru
           , sustainOn # thru, autoOff # thru, manualOff # thru
           , subLedOn # thru, subLedAutoOff # thru
           , subLedManualOff # thru, kickLedOn # thru, kickLedOff # thru
           ]
