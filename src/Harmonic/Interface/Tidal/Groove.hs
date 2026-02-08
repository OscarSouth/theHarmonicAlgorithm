{-# LANGUAGE OverloadedStrings #-}

module Harmonic.Interface.Tidal.Groove
  ( fund
  , subKick
  ) where

import qualified Harmonic.Rules.Types.Pitch as Pitch
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Progression as P
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
-- - Repetitions/cycle length (Time value, e.g., 1, 2, etc.)
-- - Velocity/dynamics
-- - (max sub duration, sub note on pattern, sub note off pattern, kick pattern)
subKick :: (P.Progression -> [[Int]])  -- Voice strategy (bass or fund)
        -> P.Progression                -- State progression
        -> Time                         -- Repetitions/cycle length (number of bars)
        -> (Time,                       -- Note duration before off (1/1 = 1 cycle, 1/2 = half cycle)
            String,                     -- Sub note on pattern string (e.g., "[1(3,8)]")
            String,                     -- Manual note off boundary pattern string (e.g., "[0 1]*2")
            String)                     -- Kick placement pattern string (e.g., "1*4")
        -> Pattern ValueMap
subKick voiceFunc prog rep (maxDur, subOnStr, subOffStr, kickStr) =
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
    repFast p = fast (pure rep) p

    -- Sub pattern: cat cycles through chords (mirrors applyProg/arrange).
    -- Each chord gets an equal segment of the cycle.
    -- sustain 0.1 = minimal note length; CC 64 sustain pedal keeps notes alive.
    subPattern = slow (4 * pure rep) $ cat $
      map (\pitch -> struct (repFast subOnPat) $
        midinote (pure $ fromIntegral pitch) # sustain 0.1
      ) normPitches

    -- Kick pattern: fixed C4 (MIDI 60) with short sustain (one-shot, unaffected by CC 64)
    kickPattern = slow (4 * pure rep) $ struct (repFast kickPat) $
                  midinote 60 # sustain 0.01

    -- Sustain pedal: continuous CC 64 = 127.
    -- This keeps all sub bass notes sustaining until explicitly released.
    sustainOn = segment 64 $ midiCC 64 127

    -- Auto note-off: CC 64 = 0 pulse at maxDur after each note-on.
    -- Shifts the actual subOnPat by maxDur so every note-on gets
    -- a corresponding note-off. When maxDur >= 1, notes sustain for
    -- full segment (no auto-off needed). Longer shifts where on/off
    -- events cross over produce emergent groove effects.
    autoOff
      | maxDur >= 1 = silence
      | otherwise   = slow (4 * pure rep) $ cat $
          map (\_ -> struct ((pure maxDur) ~> repFast subOnPat) $
            midiCC 64 0
          ) normPitches

    -- Manual note-off: CC 64 = 0 at user-specified boundaries.
    -- subOffStr controls explicit cut points (e.g., "[0 1]" = cut on beat 3).
    manualOff = slow (4 * pure rep) $ struct (repFast subOffPat) $
                midiCC 64 0

  in stack [subPattern, kickPattern, sustainOn, autoOff, manualOff]
