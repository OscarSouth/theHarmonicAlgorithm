{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Interface.Tidal.Groove
-- Description : Rhythm section interface for sub-bass and kick patterns
--
-- Provides 'subKick' (sub-bass with CC64 sustain pedal) and 'fund'
-- (fundamental bass note extraction) for rhythm-section integration
-- with harmonically-generated progressions.

module Harmonic.Interface.Tidal.Groove
  ( fund
  , subKick
  ) where

import qualified Harmonic.Rules.Types.Pitch as Pitch
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Progression as P
import Harmonic.Interface.Tidal.Form (Kinetics(..), IK, ki)
import Data.List (nub)
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

-- | Normalize pitch classes to C2-B2 range (MIDI 36-47) for MPC sub program.
-- Empty list returns 35 (B1, where no sample is assigned = silence).
-- Pitch classes [0-11] map to MIDI [36-47] (C2-B2).
normalizeToSubRange :: [Int] -> Int
normalizeToSubRange [] = 35  -- B1: no sample (silence)
normalizeToSubRange (pc:_) = 36 + (pc `mod` 12)

-- | Groove interface using patterned chord selection with kinetics gating.
--
-- CC64 sustain mechanism with chord selection from 'IK'.
-- Sub on\/off patterns and kick pattern are bar-relative:
-- @\"[1]/2\"@ = one onset every 2 bars, @\"1*4\"@ = 4 kicks per bar.
--
-- Chord selection uses 'innerJoin' — we WANT new note-ons when the
-- chord changes (unlike melodic instruments where sustain across
-- boundaries is desirable).
--
-- The progression is read from @kProg@ via @innerJoin@.
-- Sub is gated at @(0.1, 1)@ and kick at @(0.2, 1)@ via @ki@.
subKick :: Pattern Double               -- ^ Dynamics pattern (> 0 = sub active)
        -> IK                            -- ^ Performance context (kinetics + chord selection)
        -> (P.Progression -> [[Int]])    -- ^ Voice strategy (fund or bass)
        -> (Time,                        -- ^ Max sub duration before auto-off
            String,                      -- ^ Sub note on pattern string
            String,                      -- ^ Manual note off pattern string
            String)                      -- ^ Kick placement pattern string
        -> Pattern ValueMap
subKick dyn k voiceFunc (maxDur, subOnStr, subOffStr, kickStr) =
  let (kin, chordPat) = k
      -- Parse pattern strings ONCE at construction time (not per progression change)
      subOnPat  = slow 4 $ parseBP_E subOnStr
      subOffPat = slow 4 $ parseBP_E subOffStr
      kickPat   = slow 4 $ parseBP_E kickStr
      -- Pre-compute LED all-off stack (constant, shared across invocations)
      ledAllOff = stack [midicmd "control" # ctlNum (fromIntegral cc)
                         # control (fromIntegral (0 :: Int)) | cc <- [20..31 :: Int]]
      -- Pre-compute voicings at construction time
      allEvents = queryArc (kProg kin) (Arc 0 1000)
      uniqueProgs = nub (map value allEvents)
      cache = [(p, let raw = voiceFunc p
                       norm = map normalizeToSubRange raw
                       nc = length norm
                   in (norm, nc))
              | p <- uniqueProgs]
      lookupCache prog = case lookup prog cache of
        Just hit -> hit
        Nothing  -> let raw = voiceFunc prog
                    in (map normalizeToSubRange raw, length raw)
  in innerJoin $ fmap (\prog ->
       subKickCoreP (lookupCache prog) subOnPat subOffPat kickPat ledAllOff chordPat dyn k maxDur
     ) (kProg kin)

-- |Internal: subKick logic with ki gating on sub/kick groups.
-- Accepts pre-parsed patterns and pre-computed LED all-off from subKick outer level.
subKickCore :: (P.Progression -> [[Int]])
            -> P.Progression
            -> Pattern Bool             -- ^ Pre-parsed sub on pattern
            -> Pattern Bool             -- ^ Pre-parsed sub off pattern
            -> Pattern Bool             -- ^ Pre-parsed kick pattern
            -> ControlPattern           -- ^ ledAllOff (pre-computed)
            -> Pattern Int              -- ^ Chord selection pattern
            -> Pattern Double           -- ^ Dynamics
            -> IK
            -> Time                     -- ^ Max sub duration
            -> Pattern ValueMap
subKickCore voiceFunc prog subOnPat subOffPat kickPat ledAllOff chordPat dyn k maxDur
  | null normPitches = silence
  | otherwise =
  let
    -- CC helper
    midiCC num val = midicmd "control" # ctlNum num # control val

    -- LED feedback helper
    ledCC num val = midicmd "control"
                  # ctlNum (fromIntegral num)
                  # control (fromIntegral val)

    -- Convert dynamics to boolean gate for mask
    dynGate = fmap (> 0) dyn

    -- MIDI routing: channel 10 (0-indexed = midichan 9) on "thru" device
    thru = s "thru" # midichan 9

    -- 0-indexed chord index from 1-indexed input, wrapping modulo nChords
    chordIdx = fmap (\i -> (i - 1) `mod` nChords) chordPat

    -- Sub pattern: note-ons gated by dynamics and structured by subOnPat
    subPattern = mask dynGate $ struct subOnPat $
      innerJoin (fmap (\ci ->
        midinote (pure $ fromIntegral (normPitches !! (ci `mod` nChords)))
        # sustain 0.01 # amp dyn
      ) chordIdx)

    -- Kick pattern: fixed C3 (MIDI 48), one-shot
    kickPattern = struct kickPat $ midinote 48 # sustain 0.01 # amp 1

    -- Sustain pedal: CC 64 = 127 continuous background
    -- 1/128 offset avoids timestamp collision with note-on events
    sustainOn = (1/128) ~> segment 16 (midiCC 64 127)

    -- Auto note-off: CC 64 = 0 shifted by maxDur after each note-on
    autoOff
      | maxDur >= 1 = silence
      | otherwise   = struct ((pure (maxDur * 4)) ~> subOnPat) $ midiCC 64 0

    -- Manual note-off: CC 64 = 0 at user-specified boundaries
    manualOff = struct subOffPat $ midiCC 64 0

    subLedOn = (1/128) ~> (mask dynGate $ struct subOnPat $
      innerJoin (fmap (\ci ->
        ledCC (normPitches !! (ci `mod` nChords) - 16) 1
      ) chordIdx))

    subLedAutoOff
      | maxDur >= 1 = silence
      | otherwise   = struct ((pure (maxDur * 4)) ~> subOnPat) $ ledAllOff

    subLedManualOff = struct subOffPat $ ledAllOff

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

    -- Pedal up: CC64=0 when sub is inactive (kinetics below threshold)
    -- Resets physical instrument to default touch behaviour
    pedalUp = mask (fmap (< 0.1) (kSignal (fst k))) $ segment 1 (midiCC 64 0) # thru

  in stack [subGroup, kickGroup, pedalUp]
  where
    rawPitches  = voiceFunc prog
    normPitches = map normalizeToSubRange rawPitches
    nChords     = length normPitches

-- |Cached subKick: takes pre-computed (normPitches, nChords) and pre-parsed patterns.
-- All CC64/sustain/timing logic identical to subKickCore.
subKickCoreP :: ([Int], Int)
             -> Pattern Bool             -- ^ Pre-parsed sub on pattern
             -> Pattern Bool             -- ^ Pre-parsed sub off pattern
             -> Pattern Bool             -- ^ Pre-parsed kick pattern
             -> ControlPattern           -- ^ ledAllOff (pre-computed)
             -> Pattern Int              -- ^ Chord selection pattern
             -> Pattern Double           -- ^ Dynamics
             -> IK
             -> Time                     -- ^ Max sub duration
             -> Pattern ValueMap
subKickCoreP (normPitches, nChords) subOnPat subOffPat kickPat ledAllOff chordPat dyn k maxDur
  | nChords == 0 = silence
  | otherwise =
  let
    -- CC helper
    midiCC num val = midicmd "control" # ctlNum num # control val

    -- LED feedback helper
    ledCC num val = midicmd "control"
                  # ctlNum (fromIntegral num)
                  # control (fromIntegral val)

    -- Convert dynamics to boolean gate for mask
    dynGate = fmap (> 0) dyn

    -- MIDI routing: channel 10 (0-indexed = midichan 9) on "thru" device
    thru = s "thru" # midichan 9

    -- 0-indexed chord index from 1-indexed input, wrapping modulo nChords
    chordIdx = fmap (\i -> (i - 1) `mod` nChords) chordPat

    -- Sub pattern: note-ons gated by dynamics and structured by subOnPat
    subPattern = mask dynGate $ struct subOnPat $
      innerJoin (fmap (\ci ->
        midinote (pure $ fromIntegral (normPitches !! (ci `mod` nChords)))
        # sustain 0.01 # amp dyn
      ) chordIdx)

    -- Kick pattern: fixed C3 (MIDI 48), one-shot
    kickPattern = struct kickPat $ midinote 48 # sustain 0.01 # amp 1

    -- Sustain pedal: CC 64 = 127 continuous background
    -- 1/128 offset avoids timestamp collision with note-on events
    sustainOn = (1/128) ~> segment 16 (midiCC 64 127)

    -- Auto note-off: CC 64 = 0 shifted by maxDur after each note-on
    autoOff
      | maxDur >= 1 = silence
      | otherwise   = struct ((pure (maxDur * 4)) ~> subOnPat) $ midiCC 64 0

    -- Manual note-off: CC 64 = 0 at user-specified boundaries
    manualOff = struct subOffPat $ midiCC 64 0

    subLedOn = (1/128) ~> (mask dynGate $ struct subOnPat $
      innerJoin (fmap (\ci ->
        ledCC (normPitches !! (ci `mod` nChords) - 16) 1
      ) chordIdx))

    subLedAutoOff
      | maxDur >= 1 = silence
      | otherwise   = struct ((pure (maxDur * 4)) ~> subOnPat) $ ledAllOff

    subLedManualOff = struct subOffPat $ ledAllOff

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

    -- Pedal up: CC64=0 when sub is inactive (kinetics below threshold)
    -- Resets physical instrument to default touch behaviour
    pedalUp = mask (fmap (< 0.1) (kSignal (fst k))) $ segment 1 (midiCC 64 0) # thru

  in stack [subGroup, kickGroup, pedalUp]
