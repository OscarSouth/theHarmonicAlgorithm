-- |
-- Module      : Harmonic.Interface.Tidal.Devices.P6
-- Description : Roland AIRA P-6 Creative Sampler control map
--
-- Preset selection, pad triggering and the full granular-engine control set.
-- CC numbers are quoted from the official Roland P-6 MIDI chart.
--
-- All three play channels are reassigned from the device defaults, so the
-- device must be configured to match:
--
-- @
-- Auto CH = 3   keyboard \/ chromatic sample playback (device default 15)
-- S.CH    = 4   global sample trigger, 48 pads at default pitch (default 11)
-- G.CH    = 5   granular engine, notes and CC (device default 4)
-- Program = 16  preset select, fixed on the device
-- @
module Harmonic.Interface.Tidal.Devices.P6
  (
    -- * Presets, pads and granular controls
    p6prog, p6note, p6trig, p6pad, p6grainSize, p6headPos, p6headSpeed,
    p6grains, p6grainShape, p6spread, p6grainJitter, p6grainRev,
    p6grainTimeKF, p6coarseTune, p6fineTune, p6detune, p6filterType,
    p6cutoff, p6res, p6filterEnv, p6filterKF, p6filterVel, p6attack,
    p6decay, p6sustain, p6release, p6envMode, p6envTimeKF, p6ampSwitch,
    p6startMode, p6level, p6pan, p6autoPan, p6levelJitter, p6lofi,
    p6lofiSw, p6sample, p6outputBus, p6sendDelay, p6sendReverb, p6revTime,
    p6delTime, p6revLevel, p6delLevel
  ) where

import Sound.Tidal.Context
import Harmonic.Interface.Tidal.Instruments (ch)

-- A MIDI control-change line: a 0-1 value on the wire's 0-127 range. Tidal's
-- own @cc@ is a SuperDirt string parameter, a different thing entirely, so
-- this stays local rather than becoming an ambiguous export.
ctl :: Pattern Double -> Pattern Double -> ControlPattern
ctl num v = control (v * 127) # midicmd "control" # ctlNum num

-- | Sample triggering answers on channel 4, the granular engine on 5.
p6trigChan, p6granChan :: Pattern ValueMap
p6trigChan = ch 4
p6granChan = ch 5

-- Preset selection (ch 16, range 0-63)
-- | Select a preset, 0-63, on the program channel.
p6prog :: Pattern Double -> ControlPattern
p6prog p = midicmd "program" #progNum p # ch 16

-- Notes 48-95 map to bank A pad 1 through bank H pad 6 (8 banks × 6 pads)
-- | The MIDI note for bank @b@, pad @n@: notes 48-95 map bank A pad 1
-- through bank H pad 6, eight banks of six.
p6note :: Int -> Int -> Int
-- | Trigger bank @b@, pad @n@ on a boolean rhythm.
p6trig :: Int -> Int -> Pattern Bool -> ControlPattern
-- | Trigger pad @n@ counting straight through all 48, ignoring banks.
p6pad :: Int -> Pattern Bool -> ControlPattern
p6note b pad   = 48 + (b-1)*6 + (pad-1)
p6trig b pad pat = struct pat $ midinote (fromIntegral (p6note b pad)) # p6trigChan # sustain 0.1
p6pad  pad pat = struct pat $ midinote (fromIntegral (48+pad))     # p6trigChan # sustain 0.1

-- | The grain engine proper: size, head position and speed, grain count,
-- shape, spread, jitter, reverse and time key-follow.
p6grainSize, p6headPos, p6headSpeed, p6grains, p6grainShape,
  p6spread, p6grainJitter, p6grainRev, p6grainTimeKF :: Pattern Double -> ControlPattern
p6grainSize v   = ctl 23  v # p6granChan
p6headPos v     = ctl 19  v # p6granChan
p6headSpeed v   = ctl 20  v # p6granChan
p6grains v      = ctl 21  v # p6granChan
p6grainShape v  = ctl 15  v # p6granChan
p6spread v      = ctl 25  v # p6granChan
p6grainJitter v = ctl 68  v # p6granChan
p6grainRev v    = ctl 3   v # p6granChan
p6grainTimeKF v = ctl 16  v # p6granChan

-- | Coarse and fine tuning, and detune spread across grains.
p6coarseTune, p6fineTune, p6detune :: Pattern Double -> ControlPattern
p6coarseTune v  = ctl 76  v # p6granChan
p6fineTune v    = ctl 18  v # p6granChan
p6detune v      = ctl 13  v # p6granChan

-- | Filter section: type, cutoff, resonance, envelope depth, key follow
-- and velocity sensitivity.
p6filterType, p6cutoff, p6res, p6filterEnv, p6filterKF, p6filterVel :: Pattern Double -> ControlPattern
p6filterType v  = ctl 12  v # p6granChan
p6cutoff v      = ctl 74  v # p6granChan
p6res v         = ctl 71  v # p6granChan
p6filterEnv v   = ctl 24  v # p6granChan
p6filterKF v    = ctl 26  v # p6granChan
p6filterVel v   = ctl 78  v # p6granChan

-- | The amplitude envelope: four stages, mode, time key-follow, amp switch
-- and start mode.
p6attack, p6decay, p6sustain, p6release, p6envMode, p6envTimeKF,
  p6ampSwitch, p6startMode :: Pattern Double -> ControlPattern
p6attack v      = ctl 73  v # p6granChan
p6decay v       = ctl 75  v # p6granChan
p6sustain v     = ctl 30  v # p6granChan
p6release v     = ctl 72  v # p6granChan
p6envMode v     = ctl 29  v # p6granChan
p6envTimeKF v   = ctl 77  v # p6granChan
p6ampSwitch v   = ctl 28  v # p6granChan
p6startMode v   = ctl 79  v # p6granChan

-- | Level, pan, auto-pan and per-grain level jitter.
p6level, p6pan, p6autoPan, p6levelJitter :: Pattern Double -> ControlPattern
p6level v       = ctl 7   v # p6granChan
p6pan v         = ctl 10  v # p6granChan
p6autoPan v     = ctl 9   v # p6granChan
p6levelJitter v = ctl 14  v # p6granChan

-- | The lo-fi stage and its switch.
p6lofi, p6lofiSw :: Pattern Double -> ControlPattern
p6lofi v        = ctl 17  v # p6granChan
p6lofiSw v      = ctl 87  v # p6granChan

-- | Sample choice and output routing, including the two effect sends.
p6sample, p6outputBus, p6sendDelay, p6sendReverb :: Pattern Double -> ControlPattern
p6sample v      = ctl 88  v # p6granChan
p6outputBus v   = ctl 84  v # p6granChan
p6sendDelay v   = ctl 85  v # p6granChan
p6sendReverb v  = ctl 86  v # p6granChan

-- | The onboard reverb and delay.
p6revTime, p6delTime, p6revLevel, p6delLevel :: Pattern Double -> ControlPattern
p6revTime v     = ctl 89  v # p6granChan
p6delTime v     = ctl 90  v # p6granChan
p6revLevel v    = ctl 91  v # p6granChan
p6delLevel v    = ctl 92  v # p6granChan

