-- |
-- Module      : Harmonic.Interface.Tidal.Devices.S1
-- Description : Roland AIRA S-1 Tweak Synthesizer control map
--
-- Every parameter the S-1 exposes over MIDI, as a control function taking a
-- 0-1 value. CC numbers are quoted from the official Roland chart.
--
-- The synth answers on MIDI channel 6, baked into each function -- set the
-- S-1 to channel 6 on the device. Patterns compose as usual:
--
-- @, s1cutoff (lfo saw 0.2 0.9)@
module Harmonic.Interface.Tidal.Devices.S1
  (
    -- * Controls
    s1pat, s1seq, s1modwheel, s1portatime, s1pan, s1expression,
    s1portamode, s1damper, s1portasw, s1finetune, s1transpose, s1osclfo,
    s1oscrange, s1oscpwm, s1oscpwmsrc, s1oscbend, s1square, s1saw, s1sub,
    s1suboct, s1noise, s1noisemode, s1cutoff, s1res, s1filterenv,
    s1filterlfo, s1keytrack, s1filterbend, s1ampmode, s1lforate,
    s1lfowave, s1lfomod, s1lfomode, s1lfokeytrg, s1lfosync, s1attack,
    s1decay, s1sustain, s1release, s1envtrig, s1polymode, s1voice2sw,
    s1voice3sw, s1voice4sw, s1voice2shift, s1voice3shift, s1voice4shift,
    s1revlvl, s1revtime, s1dellvl, s1deltime, s1chorus, s1drawsw,
    s1drawmul, s1overtone, s1chopcomb
  ) where

import Sound.Tidal.Context
import Harmonic.Interface.Tidal.Instruments (ch)

-- A MIDI control-change line: a 0-1 value on the wire's 0-127 range. Tidal's
-- own @cc@ is a SuperDirt string parameter, a different thing entirely, so
-- this stays local rather than becoming an ambiguous export.
ctl :: Pattern Double -> Pattern Double -> ControlPattern
ctl num v = control (v * 127) # midicmd "control" # ctlNum num

-- | The S-1 listens on MIDI channel 6.
s1chan :: Pattern ValueMap
s1chan = ch 6

-- | Pattern and bank selection. 's1seq' addresses a pattern within a bank
-- of eight, both 1-indexed.
s1pat :: Pattern Double -> ControlPattern
-- | A pattern within a bank of eight, both 1-indexed.
s1seq :: Pattern Double -> Pattern Double -> ControlPattern
s1pat p        = midicmd "program" #progNum p # s1chan
s1seq pat bk   = s1pat ((pat-1)+((bk-1)*8))

-- | Global and performance controls: modulation, portamento, pan,
-- expression, damper and tuning.
s1modwheel, s1portatime, s1pan, s1expression, s1portamode, s1damper,
  s1portasw, s1finetune, s1transpose :: Pattern Double -> ControlPattern
s1modwheel v   = ctl 1   v # s1chan
s1portatime v  = ctl 5   v # s1chan
s1pan v        = ctl 10  v # s1chan
s1expression v = ctl 11  v # s1chan
s1portamode v  = ctl 31  v # s1chan
s1damper v     = ctl 64  v # s1chan
s1portasw v    = ctl 65  v # s1chan
s1finetune v   = ctl 76  v # s1chan
s1transpose v  = ctl 77  v # s1chan

-- | Oscillator section: range, waveform mix, pulse width, sub and noise.
s1osclfo, s1oscrange, s1oscpwm, s1oscpwmsrc, s1oscbend, s1square,
  s1saw, s1sub, s1suboct, s1noise, s1noisemode :: Pattern Double -> ControlPattern
s1osclfo v     = ctl 13  v # s1chan
s1oscrange v   = ctl 14  v # s1chan
s1oscpwm v     = ctl 15  v # s1chan
s1oscpwmsrc v  = ctl 16  v # s1chan
s1oscbend v    = ctl 18  v # s1chan
s1square v     = ctl 19  v # s1chan
s1saw v        = ctl 20  v # s1chan
s1sub v        = ctl 21  v # s1chan
s1suboct v     = ctl 22  v # s1chan
s1noise v      = ctl 23  v # s1chan
s1noisemode v  = ctl 78  v # s1chan

-- | Filter and amplifier: cutoff, resonance, envelope and LFO depth,
-- key tracking, amp mode.
s1cutoff, s1res, s1filterenv, s1filterlfo, s1keytrack, s1filterbend,
  s1ampmode :: Pattern Double -> ControlPattern
s1cutoff v     = ctl 74  v # s1chan
s1res v        = ctl 71  v # s1chan
s1filterenv v  = ctl 24  v # s1chan
s1filterlfo v  = ctl 25  v # s1chan
s1keytrack v   = ctl 26  v # s1chan
s1filterbend v = ctl 27  v # s1chan
s1ampmode v    = ctl 28  v # s1chan

-- | LFO and envelope: rate, waveform, trigger mode, and the four ADSR
-- stages.
s1lforate, s1lfowave, s1lfomod, s1lfomode, s1lfokeytrg, s1lfosync,
  s1attack, s1decay, s1sustain, s1release, s1envtrig :: Pattern Double -> ControlPattern
s1lforate v    = ctl 3   v # s1chan
s1lfowave v    = ctl 12  v # s1chan
s1lfomod v     = ctl 17  v # s1chan
s1lfomode v    = ctl 79  v # s1chan
s1lfokeytrg v  = ctl 105 v # s1chan
s1lfosync v    = ctl 106 v # s1chan
s1attack v     = ctl 73  v # s1chan
s1decay v      = ctl 75  v # s1chan
s1sustain v    = ctl 30  v # s1chan
s1release v    = ctl 72  v # s1chan
s1envtrig v    = ctl 29  v # s1chan

-- | Chord mode: the polyphony switch and the three additional voices with
-- their semitone shifts.
s1polymode, s1voice2sw, s1voice3sw, s1voice4sw, s1voice2shift,
  s1voice3shift, s1voice4shift :: Pattern Double -> ControlPattern
s1polymode v    = ctl 80 v # s1chan
s1voice2sw v    = ctl 81 v # s1chan
s1voice3sw v    = ctl 82 v # s1chan
s1voice4sw v    = ctl 83 v # s1chan
s1voice2shift v = ctl 85 v # s1chan
s1voice3shift v = ctl 86 v # s1chan
s1voice4shift v = ctl 87 v # s1chan

-- | The onboard reverb, delay and chorus.
s1revlvl, s1revtime, s1dellvl, s1deltime, s1chorus :: Pattern Double -> ControlPattern
s1revlvl v     = ctl 91  v # s1chan
s1revtime v    = ctl 89  v # s1chan
s1dellvl v     = ctl 92  v # s1chan
s1deltime v    = ctl 90  v # s1chan
s1chorus v     = ctl 93  v # s1chan

-- | The advanced oscillator page: draw, multiply, overtone and chop\/comb.
s1drawsw, s1drawmul, s1overtone, s1chopcomb :: Pattern Double -> ControlPattern
s1drawsw v     = ctl 107 v # s1chan
s1drawmul v    = ctl 102 v # s1chan
s1overtone v   = ctl 103 v # s1chan
s1chopcomb v   = ctl 104 v # s1chan

