-- |
-- Module      : Harmonic.Interface.Tidal.Devices.S1
-- Description : Roland AIRA S-1 Tweak Synthesizer control map
--
-- Every parameter the S-1 exposes over MIDI, as a control function taking a
-- 0-1 value. CC numbers are quoted from the official Roland chart.
--
-- The synth answers on MIDI channel 6, baked into each function -- set the
-- S-1 to channel 6 on the device. It is performed through the s101 block.
-- Patterns compose as usual:
--
-- @, s1cutoff (lfo saw 0.2 0.9)@
module Harmonic.Interface.Tidal.Devices.S1
  (
    -- * Controls
    s1prog, s1ptn, s1modwheel, s1portatime, s1pan, s1expression,
    s1portamode, s1damper, s1portasw, s1finetune, s1transpose, s1osclfo,
    s1oscrange, s1oscpwm, s1oscpwmsrc, s1oscbend, s1square, s1saw, s1sub,
    s1suboct, s1noise, s1noisemode, s1cutoff, s1res, s1filterenv,
    s1filterlfo, s1keytrack, s1filterbend, s1ampmode, s1lforate,
    s1lfowave, s1lfomod, s1lfomode, s1lfokeytrg, s1lfosync, s1attack,
    s1decay, s1sustain, s1release, s1envtrig, s1polymode, s1voice2sw,
    s1voice3sw, s1voice4sw, s1voice2shift, s1voice3shift, s1voice4shift,
    s1revlvl, s1revtime, s1dellvl, s1deltime, s1chorus, s1drawstep,
    s1drawmul, s1overtone, s1chopcomb,

    -- * Composite controls
    s1chord, s1env, s1mix,

    -- * Sidechain
    s1pump,

    -- * Device-native variants
    -- | Primed variants taking the exact number the S-1 itself displays, for
    -- setting a patch to a value read off the unit. The unprimed forms stay
    -- 0-1 and are what modulation wants.
    --
    -- Controls already in device units -- 's1transpose' and the voice key
    -- shifts in semitones, 's1pan', and every selector -- have no prime,
    -- because they are already the number on the display.
    --
    -- Roland publishes 0-255 for 's1oscpwm'', 's1keytrack'', 's1portatime'',
    -- 's1lforate'', 's1lfomod'', 's1filterbend'', 's1revtime'', 's1revlvl''
    -- and 's1dellvl''; 's1oscbend'' is 0-240 (120 = \u00b11 octave) and
    -- 's1deltime'' is 1-740 ms. The remaining primes below carry 0-255 by
    -- inference from those documented siblings -- the S-1's front-panel knobs
    -- have no published numeric range of their own.
    s1cutoff', s1res', s1filterenv', s1filterlfo', s1keytrack', s1filterbend',
    s1attack', s1decay', s1sustain', s1release',
    s1square', s1saw', s1sub', s1noise', s1osclfo', s1oscpwm', s1oscbend',
    s1lforate', s1lfomod', s1modwheel', s1expression', s1portatime',
    s1finetune', s1revtime', s1revlvl', s1deltime', s1dellvl',
    s1drawstep', s1drawmul', s1overtone', s1chopcomb',

    -- * Raw access
    s1raw
  ) where

import Sound.Tidal.Context
import Harmonic.Interface.Tidal.Instruments (ch)
import Harmonic.Interface.Tidal.Utils (pump)

-- A MIDI control-change line: a 0-1 value on the wire's 0-127 range. Tidal's
-- own @cc@ is a SuperDirt string parameter, a different thing entirely, so
-- this stays local rather than becoming an ambiguous export.
ctl :: Pattern Double -> Pattern Double -> ControlPattern
ctl num v = control (v * 127) # midicmd "control" # ctlNum num

-- | The S-1 listens on MIDI channel 6.
s1chan :: Pattern ValueMap
s1chan = ch 6                  -- TODO(channels): 13 on the new map; header text too

-- | Select a pattern by raw program number.
s1prog :: Pattern Double -> ControlPattern
-- | A pattern within a bank of eight, both 1-indexed.
s1ptn :: Pattern Double -> Pattern Double -> ControlPattern
s1prog p       = midicmd "program" #progNum p # s1chan
s1ptn ptn bk   = s1prog ((ptn-1)+((bk-1)*8))

-- | Modulation, portamento time and expression, all 0 to 1. 's1expression' is
-- what 's1pump' drives, so it stays a plain gain fraction.
s1modwheel, s1portatime, s1expression :: Pattern Double -> ControlPattern
-- | Pan position: @-64@ hard left, @0@ centre, @+63@ hard right.
s1pan :: Pattern Double -> ControlPattern
-- | Portamento mode, 1-indexed: @1@ OFF, @2@ On, @3@ Auto.
s1portamode :: Pattern Double -> ControlPattern
-- | Damper and portamento switches, 1-indexed: @1@ Off, @2@ On.
s1damper, s1portasw :: Pattern Double -> ControlPattern
-- | Oscillator fine tune, 0 to 1.
--
-- TODO confirm the span on the device. The manual describes the fine-tune
-- control as covering \u00b11 octave but publishes no value range for the CC.
s1finetune :: Pattern Double -> ControlPattern
-- | Keyboard transpose in semitones, -60 to +60 (\u00b15 octaves).
s1transpose :: Pattern Double -> ControlPattern
s1modwheel v   = ctl 1   v # s1chan
s1portatime v  = ctl 5   v # s1chan
s1pan v        = ctl 10  ((v + 64) / 127) # s1chan
s1expression v = ctl 11  v # s1chan
s1portamode v  = ctl 31  ((v - 0.5) / 3) # s1chan
s1damper v     = ctl 64  ((v - 0.5) / 2) # s1chan
s1portasw v    = ctl 65  ((v - 0.5) / 2) # s1chan
s1finetune v   = ctl 76  v # s1chan
s1transpose v  = ctl 77  ((v + 60) / 120) # s1chan

-- | LFO-to-pitch depth, pulse width, and the four oscillator levels (square,
-- saw, sub, noise), all 0 to 1.
s1osclfo, s1oscpwm, s1square, s1saw, s1sub, s1noise :: Pattern Double -> ControlPattern
-- | Oscillator range, 1-indexed: @1@ 64', @2@ 32', @3@ 16', @4@ 8', @5@ 4',
-- @6@ 2'. At 8' the lowest pad sounds middle C.
s1oscrange :: Pattern Double -> ControlPattern
-- | Pulse-width source, 1-indexed: @1@ Env, @2@ Manual, @3@ LFO.
s1oscpwmsrc :: Pattern Double -> ControlPattern
-- | Pitch-bend sensitivity, 0 to 1.
--
-- TODO confirm the span on the device; Roland publishes no range for it.
s1oscbend :: Pattern Double -> ControlPattern
-- | Sub-oscillator waveform, 1-indexed: @1@ -2 oct asymmetric, @2@ -2 oct,
-- @3@ -1 oct.
s1suboct :: Pattern Double -> ControlPattern
-- | Noise waveform, 1-indexed: @1@ Pink, @2@ White.
s1noisemode :: Pattern Double -> ControlPattern
s1osclfo v     = ctl 13  v # s1chan
s1oscrange v   = ctl 14  ((v - 0.5) / 6) # s1chan
s1oscpwm v     = ctl 15  v # s1chan
s1oscpwmsrc v  = ctl 16  ((v - 0.5) / 3) # s1chan
s1oscbend v    = ctl 18  v # s1chan
s1square v     = ctl 19  v # s1chan
s1saw v        = ctl 20  v # s1chan
s1sub v        = ctl 21  v # s1chan
s1suboct v     = ctl 22  ((v - 0.5) / 3) # s1chan
s1noise v      = ctl 23  v # s1chan
s1noisemode v  = ctl 78  ((v - 0.5) / 2) # s1chan

-- | Cutoff, resonance, envelope and LFO depth and key tracking, all 0 to 1.
s1cutoff, s1res, s1filterenv, s1filterlfo, s1keytrack :: Pattern Double -> ControlPattern
-- | Filter bend sensitivity, 0 to 1.
--
-- TODO confirm the span on the device; Roland publishes no range for it.
s1filterbend :: Pattern Double -> ControlPattern
-- | Amp control source, 1-indexed: @1@ Gate, @2@ Env.
s1ampmode :: Pattern Double -> ControlPattern
s1cutoff v     = ctl 74  v # s1chan
s1res v        = ctl 71  v # s1chan
s1filterenv v  = ctl 24  v # s1chan
s1filterlfo v  = ctl 25  v # s1chan
s1keytrack v   = ctl 26  v # s1chan
s1filterbend v = ctl 27  v # s1chan
s1ampmode v    = ctl 28  ((v - 0.5) / 2) # s1chan

-- | LFO rate and depth, and the four envelope stages, all 0 to 1.
s1lforate, s1lfomod, s1attack, s1decay, s1sustain, s1release :: Pattern Double -> ControlPattern
-- | LFO waveform, 1-indexed: @1@ saw, @2@ inverted saw, @3@ triangle,
-- @4@ square, @5@ random, @6@ noise.
s1lfowave :: Pattern Double -> ControlPattern
-- | LFO mode, 1-indexed: @1@ Normal, @2@ Fast.
s1lfomode :: Pattern Double -> ControlPattern
-- | LFO key-trigger and tempo-sync switches, 1-indexed: @1@ Off, @2@ On.
s1lfokeytrg, s1lfosync :: Pattern Double -> ControlPattern
-- | Envelope trigger mode, 1-indexed: @1@ LFO, @2@ Gate, @3@ Gate+Trig.
s1envtrig :: Pattern Double -> ControlPattern
s1lforate v    = ctl 3   v # s1chan
s1lfowave v    = ctl 12  ((v - 0.5) / 6) # s1chan
s1lfomod v     = ctl 17  v # s1chan
s1lfomode v    = ctl 79  ((v - 0.5) / 2) # s1chan
s1lfokeytrg v  = ctl 105 ((v - 0.5) / 2) # s1chan
s1lfosync v    = ctl 106 ((v - 0.5) / 2) # s1chan
s1attack v     = ctl 73  v # s1chan
s1decay v      = ctl 75  v # s1chan
s1sustain v    = ctl 30  v # s1chan
s1release v    = ctl 72  v # s1chan
s1envtrig v    = ctl 29  ((v - 0.5) / 3) # s1chan

-- | Sound-triggering mode, 1-indexed: @1@ Mono, @2@ Unison, @3@ Poly,
-- @4@ Chord.
s1polymode :: Pattern Double -> ControlPattern
-- | Chord voices 2-4 on\/off, 1-indexed: @1@ Off, @2@ On.
s1voice2sw, s1voice3sw, s1voice4sw :: Pattern Double -> ControlPattern
-- | Chord voice key shifts, in semitones, -12 to +12.
s1voice2shift, s1voice3shift, s1voice4shift :: Pattern Double -> ControlPattern
s1polymode v    = ctl 80 ((v - 0.5) / 4) # s1chan
s1voice2sw v    = ctl 81 ((v - 0.5) / 2) # s1chan
s1voice3sw v    = ctl 82 ((v - 0.5) / 2) # s1chan
s1voice4sw v    = ctl 83 ((v - 0.5) / 2) # s1chan
s1voice2shift v = ctl 85 ((v + 12) / 24) # s1chan
s1voice3shift v = ctl 86 ((v + 12) / 24) # s1chan
s1voice4shift v = ctl 87 ((v + 12) / 24) # s1chan

-- | The onboard reverb and delay, 0 to 1.
s1revlvl, s1revtime, s1dellvl, s1deltime :: Pattern Double -> ControlPattern
-- | Chorus TYPE, 1-indexed: @1@ OFF, @2@-@5@ chorus types 1-4. Not a level.
s1chorus :: Pattern Double -> ControlPattern
s1revlvl v     = ctl 91  v # s1chan
s1revtime v    = ctl 89  v # s1chan
s1dellvl v     = ctl 92  v # s1chan
s1deltime v    = ctl 90  v # s1chan
s1chorus v     = ctl 93  ((v - 0.5) / 5) # s1chan

-- | The advanced oscillator page: draw, multiply, overtone and chop\/comb.
s1drawstep, s1drawmul, s1overtone, s1chopcomb :: Pattern Double -> ControlPattern
s1drawstep v   = ctl 107 v # s1chan
s1drawmul v    = ctl 102 v # s1chan
s1overtone v   = ctl 103 v # s1chan
s1chopcomb v   = ctl 104 v # s1chan

-------------------------------------------------------------------------------
-- Composite controls
-------------------------------------------------------------------------------

-- | Chord mode in one call: switch voices 2-4 on and set their three key
-- shifts, so one held note sounds a voicing. The S-1's signature feature.
--
-- Shifts are in semitones, -12 to +12:
--
-- @, s1chord 4 7 11 # o@   -- a major seventh above each played note
s1chord :: Pattern Double -> Pattern Double -> Pattern Double -> ControlPattern
s1chord sh2 sh3 sh4 = stack
  [ s1voice2sw 2,      s1voice3sw 2,      s1voice4sw 2
  , s1voice2shift sh2, s1voice3shift sh3, s1voice4shift sh4 ]

-- | All four envelope stages in one call.
s1env :: Pattern Double -> Pattern Double -> Pattern Double -> Pattern Double -> ControlPattern
s1env atk dec sLvl rTime =
  stack [s1attack atk, s1decay dec, s1sustain sLvl, s1release rTime]

-- | The oscillator mixer in one call: square, saw, sub and noise levels.
s1mix :: Pattern Double -> Pattern Double -> Pattern Double -> Pattern Double -> ControlPattern
s1mix sqLvl sawLvl subLvl nzLvl =
  stack [s1square sqLvl, s1saw sawLvl, s1sub subLvl, s1noise nzLvl]

-------------------------------------------------------------------------------
-- Sidechain
-------------------------------------------------------------------------------

-- | Duck the S-1 against a ghost sidechain: 'pump' driving expression
-- (CC 11), which is the S-1\'s only continuous gain stage — it has no CC 7.
--
-- Sits in the stack as its own element, never applied to the notes:
--
-- @, s1pump \"1 0 0 0\" 0.55 0.75 # o@
s1pump :: Pattern Bool -> Pattern Double -> Double -> ControlPattern
s1pump st depth rls = s1expression (pump st depth rls)

-------------------------------------------------------------------------------
-- Raw access
-------------------------------------------------------------------------------

-- | Any control change by number, taking a raw 0-1 value — the escape hatch for
-- when a calibration above turns out wrong on the device.
--
-- @, s1raw 74 0.5 # o@
s1raw :: Pattern Double -> Pattern Double -> ControlPattern
s1raw num v = ctl num v # s1chan

-------------------------------------------------------------------------------
-- Device-native variants
-------------------------------------------------------------------------------

-- | Filter and envelope controls on the S-1's 0-255 scale. 's1keytrack'' and
-- 's1filterbend'' are published at 0-255; the rest are inferred from them.
s1cutoff', s1res', s1filterenv', s1filterlfo', s1keytrack', s1filterbend',
  s1attack', s1decay', s1sustain', s1release' :: Pattern Double -> ControlPattern
s1cutoff' v     = ctl 74  (v / 255) # s1chan
s1res' v        = ctl 71  (v / 255) # s1chan
s1filterenv' v  = ctl 24  (v / 255) # s1chan
s1filterlfo' v  = ctl 25  (v / 255) # s1chan
s1keytrack' v   = ctl 26  (v / 255) # s1chan
s1filterbend' v = ctl 27  (v / 255) # s1chan
s1attack' v     = ctl 73  (v / 255) # s1chan
s1decay' v      = ctl 75  (v / 255) # s1chan
s1sustain' v    = ctl 30  (v / 255) # s1chan
s1release' v    = ctl 72  (v / 255) # s1chan

-- | Oscillator controls, 0-255 — except 's1oscbend'', which Roland gives as
-- 0-240, where 120 is a bend range of \u00b11 octave and 240 is \u00b12.
s1square', s1saw', s1sub', s1noise', s1osclfo', s1oscpwm', s1oscbend',
  s1finetune' :: Pattern Double -> ControlPattern
s1square' v     = ctl 19  (v / 255) # s1chan
s1saw' v        = ctl 20  (v / 255) # s1chan
s1sub' v        = ctl 21  (v / 255) # s1chan
s1noise' v      = ctl 23  (v / 255) # s1chan
s1osclfo' v     = ctl 13  (v / 255) # s1chan
s1oscpwm' v     = ctl 15  (v / 255) # s1chan
s1oscbend' v    = ctl 18  (v / 240) # s1chan
s1finetune' v   = ctl 76  (v / 255) # s1chan

-- | LFO, performance and portamento controls, 0-255.
s1lforate', s1lfomod', s1modwheel', s1expression',
  s1portatime' :: Pattern Double -> ControlPattern
s1lforate' v    = ctl 3   (v / 255) # s1chan
s1lfomod' v     = ctl 17  (v / 255) # s1chan
s1modwheel' v   = ctl 1   (v / 255) # s1chan
s1expression' v = ctl 11  (v / 255) # s1chan
s1portatime' v  = ctl 5   (v / 255) # s1chan

-- | Reverb and delay: levels and reverb time are 0-255, delay time 1-740 ms.
s1revtime', s1revlvl', s1deltime', s1dellvl' :: Pattern Double -> ControlPattern
s1revtime' v    = ctl 89  (v / 255) # s1chan
s1revlvl' v     = ctl 91  (v / 255) # s1chan
s1deltime' v    = ctl 90  ((v - 1) / 739) # s1chan
s1dellvl' v     = ctl 92  (v / 255) # s1chan

-- | The DRAW and CHOP oscillator page, 0-255.
s1drawstep', s1drawmul', s1overtone', s1chopcomb' :: Pattern Double -> ControlPattern
s1drawstep' v   = ctl 107 (v / 255) # s1chan
s1drawmul' v    = ctl 102 (v / 255) # s1chan
s1overtone' v   = ctl 103 (v / 255) # s1chan
s1chopcomb' v   = ctl 104 (v / 255) # s1chan
