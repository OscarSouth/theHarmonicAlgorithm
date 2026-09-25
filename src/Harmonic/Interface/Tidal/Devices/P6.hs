-- |
-- Module      : Harmonic.Interface.Tidal.Devices.P6
-- Description : Roland AIRA P-6 Creative Sampler control map
--
-- The P-6 answers on three independent receive channels at once, so it is
-- three performable layers rather than one instrument:
--
-- @
-- Auto CH = 16  auto   the focused pad, played chromatically and polyphonically
-- S.CH    = 15  smpl   the global 48 pads, each at its own fixed pitch
-- G.CH    = 14  gnlr   the granular engine, notes and CC
-- Program = 16         preset select, fixed on the device; the auto layer shares it
-- @
--
-- All three are reassigned from the device defaults (15 \/ 11 \/ 4), so the
-- unit must be configured to match. They sound simultaneously, sharing only
-- the P-6's voice pool.
--
-- CC numbers are quoted from the official Roland P-6 MIDI chart. The chart
-- carries ONE control-change set, received on either the granular or the auto
-- channel, so every control below is shared between the granular and auto
-- layers. Each defaults to the granular channel; postfix a channel to retarget
-- it, since @#@ takes values from the right — this is the same control, moved
-- onto the auto layer:
--
-- @, p6cutoff (lfo saw 0.2 0.9) # ch 16@
module Harmonic.Interface.Tidal.Devices.P6
  (
    -- * Source
    p6prog, p6src

    -- * Playhead
  , p6headpos, p6headspeed, p6scrub

    -- * Grain cloud
  , p6grains, p6grainsize, p6spread, p6grainshape, p6cloud

    -- * Chaos
  , p6grainjitter, p6leveljitter, p6grainrev, p6chaos

    -- * Pitch
  , p6coarse, p6fine, p6detune, p6grainkf

    -- * Filter
  , p6filtertype, p6cutoff, p6res, p6filterenv, p6filterkf, p6filtervel

    -- * Envelope
  , p6attack, p6decay, p6sustain, p6release, p6env
  , p6envmode, p6envkf, p6ampsw, p6startmode

    -- * Output and effects
  , p6level, p6pan, p6autopan, p6bus, p6senddel, p6sendrev, p6lofi, p6lofisw
  , p6revtime, p6revlvl, p6deltime, p6dellvl

    -- * Pads
  , p6note, p6trig, p6pad

    -- * Device-native variants
    -- | Primed variants taking the exact number the P-6 itself displays,
    -- for setting a patch to a value read off the unit. The unprimed forms
    -- stay 0-1 and are what modulation wants.
    --
    -- Only controls whose unprimed form is a 0-1 abstraction have a primed
    -- variant. Anything already in device units -- 'p6coarse' in semitones,
    -- 'p6spread' in percent, 'p6pan', 'p6src', and every selector -- has no
    -- prime, because it is already the number on the display. 'p6headpos'
    -- and 'p6grainsize' have none either: Roland gives them in seconds
    -- \"0.000 to sample end\", so the absolute value depends on the sample.
  , p6cutoff', p6res', p6filterenv', p6filterkf', p6filtervel'
  , p6attack', p6decay', p6sustain', p6release', p6envkf', p6grainkf'
  , p6level', p6senddel', p6sendrev', p6lofi'
  , p6revtime', p6revlvl', p6deltime', p6dellvl'

    -- * Raw access
  , p6raw

    -- * Sidechain
  , p6pump
  ) where

import Sound.Tidal.Context
import Harmonic.Interface.Tidal.Instruments (ch)
import Harmonic.Interface.Tidal.Utils (pump)

-- A MIDI control-change line: a 0-1 value on the wire's 0-127 range. Tidal's
-- own @cc@ is a SuperDirt string parameter, a different thing entirely, so
-- this stays local rather than becoming an ambiguous export.
ctl :: Pattern Double -> Pattern Double -> ControlPattern
ctl num v = control (v * 127) # midicmd "control" # ctlNum num

-- Pad triggering answers on S.CH; every control change defaults to G.CH and is
-- retargeted to the auto layer by a postfix channel.
p6trigChan, p6granChan :: Pattern ValueMap
p6trigChan = ch 15
p6granChan = ch 14

-------------------------------------------------------------------------------
-- Source
-------------------------------------------------------------------------------

-- | Select a preset, 0-63, on the program channel.
p6prog :: Pattern Double -> ControlPattern
p6prog p = midicmd "program" #progNum p # ch 16

-- | The granular source by pad number, 1-48 — the same numbering as 'p6pad'.
-- Patterning this is what makes the granular layer sequenceable rather than
-- hand-selected on the unit.
--
-- TODO confirm the slot count on the device. The pad grid is 48 (MIDI notes
-- 48-95), but Roland documents the Sample parameter as \"A-1 to h-8\", which
-- reads as 64 slots; if so the divisor here becomes 64.
p6src :: Pattern Double -> ControlPattern
p6src pad       = ctl 88  ((pad - 0.5) / 48) # p6granChan

-------------------------------------------------------------------------------
-- Playhead
-------------------------------------------------------------------------------

-- | Where in the sample the grains are drawn from — a fraction of the sample,
-- 0 to 1. Roland gives this as \"0.000 to sample end\", so the absolute range
-- depends on the loaded sample and a fraction is the only stable unit.
p6headpos :: Pattern Double -> ControlPattern
-- | How fast the playhead travels, as a speed multiplier: -4.00 to +4.00.
-- @1@ is normal speed, @0@ freezes the playhead, negative values run backwards.
p6headspeed :: Pattern Double -> ControlPattern
p6headpos v     = ctl 19  v # p6granChan
p6headspeed v   = ctl 20  ((v + 4) / 8) # p6granChan

-- | Position (0 to 1) and speed (-4.00 to +4.00) as one gesture: scan, stall
-- and reverse through the sample. Each side stays independently patternable.
--
-- @, p6scrub (lfo saw 0.1 0.9) 1@
p6scrub :: Pattern Double -> Pattern Double -> ControlPattern
p6scrub pos spd = stack [p6headpos pos, p6headspeed spd]

-------------------------------------------------------------------------------
-- Grain cloud
-------------------------------------------------------------------------------

-- | Grain density, 0.5 to 8.0.
p6grains :: Pattern Double -> ControlPattern
-- | Grain duration as a fraction of the sample, 0 to 1 — Roland gives it as
-- \"0.000 to sample duration\", so it is sample-relative like 'p6headpos'.
p6grainsize :: Pattern Double -> ControlPattern
-- | How wide the grains scatter, 0-100%.
p6spread :: Pattern Double -> ControlPattern
-- | The volume envelope each grain wears, 0-100.
p6grainshape :: Pattern Double -> ControlPattern
p6grains v      = ctl 21  ((v - 0.5) / 7.5) # p6granChan
p6grainsize v   = ctl 23  v # p6granChan
p6spread v      = ctl 25  (v / 100) # p6granChan
p6grainshape v  = ctl 15  (v / 100) # p6granChan

-- | One axis from sparse and grainy to dense and washed. Grain count and
-- spread rise with @v@ while grain size falls, so the grains get shorter as
-- they get more numerous — the inversion is the whole point of the macro.
-- The three controls stay available separately.
p6cloud :: Pattern Double -> ControlPattern
p6cloud v       = stack [p6grains (0.5 + v * 7.5), p6grainsize (1 - v), p6spread (v * 100)]

-------------------------------------------------------------------------------
-- Chaos
-------------------------------------------------------------------------------

-- | The instability controls, all 0-100%: when each grain fires, how loud it
-- is, and how often one plays backwards.
p6grainjitter, p6leveljitter, p6grainrev :: Pattern Double -> ControlPattern
p6grainjitter v = ctl 68  (v / 100) # p6granChan
p6leveljitter v = ctl 14  (v / 100) # p6granChan
p6grainrev v    = ctl 3   (v / 100) # p6granChan

-- | Mechanical to unstable on one axis: timing jitter, level jitter and
-- reverse probability together.
p6chaos :: Pattern Double -> ControlPattern
p6chaos v       = stack [p6grainjitter (v * 100), p6leveljitter (v * 100), p6grainrev (v * 100)]

-------------------------------------------------------------------------------
-- Pitch
-------------------------------------------------------------------------------

-- | Coarse tuning in semitones, -24 to +24.
p6coarse :: Pattern Double -> ControlPattern
-- | Fine tuning in cents, -100 to +100.
p6fine :: Pattern Double -> ControlPattern
-- | Random pitch variation across grains, 0-100%.
p6detune :: Pattern Double -> ControlPattern
-- | Whether grain time follows the played pitch (formant-preserving) or rides
-- it, 0 to 1.
p6grainkf :: Pattern Double -> ControlPattern
p6coarse v      = ctl 76  ((v + 24) / 48) # p6granChan
p6fine v        = ctl 18  ((v + 100) / 200) # p6granChan
p6detune v      = ctl 13  (v / 100) # p6granChan
p6grainkf v     = ctl 16  v # p6granChan

-------------------------------------------------------------------------------
-- Filter
-------------------------------------------------------------------------------

-- | Filter type, 1-indexed: @1@ OFF, @2@ LPF, @3@ BPF, @4@ HPF, @5@ PKG.
p6filtertype :: Pattern Double -> ControlPattern
-- | Cutoff, resonance, envelope depth, key follow and velocity sensitivity,
-- all 0 to 1 — the device scale is 0-255 with no stated taper.
p6cutoff, p6res, p6filterenv, p6filterkf, p6filtervel :: Pattern Double -> ControlPattern
p6filtertype v  = ctl 12  ((v - 0.5) / 5) # p6granChan
p6cutoff v      = ctl 74  v # p6granChan
p6res v         = ctl 71  v # p6granChan
p6filterenv v   = ctl 24  v # p6granChan
p6filterkf v    = ctl 26  v # p6granChan
p6filtervel v   = ctl 78  v # p6granChan

-------------------------------------------------------------------------------
-- Envelope
-------------------------------------------------------------------------------

-- | The four stages of the time envelope.
p6attack, p6decay, p6sustain, p6release :: Pattern Double -> ControlPattern
p6attack v      = ctl 73  v # p6granChan
p6decay v       = ctl 75  v # p6granChan
p6sustain v     = ctl 30  v # p6granChan
p6release v     = ctl 72  v # p6granChan

-- | All four stages in one call.
p6env :: Pattern Double -> Pattern Double -> Pattern Double -> Pattern Double -> ControlPattern
p6env atk dec sLvl rTime = stack [p6attack atk, p6decay dec, p6sustain sLvl, p6release rTime]

-- | Envelope mode, 1-indexed: @1@ ADSR, @2@ ADR, @3@ ADA.C.
p6envmode :: Pattern Double -> ControlPattern
-- | Envelope time key-follow, 0 to 1.
p6envkf :: Pattern Double -> ControlPattern
-- | Amp switch, 1-indexed: @1@ Off, @2@ On.
p6ampsw :: Pattern Double -> ControlPattern
-- | Start mode, 1-indexed: @1@ Cold, @2@ Hot.
p6startmode :: Pattern Double -> ControlPattern
p6envmode v     = ctl 29  ((v - 0.5) / 3) # p6granChan
p6envkf v       = ctl 77  v # p6granChan
p6ampsw v       = ctl 28  ((v - 0.5) / 2) # p6granChan
p6startmode v   = ctl 79  ((v - 0.5) / 2) # p6granChan

-------------------------------------------------------------------------------
-- Output and effects
-------------------------------------------------------------------------------

-- | Level, the two effect sends and the lo-fi stage, all 0 to 1. 'p6level' is
-- what 'p6pump' drives, so it stays a plain gain fraction.
p6level, p6senddel, p6sendrev, p6lofi :: Pattern Double -> ControlPattern
-- | Pan position: @-64@ hard left, @0@ centre, @+63@ hard right. Only active
-- while 'p6autopan' is OFF.
p6pan :: Pattern Double -> ControlPattern
-- | Auto-pan, 1-indexed: @1@ OFF, @2@ ALT, @3@ SWING, @4@ RND.
p6autopan :: Pattern Double -> ControlPattern
-- | Output routing, 1-indexed: @1@ Bus A, @2@ Bus B, @3@ EFCT.
p6bus :: Pattern Double -> ControlPattern
-- | Lo-fi switch, 1-indexed: @1@ Off, @2@ On.
p6lofisw :: Pattern Double -> ControlPattern
p6level v       = ctl 7   v # p6granChan
p6pan v         = ctl 10  ((v + 64) / 127) # p6granChan
p6autopan v     = ctl 9   ((v - 0.5) / 4) # p6granChan
p6bus v         = ctl 84  ((v - 0.5) / 3) # p6granChan
p6senddel v     = ctl 85  v # p6granChan
p6sendrev v     = ctl 86  v # p6granChan
p6lofi v        = ctl 17  v # p6granChan
p6lofisw v      = ctl 87  ((v - 0.5) / 2) # p6granChan

-- | The onboard reverb and delay.
p6revtime, p6revlvl, p6deltime, p6dellvl :: Pattern Double -> ControlPattern
p6revtime v     = ctl 89  v # p6granChan
p6revlvl v      = ctl 91  v # p6granChan
p6deltime v     = ctl 90  v # p6granChan
p6dellvl v      = ctl 92  v # p6granChan

-------------------------------------------------------------------------------
-- Pads
-------------------------------------------------------------------------------

-- | The MIDI note for bank @b@, pad @n@: notes 48-95 map bank A pad 1
-- through bank H pad 6, eight banks of six. Both indices are 1-based.
p6note :: Int -> Int -> Int
-- | Trigger bank @b@, pad @n@ on a boolean rhythm.
p6trig :: Int -> Int -> Pattern Bool -> ControlPattern
-- | Trigger pad @n@ counting straight through all 48, ignoring banks, so
-- @p6pad 1@ and @p6trig 1 1@ are the same pad.
p6pad :: Int -> Pattern Bool -> ControlPattern
p6note b pad     = 48 + (b-1)*6 + (pad-1)
p6trig b pad pat = struct pat $ midinote (fromIntegral (p6note b pad))  # p6trigChan # sustain 0.1
p6pad  pad pat   = struct pat $ midinote (fromIntegral (48 + (pad-1))) # p6trigChan # sustain 0.1

-------------------------------------------------------------------------------
-- Sidechain
-------------------------------------------------------------------------------

-- | Duck the P-6 against a ghost sidechain: 'pump' driving Level (CC 7).
--
-- Roland\'s chart names CC 7 @GRANULAR [Level]@, so on the granular channel
-- this is the granular engine\'s own gain and ducks reliably. Postfixed onto
-- the auto channel it most likely moves that same parameter rather than an
-- independent auto level, and the sampler channel is not listed as receiving
-- control change at all — confirm on the device before relying on either.
--
-- Sits in the stack as its own element, never applied to the notes:
--
-- @, p6pump \"1 0 0 0\" 0.8 1.5 # o@
p6pump :: Pattern Bool -> Pattern Double -> Double -> ControlPattern
p6pump st depth rls = p6level (pump st depth rls)

-------------------------------------------------------------------------------
-- Raw access
-------------------------------------------------------------------------------

-- | Any control change by number, taking a raw 0-1 value — the escape hatch for
-- when a calibration above turns out wrong on the device.
--
-- @, p6raw 74 0.5 # o@
p6raw :: Pattern Double -> Pattern Double -> ControlPattern
p6raw num v = ctl num v # p6granChan

-------------------------------------------------------------------------------
-- Device-native variants
-------------------------------------------------------------------------------

-- | Filter and envelope controls on the P-6's own 0-255 scale.
p6cutoff', p6res', p6filterenv', p6filterkf', p6filtervel',
  p6attack', p6decay', p6sustain', p6release', p6envkf',
  p6grainkf' :: Pattern Double -> ControlPattern
p6cutoff' v     = ctl 74  (v / 255) # p6granChan
p6res' v        = ctl 71  (v / 255) # p6granChan
p6filterenv' v  = ctl 24  (v / 255) # p6granChan
p6filterkf' v   = ctl 26  (v / 255) # p6granChan
p6filtervel' v  = ctl 78  (v / 255) # p6granChan
p6attack' v     = ctl 73  (v / 255) # p6granChan
p6decay' v      = ctl 75  (v / 255) # p6granChan
p6sustain' v    = ctl 30  (v / 255) # p6granChan
p6release' v    = ctl 72  (v / 255) # p6granChan
p6envkf' v      = ctl 77  (v / 255) # p6granChan
p6grainkf' v    = ctl 16  (v / 255) # p6granChan

-- | Level on the P-6's 0-127 scale, where 100 is 0 dB and 127 is +12 dB.
p6level' :: Pattern Double -> ControlPattern
p6level' v      = ctl 7   (v / 127) # p6granChan

-- | Sends and the lo-fi stage, 0-255.
--
-- The two sends are documented at 0-255; the lo-fi intensity has no published
-- range and is assumed to share it.
p6senddel', p6sendrev', p6lofi' :: Pattern Double -> ControlPattern
p6senddel' v    = ctl 85  (v / 255) # p6granChan
p6sendrev' v    = ctl 86  (v / 255) # p6granChan
p6lofi' v       = ctl 17  (v / 255) # p6granChan

-- | Reverb and delay on their own scales: levels and reverb time are 0-255,
-- delay time is 1-740 ms.
p6revtime', p6revlvl', p6deltime', p6dellvl' :: Pattern Double -> ControlPattern
p6revtime' v    = ctl 89  (v / 255) # p6granChan
p6revlvl' v     = ctl 91  (v / 255) # p6granChan
p6deltime' v    = ctl 90  ((v - 1) / 739) # p6granChan
p6dellvl' v     = ctl 92  (v / 255) # p6granChan
