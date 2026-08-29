-- |
-- Module      : Harmonic.Interface.Tidal.Devices.JV1010
-- Description : Roland JV-1010 drum map and continuo bank addressing
--
-- Two things the JV-1010 knows and nothing else does: which MIDI note each
-- drum voice sits on, and how a patch is named.
--
-- The kit answers on channel 10 and each voice takes a boolean rhythm:
--
-- @, kick "1 ~ ~ 1"@
--
-- The primed variants are the +60 alternate kit. \'hh\' and \'hh\'\' take a
-- two-symbol grid instead (@x@ closed, @o@ open), so one pattern carries both
-- hats.
--
-- A continuo voice is a full bank address — @(bank msb, bank lsb, program)@ —
-- so any card or internal set is reachable, not just the Orchestral board.
-- The JV-1010 selects a patch with Bank Select (CC0 msb \/ CC32 lsb) followed
-- by a Program Change; quote Orchestral-card numbers straight off Roland\'s
-- 001-255 listing via \'orch\', and reach the internal sets with \'presetA\'
-- and \'presetB\'.
module Harmonic.Interface.Tidal.Devices.JV1010
  ( -- * Drum kit (channel 10)
    kick, kick', kick'', kick2, kick2', snap, hhcl, hhcl', hhop, hhop',
    ride, ride', crash, click, click', snare, snare', cowbell, fm, fm',
    rimshot, rimshot', hh, hh'

    -- * Continuo bank addressing
  , ContinuoVoice
  , orch, presetA, presetB, orchMSB, orchLSB
  , harpV, harpPluckedV, harpsichordV, pianoV, celestaV, glockenV
  , tubularV, choirV, guitarV, organV
  ) where

import Sound.Tidal.Context
import Harmonic.Interface.Tidal.Instruments (ch, vel)

-- | The kit answers on MIDI channel 10.
kitChan :: Pattern ValueMap
kitChan = ch 10

-- | One drum voice each, taking a boolean rhythm. The primed names are the
-- +60 alternate kit; @kick2@ and @kick2'@ are the second bass drum.
kick, kick', kick'', kick2, kick2', snap, hhcl, hhcl', hhop, hhop',
  ride, ride', crash, click, click', snare, snare', cowbell, fm, fm',
  rimshot, rimshot' :: Pattern Bool -> ControlPattern
kick pat = struct pat $ midinote "0" # kitChan # sustain 0.05
kick' pat = struct pat $ midinote "60" # kitChan # sustain 0.05
kick'' pat = struct pat $ midinote "70" # kitChan # sustain 0.05
kick2 pat = struct pat $ midinote "70" # kitChan # sustain 0.05
kick2' pat = struct pat $ midinote "80" # kitChan # sustain 0.05
snap pat = struct pat $ midinote "1" # kitChan # sustain 0.05
hhcl pat = struct pat $ midinote "2" # kitChan # sustain 0.05
hhcl' pat = struct pat $ midinote "62" # kitChan # sustain 0.05
hhop pat = struct pat $ midinote "3" # kitChan # sustain 0.05
hhop' pat = struct pat $ midinote "63" # kitChan # sustain 0.05
ride pat = struct pat $ midinote "4" # kitChan # sustain 0.05
ride' pat = struct pat $ midinote "64" # kitChan # sustain 0.05
crash pat = struct pat $ midinote "5" # kitChan # sustain 0.05
click pat = struct pat $ midinote "6" # kitChan # sustain 0.05
click' pat = struct pat $ midinote "66" # kitChan # sustain 0.05
snare pat = struct pat $ midinote "7" # kitChan # sustain 0.05
snare' pat = struct pat $ midinote "67" # kitChan # sustain 0.05
cowbell pat = struct pat $ midinote "8" # kitChan # sustain 0.05
fm pat = struct pat $ midinote "9" # kitChan # sustain 0.05
fm' pat = struct pat $ midinote "69" # kitChan # sustain 0.05
rimshot pat = struct pat $ midinote "[6,7]" # kitChan # sustain 0.05
rimshot' pat = struct pat $ midinote "[66,67]" # kitChan # sustain 0.05

-- | Both hats from one grid: @x@ (or @1@) closed, @o@ (or @2@) open.
hh, hh' :: Pattern String -> ControlPattern
hh pat = stack [
  struct (fmap (`elem` ["x","1"]) pat) $ midinote 2 # kitChan # sustain 0.05 # vel 0.5,
  struct (fmap (`elem` ["o","2"]) pat) $ midinote 3 # kitChan # sustain 0.05 # vel 0.5
  ]

hh' pat = stack [
  struct (fmap (`elem` ["x","1"]) pat) $ midinote 62 # kitChan # sustain 0.05 # vel 0.5,
  struct (fmap (`elem` ["o","2"]) pat) $ midinote 63 # kitChan # sustain 0.05 # vel 0.5
  ]

-- Bank addressing. JV-1010 preset banks are msb 81 / lsb 0-1; expansion
-- cards continue the lsb series.
-- | A full bank address: @(bank msb, bank lsb, program 0-127)@.
type ContinuoVoice = (Int, Int, Int)

-- | JV-1010 preset banks are msb 81 \/ lsb 0-1; expansion cards continue the
-- lsb series.
--
-- TODO confirm orchLSB on the device; adjust if the voice lands wrong.
orchMSB, orchLSB :: Int
orchMSB = 81
orchLSB = 32

-- | Orchestral-card patch @n@, quoted straight off Roland's 001-255 listing.
-- The card spans two 128-patch sub-banks, so the arithmetic is done here.
orch :: Int -> ContinuoVoice
orch patch = (orchMSB, orchLSB + ((patch - 1) `div` 128), (patch - 1) `mod` 128)

-- | The internal preset banks, patch @n@ numbered 1-128 as printed.
presetA, presetB :: Int -> ContinuoVoice
presetA patch = (81, 0, patch - 1)
presetB patch = (81, 1, patch - 1)

-- | Named continuo voices, from the SR-JV80-02 Orchestral card unless
-- noted otherwise.
harpV, harpPluckedV, harpsichordV, pianoV, celestaV, glockenV, tubularV, choirV, guitarV, organV :: ContinuoVoice
harpV        = orch 186   -- Harp 1        (default: Orpheus's lyre)
harpPluckedV = orch 188   -- Plucked Harp
harpsichordV = orch 196   -- Harpsichord1  (Baroque continuo)
pianoV       = orch 192   -- ClasclPiano1
celestaV     = orch 200   -- Celesta 1
glockenV     = orch 211   -- Glocken 1
tubularV     = orch 216   -- TubulaBells1
choirV       = orch 227   -- Choir 1
guitarV      = orch 185   -- Classical Gt
organV       = presetA 18 -- church/pipe organ from the internal set
                          -- TODO confirm patch number on the device
