-- |
-- Module      : Harmonic.Interface.Tidal.Instruments
-- Description : MIDI channel routing and instrument shorthand for TidalCycles
--
-- Provides the channel-assignment helpers every MIDI line in the library
-- routes through, and the studio-rig channel map. Device and section
-- launchers (@wind@, @strg@, @brss@, @perc@; @grnd@, @hmnx@, @s101@, @gnlr@)
-- are NOT defined here — they are composed per-performance in the .tidal
-- files (see documents\/ALGORITHMIC_ORCHESTRATION.md) and in the Pulsar
-- snippets, each block owning its channel with the @ch N@ postfix. Library
-- code never carries a launcher's name: a block and a wrapper of the same
-- name would shadow each other in the session.

module Harmonic.Interface.Tidal.Instruments (
    -- * Channel shorthand
    p10, p11, p12, p13, p14, p15, p16,
    ch,

    -- * Studio rig
    -- $studioRig
    pad,

    -- * Velocity
    vel,
) where

import Sound.Tidal.Context

-- | Route a pattern to a MIDI channel, by position. @p10@ is MIDI channel 10
-- (@midichan 9@, zero-indexed) through @p16@ on channel 16.
--
-- Postfix onto any pattern:
--
-- @d1 $ p14 $ n "0 3 5 7"@
--
-- Use 'ch' when the channel number is computed rather than literal.
p10, p11, p12, p13, p14, p15, p16 :: Pattern ValueMap -> Pattern ValueMap
p10 = (\pat -> pat # s "thru" # midichan 9)
p11 = (\pat -> pat # s "thru" # midichan 10)
p12 = (\pat -> pat # s "thru" # midichan 11)
p13 = (\pat -> pat # s "thru" # midichan 12)
p14 = (\pat -> pat # s "thru" # midichan 13)
p15 = (\pat -> pat # s "thru" # midichan 14)
p16 = (\pat -> pat # s "thru" # midichan 15)

-- $studioRig
-- The JV-1010 orchestra owns a separate, library-fixed map
-- ("Harmonic.Interface.Tidal.Orchestra"); the studio rig is the one below,
-- block names as the snippets launch them. Every channel has exactly one
-- owner; the blocks set it with @ch N@ (documents\/LIVE_ENVIRONMENT.md).
--
-- @
-- 1 grnd  grand piano (soft)        9 k909 \/ kgrv  drum machines
-- 2 mini  Minimoog pad (soft)      10 subk \/ kmpc  MPC sub, kit, 12-step LEDs
-- 3 DFAM osc1                      11 hmnx  MPC harmonics keygroup
-- 4 DFAM osc2                      12 slce  MPC misc-sample kit
-- 5 DFAM step trigger              13 s101  Roland S-1 (s1 controls)
-- 6 m32   Moog Mother-32           14 gnlr  P-6 G.CH  granular engine
-- 7 jura  Juno poly bells (soft)   15 smpl  P-6 S.CH  pads, 48 one-shots
-- 8 walk  bass line                16 auto  P-6 Auto CH focused pad + program change
-- @

-- | MPC misc-sample pad @n@ (1..16 = MIDI 36..51, bank A) on channel 12, the
-- @slce@ block's channel, baked in like the JV1010 drum voices. One note per
-- cycle: inside a 'Harmonic.Interface.Tidal.Form.mark' cue it fires once; for
-- rhythm, @struct \"1 ~ ~ ~\" (pad 4)@.
pad :: Int -> ControlPattern
pad padN = midinote (fromIntegral (35 + padN)) # sustain 0.1 # ch 12

-- | Set the MIDI channel from a 1-indexed 'Int', so @ch 1@ is MIDI channel 1.
-- The general form of 'p10' .. 'p16'; used internally by every orchestral
-- instrument in "Harmonic.Interface.Tidal.Orchestra".
ch :: Int -> Pattern ValueMap
ch c = s "thru" # midichan (fromIntegral (c - 1))

-- | Velocity, as an alias for the @amp@ control. Multiply onto a pattern to
-- scale it:
--
-- @, violin1 T (0,1) k vl grid Soprano |* vel 0.7@
vel :: Pattern Double -> Pattern ValueMap
vel = pF "amp"
