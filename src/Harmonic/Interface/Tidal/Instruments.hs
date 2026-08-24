-- |
-- Module      : Harmonic.Interface.Tidal.Instruments
-- Description : MIDI channel routing and instrument shorthand for TidalCycles
--
-- Provides channel-assignment helpers (@p10@..@p16@, @ch@) and instrument
-- launchers (@wind@, @strg@, @brss@, @perc@, etc.) that combine MIDI
-- routing with form-driven kinetics for live performance.

module Harmonic.Interface.Tidal.Instruments (
    -- * Channel shorthand
    p10, p11, p12, p13, p14, p15, p16,
    ch,

    -- * Named synth wrappers
    moog, s101, juno,

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

-- | Moog Mother-32, channel 14.
moog :: Pattern ValueMap -> Pattern ValueMap
moog = (\pat -> pat # s "thru" # midichan 13)

-- | Roland SH-101, channel 15.
s101 :: Pattern ValueMap -> Pattern ValueMap
s101 = (\pat -> pat # s "thru" # midichan 14)

-- | Roland Juno, channel 16.
juno :: Pattern ValueMap -> Pattern ValueMap
juno = (\pat -> pat # s "thru" # midichan 15)

-- | Set the MIDI channel from a 1-indexed 'Int', so @ch 1@ is MIDI channel 1.
-- The general form of 'p10' .. 'p16'; used internally by every orchestral
-- instrument in "Harmonic.Interface.Tidal.Orchestra".
ch :: Int -> Pattern ValueMap
ch n = s "thru" # midichan (fromIntegral (n - 1))

-- | Velocity, as an alias for the @amp@ control. Multiply onto a pattern to
-- scale it:
--
-- @, violin1 T (0,1) k vl grid Soprano |* vel 0.7@
vel :: Pattern Double -> Pattern ValueMap
vel = pF "amp"
