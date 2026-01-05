{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Harmonic.Tidal.Instruments where

import Sound.Tidal.Context

-- Shorthand channel helpers
p10, p11, p12, p13, p14, p15, p16 :: Pattern ValueMap -> Pattern ValueMap
p10 = (\pat -> pat # s "thru" # midichan 9)
p11 = (\pat -> pat # s "thru" # midichan 10)
p12 = (\pat -> pat # s "thru" # midichan 11)
p13 = (\pat -> pat # s "thru" # midichan 12)
p14 = (\pat -> pat # s "thru" # midichan 13)
p15 = (\pat -> pat # s "thru" # midichan 14)
p16 = (\pat -> pat # s "thru" # midichan 15)

-- Instrument wrappers for common synths
moog :: Pattern ValueMap -> Pattern ValueMap
moog = (\pat -> pat # s "thru" # midichan 13)

s101 :: Pattern ValueMap -> Pattern ValueMap
s101 = (\pat -> pat # s "thru" # midichan 14)

juno :: Pattern ValueMap -> Pattern ValueMap
juno = (\pat -> pat # s "thru" # midichan 15)

flute :: Pattern ValueMap -> Pattern ValueMap
flute = (\pat -> pat # s "thru" # midichan 0)

-- Utility for setting channel from int
ch :: Int -> Pattern ValueMap
ch n = s "thru" # midichan (fromIntegral (n - 1))

-- Velocity alias
vel :: Pattern Double -> Pattern ValueMap
vel = pF "amp"
