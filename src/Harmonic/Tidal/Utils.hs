module Harmonic.Tidal.Utils where

import Sound.Tidal.Context

-- Octave transpose
oct :: Int -> Pattern ValueMap
oct n = note (fromIntegral (12 * n))

-- Pull/push aliases (Time rotation operators match TidalCycles early/late)
pullBy :: Time -> Pattern a -> Pattern a
pullBy t pat = (pure t) <~ pat

pushBy :: Time -> Pattern a -> Pattern a
pushBy t pat = (pure t) ~> pat

-- Humanize velocity
humanise :: Double -> Pattern ValueMap
humanise n = pF "amp" (range (pure (-0.09 * n)) (pure (0.09 * n)) rand)

-- Time divisions
hemidemisemiquaver, demisemiquaver, semiquaver, quaver, crotchet, minim :: Time
hemidemisemiquaver = 1/64
demisemiquaver = 1/32
semiquaver = 1/16
quaver = 1/8
crotchet = 1/4
minim = 1/2
