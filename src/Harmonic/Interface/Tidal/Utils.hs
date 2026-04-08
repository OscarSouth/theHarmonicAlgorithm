-- |
-- Module      : Harmonic.Interface.Tidal.Utils
-- Description : TidalCycles helper functions for octave transposition and time rotation
--
-- Shorthand utilities used across live-coding scripts: octave transposition
-- via 'oct', and time rotation operators 'pullBy'\/'pushBy' that wrap
-- TidalCycles' early\/late operators.

module Harmonic.Interface.Tidal.Utils where

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

-- | Ensure every event is an onset by aligning whole start with part start,
-- but only at cycle boundaries. Prevents TidalCycles' onset detection from
-- filtering events in cat constructions where inner patterns have period > 1
-- cycle, without causing MIDI flood from sub-cycle queries.
onset :: Pattern a -> Pattern a
onset pat = pat {query = q, pureValue = Nothing}
  where
    q st = map align (query pat st)
    align ev = case whole ev of
      Nothing -> ev
      Just (Arc _ we) ->
        let ps = start (part ev)
        in if ps == sam ps
           then ev {whole = Just (Arc ps (min we (nextSam ps)))}
           else ev


-- Time divisions
hemidemisemiquaver, demisemiquaver, semiquaver, quaver, crotchet, minim :: Time
hemidemisemiquaver = 1/64
demisemiquaver = 1/32
semiquaver = 1/16
quaver = 1/8
crotchet = 1/4
minim = 1/2
