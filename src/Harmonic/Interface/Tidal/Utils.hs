-- |
-- Module      : Harmonic.Interface.Tidal.Utils
-- Description : TidalCycles helper functions for octave transposition and time rotation
--
-- Shorthand utilities used across live-coding scripts: octave transposition
-- via 'oct', and time rotation operators 'pullBy'\/'pushBy' that wrap
-- TidalCycles' early\/late operators.

module Harmonic.Interface.Tidal.Utils (
    -- * Transposition
    oct,

    -- * Time rotation
    pullBy, pushBy,

    -- * Humanisation
    humanise,

    -- * Onset repair
    onset,

    -- * Note-length constants
    hemidemisemiquaver, demisemiquaver, semiquaver, quaver, crotchet, minim,
) where

import Sound.Tidal.Context

-- | Transpose by whole octaves. @oct 1@ is up an octave, @oct (-1)@ down.
--
-- Add onto a pattern:
--
-- @, cello T (0,1) k vl grid Bass |+ oct (-1)@
oct :: Int -> Pattern ValueMap
oct n = note (fromIntegral (12 * n))

-- | Rotate a pattern earlier ('pullBy') or later ('pushBy') in time.
-- Function forms of the TidalCycles @\<~@ and @~>@ operators, so they compose
-- in a modifier chain rather than needing parentheses.
--
-- @, pushBy (1\/8) $ harp T (0,1) k vl flow Alto@
pullBy :: Time -> Pattern a -> Pattern a
pullBy t pat = (pure t) <~ pat

-- | Rotate a pattern later in time. See 'pullBy'.
pushBy :: Time -> Pattern a -> Pattern a
pushBy t pat = (pure t) ~> pat

-- | Random per-event velocity jitter, for a less mechanical feel. The argument
-- scales the spread: @humanise 1@ varies @amp@ by up to &#177;0.09.
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


-- | Note-length constants, as fractions of a cycle: @1\/64@, @1\/32@, @1\/16@,
-- @1\/8@, @1\/4@ and @1\/2@ respectively. Useful as arguments to 'pullBy' and
-- 'pushBy', where a named length reads better than a bare fraction.
hemidemisemiquaver, demisemiquaver, semiquaver, quaver, crotchet, minim :: Time
hemidemisemiquaver = 1/64
demisemiquaver = 1/32
semiquaver = 1/16
quaver = 1/8
crotchet = 1/4
minim = 1/2
