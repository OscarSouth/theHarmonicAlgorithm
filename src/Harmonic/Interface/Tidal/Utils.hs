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

    -- * Swing
    swing8, swing16,

    -- * Selection
    over, (-->),

    -- * Random gating
    binaryrange,
) where

import Sound.Tidal.Context

-- | Transpose by whole octaves. @oct 1@ is up an octave, @oct (-1)@ down.
-- The shift is a pattern, so it can move across the cycle: @|+ oct "[0 1]*2"@.
--
-- Add onto a pattern:
--
-- @, cello T (0,1) k vl grid Bass |+ oct (-1)@
oct :: Pattern Note -> Pattern ValueMap
oct k = note (12 * k)

-- | Rotate a pattern earlier ('pullBy') or later ('pushBy') in time.
-- Function forms of the TidalCycles @\<~@ and @~>@ operators, so they compose
-- in a modifier chain rather than needing parentheses. The amount is a
-- pattern, so the rotation can itself vary across the cycle.
--
-- @, pushBy (1\/8) $ harp T (0,1) k vl flow Alto@
pullBy :: Pattern Time -> Pattern a -> Pattern a
pullBy = (<~)

-- | Rotate a pattern later in time. See 'pullBy'.
pushBy :: Pattern Time -> Pattern a -> Pattern a
pushBy = (~>)

-- | Random per-event velocity jitter, for a less mechanical feel. The argument
-- scales the spread: @humanise 1@ varies @amp@ by up to ±0.09. The value
-- is centred on ZERO — combine with @|+@ (\"|+ humanise 0.2\"), never
-- @#@, which would replace the amp instead of jittering it.
humanise :: Double -> Pattern ValueMap
humanise x = pF "amp" (range (pure (-0.09 * x)) (pure (0.09 * x)) rand)

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

-- | Swing by musical PROPORTION: @0.5@ is straight, @0.667@ triplet. Places
-- the swung note at exactly that proportion of its subdivision.
--
-- One cycle is one beat here, so 'swing8' swings eighth-notes (the \"&\") and
-- 'swing16' swings sixteenths (the \"e\"\/\"a\"). Jazz eighth-feel —
-- spang-a-lang, offbeat eighths — takes 'swing8'; a sixteenth shuffle (funk,
-- UK garage, house, phonk, fusion) takes 'swing16'.
--
-- @f = swing8 0.6@
swing8, swing16 :: Pattern Time -> Pattern a -> Pattern a
swing8  x = swingBy (x - 0.5) 1
swing16 x = swingBy (x - 0.5) 2

-- | Step through a list under a 0-1 control signal: the signal's range is
-- divided evenly among the elements, so a knob sweeps through them in order.
-- An empty list is silence.
--
-- @, over qlink1 [flow, grid, lite]@
over :: Pattern Double -> [a] -> Pattern a
over _ [] = silence
over ctrl xs =
  let count = length xs
      step  = 1 / fromIntegral count
  in fmap (\x -> xs !! max 0 (floor (min (fromIntegral (count - 1)) (x / step)))) ctrl

-- | Operator form of 'over'.
(-->) :: Pattern Double -> [a] -> Pattern a
(-->) = over

-- | A boolean gate whose density wanders between two step counts, via
-- 'binary' over a random integer in @[lo, hi)@.
binaryrange :: Pattern Int -> Pattern Int -> Pattern Bool
binaryrange lo hi = binary $ lo |+ irand (hi - lo)
