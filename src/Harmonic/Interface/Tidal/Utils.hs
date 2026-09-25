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

    -- * Sidechain
    pump,
    -- * Monophony
    mono', retrig,
) where

import Data.List (sortOn)
import Data.Maybe (mapMaybe)
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

-- | A ducking gain envelope — a ghost sidechain. Every onset of the struct
-- pattern drops the gain to @1 - depth@, which then recovers to @1@ over the
-- release @rls@. A cycle is a beat here (@cps = bpm\/60@), so @rls@ reads
-- directly in beats: @1@ is one beat, @0.25@ a quarter of one.
--
-- @depth@ is @0@ for no duck, @1@ to duck to silence, and is a pattern, so it
-- can ride an LFO or the form.
--
-- The result is a 'Pattern Double' meant to drive a gain control. It belongs
-- BESIDE the notes as its own element in a stack, never applied to them:
--
-- @, s1pump \"1 0 0 0\" 0.6 0.75 # o@
--
-- Applying it to a note pattern with @|*@, @*|@ or @#@ splits every note once
-- per overlapping envelope step — a held note re-triggers sixteen times, and
-- drums double-trigger. A hand-written gain pattern coarser than, and aligned
-- to, the note grid (@|* vel \"0.45 1 0.8 1\"@) is the way to duck note
-- velocities.
--
-- An @rls@ longer than the gap to the next onset interleaves two envelopes: the
-- older tail\'s recovered values arrive after the new duck and cancel it.
-- Nothing clamps this.
pump :: Pattern Bool -> Pattern Double -> Double -> Pattern Double
pump st depth rls =
    stack [ rotR (toRational (rls * fromIntegral i / fromIntegral steps))
                 (struct st (fmap (gainAt i) depth))
          | i <- [0 .. steps] ]
  where
    -- 16 steps is a smooth ramp at roughly 31 CC/s per ducked beat at 118bpm,
    -- against a DIN budget of about 1000.
    steps = 16 :: Int
    gainAt i dp
      | i == steps = 1                        -- land exactly recovered
      | otherwise  = 1 - dp * exp (-4.5 * fromIntegral i / fromIntegral steps)

-- | Monophonic with latest-note priority: each event's whole is truncated at
-- the next onset in the pattern, so a new note always starts on time and stops
-- the one before it — a string being re-fingered, not a queue. Tidal's own
-- 'mono' is the opposite (first-note priority: a later overlapping event is
-- delayed or dropped). Simultaneous onsets keep only the first. Legato scales
-- the truncated whole, so @# legato 1@ rings exactly to the next note, @< 1@
-- leaves a gap, and cranking legato above 1 is unnecessary. Continuous
-- (whole-less) events pass through untouched.
mono' :: Pattern a -> Pattern a
mono' pat = splitQueries $ pat { query = f, steps = Nothing, pureValue = Nothing }
  where
    look = 8 :: Time
    onsetOf = start . wholeOrPart
    f st =
      let a    = arc st
          c0   = sam (start a)
          wide = query pat st { arc = Arc (c0 - look) (c0 + look + 1) }
          -- Every discrete event in the window, ordered by onset, one per onset.
          uniq = dedupe (sortOn onsetOf [ e | e <- wide, whole e /= Nothing ])
          onsets = map onsetOf uniq
          nextAfter s0 = case dropWhile (<= s0) onsets of
                           (x:_) -> Just x
                           []    -> Nothing
          build ev =
            let w  = wholeOrPart ev
                s0 = start w
                w' = Arc s0 (maybe (stop w) (min (stop w)) (nextAfter s0))
            in (\pt -> ev { whole = Just w', part = pt }) <$> subArc a w'
          analog = [ e | e <- query pat st, whole e == Nothing ]
      in mapMaybe build uniq ++ analog
    dedupe [] = []
    dedupe (e:es) = e : dedupe (dropWhile ((== onsetOf e) . onsetOf) es)

-- | Alias for 'mono'': a new note re-triggers the voice.
retrig :: Pattern a -> Pattern a
retrig = mono'
