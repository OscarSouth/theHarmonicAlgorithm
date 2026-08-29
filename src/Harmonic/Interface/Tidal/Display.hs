-- |
-- Module      : Harmonic.Interface.Tidal.Display
-- Description : 4-character LED display feed for the 12 Step controller
--
-- Broadcasts the truth from the Tidal side; SuperCollider paints what it
-- receives.
--
-- * CC 113 — bar number (1..8) at each chord onset; 0 on bar-off, an eighth
--   of a cycle later.
-- * CC 114 \/ CC 115 — 14-bit form loop length (high @\<\< 7 .|. low@). Zero
--   means atemporal (@lK@, or a single-node @iK@) and the counter cells stay
--   blank.
-- * CC 117 \/ CC 118 — 14-bit current form-local position, sampled 30 times a
--   cycle. Derived from the same cycle time that drives the form
--   interpolation, so the displayed value is always in lockstep with the
--   form's kinetics — no anchor signal needed.
--
-- SuperCollider owns CC 50-53, the display cells themselves; these are only
-- the signals feeding them. Both functions return a pattern, so the launcher
-- decides which stream carries it:
--
-- @, display k@   where   @display k = p \"displayClock\" $ displayClock k@
module Harmonic.Interface.Tidal.Display
  ( displayClock
  , displayClock'
  ) where

import Sound.Tidal.Context
import Harmonic.Interface.Tidal.Form (IK, Kinetics(..))

-- | Counter cells show elapsed SECONDS within the form loop.
displayClock :: IK -> ControlPattern
displayClock k =
  let loopSecs  = kLoopSecs (fst k)
      cpsV      = kCps (fst k)
      loopInt   = floor loopSecs :: Int
      hiByte    = fromIntegral (loopInt `div` 128) :: Double
      loByte    = fromIntegral (loopInt `mod` 128) :: Double
      thruCh10     = s "thru" # midichan 9             -- 1 event / cycle (constant CCs, struct-driven onsets)
      thruCh10Fast = fast 30 (s "thru") # midichan 9   -- 30 events / cycle (~56 Hz; for the seconds counter only)

      -- 1-indexed second counter, phase-locked to cycle 0. The displayed
      -- value is a continuous function of cycle time. Emission rate (~56 Hz
      -- at cps 1.867) comes from the *structural* side via thruCh10Fast on
      -- the CC 117/118 lines below — Tidal's `#` is `|>`, which reads
      -- structure from the left and only samples values from the right.
      -- Putting `segment 30` on this sig has no effect on emission rate;
      -- the leftmost pattern in the chain determines onsets.
      -- Values are 1..n inclusive (first second of the loop displays "1").
      -- Atemporal forms broadcast 0 continuously (SC treats this as blank).
      wholeSecs = floor loopSecs :: Int
      currentSecsPat = if wholeSecs >= 1 && cpsV > 0
        then sig $ \t ->
               let cyclesNow     = realToFrac t :: Double
                   secondsNow    = cyclesNow / cpsV
                   formLocalSecs = secondsNow - fromIntegral (floor (secondsNow / loopSecs) :: Int) * loopSecs
                   displayed     = (floor formLocalSecs :: Int) + 1
               in fromIntegral displayed :: Double
        else pure 0

      secsHiPat = fmap (\x -> fromIntegral ((floor x :: Int) `div` 128) :: Double) currentSecsPat
      secsLoPat = fmap (\x -> fromIntegral ((floor x :: Int) `mod` 128) :: Double) currentSecsPat

  in stack
       [ -- Bar onset: CC 113 = current bar (clamped to 1..8)
         (1/64) ~> (struct (fmap (const True) (snd k)) $
           thruCh10 # midicmd "control" # ctlNum 113
             # control (fmap (fromIntegral . min 8) (snd k)))
         -- Bar off: CC 113 = 0 at +1/8 cycle (P1 blank between flashes)
       , (1/64) ~> (struct ((pure (1/8)) ~> fmap (const True) (snd k)) $
           thruCh10 # midicmd "control" # ctlNum 113 # control 0)
         -- Loop length, high byte
       , thruCh10 # midicmd "control" # ctlNum 114 # control (pure hiByte)
         -- Loop length, low byte
       , thruCh10 # midicmd "control" # ctlNum 115 # control (pure loByte)
         -- Current form-local seconds, high byte (30 events/cycle from thruCh10Fast; ≤18 ms jitter)
       , thruCh10Fast # midicmd "control" # ctlNum 117 # control secsHiPat
         -- Current form-local seconds, low byte (30 events/cycle from thruCh10Fast; ≤18 ms jitter)
       , thruCh10Fast # midicmd "control" # ctlNum 118 # control secsLoPat
       ]

-- | As 'displayClock', but the counter shows the current BAR NUMBER
-- (1-indexed) within the form loop instead of elapsed seconds. One bar is
-- four cycles, so the count ticks in lockstep with the chord selection.
displayClock' :: IK -> ControlPattern
displayClock' k =
  let loopSecs     = kLoopSecs (fst k)
      cpsV         = kCps (fst k)
      cyclesPerBar = 4 :: Double
      loopBars     = loopSecs * cpsV / cyclesPerBar
      loopInt      = floor loopBars :: Int
      hiByte       = fromIntegral (loopInt `div` 128) :: Double
      loByte       = fromIntegral (loopInt `mod` 128) :: Double
      thruCh10     = s "thru" # midichan 9             -- 1 event / cycle (constant CCs, struct-driven onsets)
      thruCh10Fast = fast 30 (s "thru") # midichan 9   -- 30 events / cycle (catches the bar-boundary transition)

      -- 1-indexed bar counter, phase-locked to cycle 0. Ticks once per bar
      -- (every 4 cycles), in lockstep with the chord selection `snd k`.
      -- Values are 1..loopBars inclusive (first bar of the loop displays "1").
      -- Atemporal forms broadcast 0 continuously (SC treats this as blank).
      wholeBars = floor loopBars :: Int
      currentBarsPat = if wholeBars >= 1 && cpsV > 0
        then sig $ \t ->
               let cyclesNow     = realToFrac t :: Double
                   barsNow       = cyclesNow / cyclesPerBar
                   formLocalBars = barsNow - fromIntegral (floor (barsNow / loopBars) :: Int) * loopBars
                   displayed     = (floor formLocalBars :: Int) + 1
               in fromIntegral displayed :: Double
        else pure 0

      barsHiPat = fmap (\x -> fromIntegral ((floor x :: Int) `div` 128) :: Double) currentBarsPat
      barsLoPat = fmap (\x -> fromIntegral ((floor x :: Int) `mod` 128) :: Double) currentBarsPat

  in stack
       [ -- Bar onset: CC 113 = current bar (clamped to 1..8)
         (1/64) ~> (struct (fmap (const True) (snd k)) $
           thruCh10 # midicmd "control" # ctlNum 113
             # control (fmap (fromIntegral . min 8) (snd k)))
         -- Bar off: CC 113 = 0 at +1/8 cycle (P1 blank between flashes)
       , (1/64) ~> (struct ((pure (1/8)) ~> fmap (const True) (snd k)) $
           thruCh10 # midicmd "control" # ctlNum 113 # control 0)
         -- Loop length in bars, high byte
       , thruCh10 # midicmd "control" # ctlNum 114 # control (pure hiByte)
         -- Loop length in bars, low byte
       , thruCh10 # midicmd "control" # ctlNum 115 # control (pure loByte)
         -- Current form-local bar, high byte
       , thruCh10Fast # midicmd "control" # ctlNum 117 # control barsHiPat
         -- Current form-local bar, low byte
       , thruCh10Fast # midicmd "control" # ctlNum 118 # control barsLoPat
       ]
