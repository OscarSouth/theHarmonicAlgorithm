-- |
-- Module      : Harmonic.Interface.Tidal.Devices.RC10R
-- Description : Boss RC-10R Rhythm Loop Station — structural looper control
--
-- The pedal's own switches own everything one-off or stateful: recording,
-- undo\/redo, phrase memories, clearing. This module covers the timed,
-- repeated and structural side: starting and stopping the recorded tracks,
-- breaks, the preset rhythm's start, stop, fills and A\/B groove, the rhythm
-- kit, and levels that follow the form.
--
-- Every function is a control change on MIDI channel 10, CC 70-84. The rig's
-- X3 interface consumes that lane before the MPC sees it and re-sends it on
-- the RC-10R's own wire (X3 Out C): CC 70-83 on the RC-10R's control channel
-- (1), CC 84 as a program change on its note channel (2). The pedal's MIDI
-- menu carries the matching assignments:
--
-- @
-- CC  RC-10R function        CC  RC-10R function
-- 70  TRK1 START             78  RHY STOP
-- 71  TRK1 STOP              79  RHY FILLIN
-- 72  TRK2 START             80  RHY DIVISION
-- 73  TRK2 STOP              81  LOOP LEVEL
-- 74  LOOP STOP              82  RHY LEVEL
-- 75  ALL BREAK              83  TOTAL LEVEL
-- 76  RHY BREAK              84  rhythm kit (program change, value = kit 1-16)
-- 77  RHY START
-- RX CONTROL CH 1 · RX NOTE CH 2 · SYNC CLOCK MIDI (Tidal's clock, re-clocked by the X3)
-- @
--
-- Presses take a gate, like the drum names: @rcFill \"[0 0 0 0 0 0 0 1]\/32\"@,
-- @mark 33 k [silence, rcBreak \"1\"]@, @once $ rcStop \"1\"@.
--
-- The pedal's drum kit is played through the MPC on a second lane, ch 10
-- CC 33-48: CC 33 + pad, value = velocity. The X3 consumes it and sends each
-- hit as note 19-34 (pad + 19, clear of every other ch 10 note) out of its USB
-- port 2 (Drums) to an MPC MIDI track (pads Chromatic C-2 transposed +19), which
-- records or plays it and sends it on through port 3
-- (Looper); the X3 translates those notes to the kit's own and plays them on
-- the note channel. A @dkit@ block is written with the shared drum names
-- ("Harmonic.Interface.Tidal.Devices.JV1010", notes 0-15) and finished by
-- 'x3rc10'.
module Harmonic.Interface.Tidal.Devices.RC10R
  (
    -- * Loop tracks
    rcTrk1, rcTrk1Stop, rcTrk2, rcTrk2Stop, rcStop

    -- * Breaks
  , rcBreak, rcRhythmBreak

    -- * Preset rhythm
  , rcRhythm, rcRhythmStop, rcFill, rcPattern, rcKit

    -- * Levels
  , rcLoopLevel, rcRhythmLevel, rcLevel

    -- * Loops entering with the form
  , rcGate

    -- * Drum kit
  , x3rc10
  ) where

import qualified Data.Map.Strict as Map
import Sound.Tidal.Context
import Harmonic.Interface.Tidal.Instruments (ch)
import Harmonic.Interface.Tidal.Form (Kinetics(..), IK, beatsPerBar)

-- The lane every RC-10R message rides: channel 10, consumed by the X3.
rcLane :: Pattern ValueMap
rcLane = ch 10

-- A raw control-change value (0-127) on the lane.
ccRaw :: Pattern Double -> Pattern Double -> ControlPattern
ccRaw num v = control v # midicmd "control" # ctlNum num # rcLane

-- | A footswitch press on CC @num@ at each true onset of the gate: value 127,
-- then 0 a sixteenth of a cycle later, so every press is a fresh transition.
pressCC :: Pattern Double -> Pattern Bool -> ControlPattern
pressCC num g = stack
  [ struct g (ccRaw num 127)
  , (1/16) ~> struct g (ccRaw num 0)
  ]

-------------------------------------------------------------------------------
-- Loop tracks
-------------------------------------------------------------------------------

-- | Start or stop a recorded track. With LOOP TRACK MODE set to PARALLEL the
-- two tracks are independent, so they can be arranged like instruments.
rcTrk1, rcTrk1Stop, rcTrk2, rcTrk2Stop :: Pattern Bool -> ControlPattern
rcTrk1     = pressCC 70
rcTrk1Stop = pressCC 71
rcTrk2     = pressCC 72
rcTrk2Stop = pressCC 73

-- | Stop the looper (both tracks), observing the pedal's STOP MODE.
rcStop :: Pattern Bool -> ControlPattern
rcStop = pressCC 74

-------------------------------------------------------------------------------
-- Breaks
-------------------------------------------------------------------------------

-- | ALL BREAK: rhythm and loops cut out for the rest of the bar and return on
-- the next downbeat.
rcBreak :: Pattern Bool -> ControlPattern
rcBreak = pressCC 75

-- | RHY BREAK: the same, drums only; the loops keep playing.
rcRhythmBreak :: Pattern Bool -> ControlPattern
rcRhythmBreak = pressCC 76

-------------------------------------------------------------------------------
-- Preset rhythm
-------------------------------------------------------------------------------

-- | The preset-rhythm footswitches: RHY START, RHY STOP (observing the pedal's
-- STOP MODE), RHY FILLIN, and RHY DIVISION, which toggles PTN1 \/ PTN2 with the
-- pedal holding the state between presses.
rcRhythm, rcRhythmStop, rcFill, rcPattern :: Pattern Bool -> ControlPattern
rcRhythm     = pressCC 77   -- RHY START
rcRhythmStop = pressCC 78   -- RHY STOP, observing the pedal's STOP MODE
rcFill       = pressCC 79   -- RHY FILLIN
rcPattern    = pressCC 80   -- RHY DIVISION: toggles PTN1 / PTN2; the pedal holds the state

-- | Select the rhythm kit, 1-16, in Roland's order: Studio, Live, Light,
-- Heavy, Rock, Metal, Jazz, Brushes, Cajon, Drum&Bs, R&B, Dance, Techno,
-- Dance Beats, Hiphop, 808+909. One message per event:
-- @mark 1 k [silence, rcKit 8]@.
rcKit :: Pattern Double -> ControlPattern
rcKit = ccRaw 84

-------------------------------------------------------------------------------
-- Levels
-------------------------------------------------------------------------------

-- | Playback levels, 0-1. Any signal — a constant, an @lfo@, or the form's own
-- @kDynamic (fst k)@ — sampled sixteen times per bar.
rcLoopLevel, rcRhythmLevel, rcLevel :: Pattern Double -> ControlPattern
rcLoopLevel   = levelCC 81
rcRhythmLevel = levelCC 82
rcLevel       = levelCC 83   -- TOTAL LEVEL: rhythm and loops together

levelCC :: Pattern Double -> Pattern Double -> ControlPattern
levelCC num v = ccRaw num (fmap (\x -> fromIntegral (round (max 0 (min 1 x) * 127) :: Int)) (segment 4 v))

-------------------------------------------------------------------------------
-- Loops entering with the form
-------------------------------------------------------------------------------

-- | A recorded track that enters and leaves with the form, like a section at
-- its kinetics threshold: TRK@trk@ START on the first bar the form's kinetics is
-- inside @(lo, hi)@, TRK@trk@ STOP on the first bar it is outside. Sampled at
-- each downbeat; only the crossings send anything, so no state is kept.
rcGate :: (Double, Double) -> IK -> Int -> ControlPattern
rcGate (lo, hi) k trk =
  let bar      = realToFrac beatsPerBar :: Time
      inBand   = fmap (\x -> x >= lo && x <= hi) (kSignal (fst k))
      now      = slow (pure bar) (segment 1 (fast (pure bar) inBand))   -- one sample per bar, at the downbeat
      before   = bar `rotR` now                            -- the previous bar's sample
      edge f   = filterValues id (f <$> now <*> before)
      entering = edge (\a b -> a && not b)
      leaving  = edge (\a b -> not a && b)
      (onTrk, offTrk) = if trk == 2 then (rcTrk2, rcTrk2Stop) else (rcTrk1, rcTrk1Stop)
  in stack [onTrk entering, offTrk leaving]

-------------------------------------------------------------------------------
-- Drum kit
-------------------------------------------------------------------------------

-- | The @dkit@ block's finaliser: every drum-pad event (notes 0-15) becomes a
-- CC on the drum lane, CC 33 + pad with its final velocity (@amp@ x 127) as the
-- value. It comes last because a block applies @|* vel d@ after the names. An
-- event without @amp@ takes SuperDirt's default, 0.4, as it would on the MPC
-- kits. Any other note, and a velocity that rounds to 0, is dropped.
x3rc10 :: ControlPattern -> ControlPattern
x3rc10 = filterJust . fmap toLane
  where
    toLane vm = do
      nt <- Map.lookup "note" vm >>= getN
      let a   = maybe 0.4 id (Map.lookup "amp" vm >>= getF)
          pad = round (unNote nt) + 60 :: Int
          v   = min 127 (round (a * 127)) :: Int
      if pad < 0 || pad > 15 || v < 1 then Nothing else
        Just $ Map.union
          (Map.fromList [ ("s", VS "thru"), ("midichan", VF 9), ("midicmd", VS "control")
                        , ("ctlNum", VF (fromIntegral (33 + pad))), ("control", VF (fromIntegral v)) ])
          (foldr Map.delete vm ["note", "amp", "sustain"])
