-- |
-- Module      : Harmonic.Interface.Tidal.Groove
-- Description : Rhythm section interface for sub-bass and kick patterns
--
-- Provides 'subKick' (sub-bass with CC64 sustain pedal) and 'fund'
-- (fundamental bass note extraction) for rhythm-section integration
-- with harmonically-generated progressions.
--
-- 'subKick' tracks the harmony: it reads the fundamental of whichever bar the
-- form is currently on, so the sub follows a modulation without being
-- rewritten. In a launcher it is an ordinary block:
--
-- @
-- subk f k d = p \"subk\" $ f
--   $ subKick d k fund (1\/2, \"1 ~ ~ ~\", \"~\", \"~ ~ 1 ~\")
-- @
--
-- (dynamics, context, voice strategy, then (maxDur, sub-on, manual-off,
-- kick) pattern strings). Dynamics discipline: the launcher @d@ is the
-- only dynamic applied — 'subKick' deliberately ignores the form's
-- 'kDynamic' envelope, so the sub holds its level while the orchestra
-- swells and ducks around it.
--
-- The note is held by a CC64 sustain pedal rather than by note length, so the
-- sub rings between onsets instead of retriggering — see 'subKick' for why
-- the sustain value and the pedal are paired.

module Harmonic.Interface.Tidal.Groove
  ( fund
  , subKick
  , noteoff
    -- * Clave and cascara grids
  , son32, son23, rumba32, rumba23, bossa32, bossa23, bellpat32, bellpat23
  ) where

import qualified Harmonic.Rules.Types.Pitch as Pitch
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Progression as P
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import Harmonic.Interface.Tidal.Form (Kinetics(..), IK, ki)
import Data.List (nub, sortOn)
import Data.Maybe (catMaybes)
import Data.Foldable (toList)
import Sound.Tidal.Context

-- | Extract harmonic roots regardless of inversion.
-- Triads go through inversion detection (a first-inversion bar reports
-- its harmonic root, not its bass). Bars of more than three tones keep
-- their STORED root: jazz and hand-built extended chords store the
-- anchor directly, and the triad-reduction path would re-derive a root
-- from the most-consonant embedded triad — a different answer from the
-- one 'Harmonic.Interface.Tidal.Arranger.root' and the walking bass give
-- for the same bar.
fund :: P.Progression -> [[Int]]
fund prog =
  let cadenceStates = toList (P.unProgression prog)
  in map fundToInt cadenceStates
  where
    fundToInt :: H.CadenceState -> [Int]
    fundToInt cs
      | length (H.cadenceIntervals (H.stateCadence cs)) > 3 =
          [Pitch.unPitchClass (Pitch.pitchClass (H.stateCadenceRoot cs))]
      | otherwise =
          let chord = H.fromCadenceState cs
              rootNoteName = H.chordNoteName chord
              rootPc = Pitch.pitchClass rootNoteName
          in [Pitch.unPitchClass rootPc]

-- | Truncate each gate onset's note length to at most @1\/n@ of a bar (bar = 4
--   cycles), else extend it to the next onset. Only @True@ onsets sound; a
--   truncated tail is a rest; onsets are not moved. Pair with @# legato 1@ on
--   sustaining instruments to hear the length. Precondition: @n > 0@.
--
--   Bar patterns are written @\"\/4\"@ (1 cycle = 1 beat, 1 bar = 4 cycles), so
--   e.g. @noteoff 4@ caps each hit at a quarter note (1 cycle):
--
--   > noteoff 4 "[[1 0 0 0] [0 0 0 0] [1 0 0 0] [1 0 0 0]]\/4"  ==  "[1 0 1 1]\/4"
noteoff :: Time -> Pattern Bool -> Pattern Bool
noteoff nBeats p = splitQueries $ p { query = f, steps = Nothing, pureValue = Nothing }
  where
    barLen = 4
    cap    = barLen / nBeats
    f st =
      let a   = arc st
          b0  = barLen * sam (start a / barLen)          -- enclosing-bar start
          ons = sortOn (start . wholeOrPart)
                  $ filter (\e -> eventHasOnset e && value e)
                  $ query p st { arc = Arc b0 (b0 + barLen) }
          nexts = drop 1 (map (start . wholeOrPart) ons) ++ [b0 + barLen]
          build ev nx =
            let s0 = start (wholeOrPart ev)
                w  = Arc s0 (min nx (s0 + cap))
            in (\pt -> ev { whole = Just w, part = pt }) <$> subArc a w
      in catMaybes (zipWith build ons nexts)

-- | Normalize pitch classes to C2-B2 range (MIDI 36-47) for MPC sub program.
-- Empty list returns 35 (B1, where no sample is assigned = silence).
-- Pitch classes [0-11] map to MIDI [36-47] (C2-B2).
normalizeToSubRange :: [Int] -> Int
normalizeToSubRange [] = 35  -- B1: no sample (silence)
normalizeToSubRange (pc:_) = 36 + (pc `mod` 12)

-- | Groove interface using patterned chord selection with kinetics gating.
--
-- CC64 sustain mechanism with chord selection from 'IK'.
-- Sub on\/off patterns and kick pattern are bar-relative:
-- @\"[1]\/2\"@ = one onset every 2 bars, @\"1*4\"@ = 4 kicks per bar.
--
-- Chord selection uses 'innerJoin' — we WANT new note-ons when the
-- chord changes (unlike melodic instruments where sustain across
-- boundaries is desirable).
--
-- The progression is read from @kProg@ via @innerJoin@.
-- Sub is gated at @(0.1, 1)@ and kick at @(0.2, 1)@ via @ki@.
subKick :: Pattern Double               -- ^ Dynamics pattern (> 0 = sub active)
        -> IK                            -- ^ Performance context (kinetics + chord selection)
        -> (P.Progression -> [[Int]])    -- ^ Voice strategy (fund or bass)
        -> (Time,                        -- ^ Max sub duration before auto-off
            String,                      -- ^ Sub note on pattern string
            String,                      -- ^ Manual note off pattern string
            String)                      -- ^ Kick placement pattern string
        -> Pattern ValueMap
subKick dyn k voiceFunc (maxDur, subOnStr, subOffStr, kickStr) =
  let (kin, chordPat) = k
      -- Parse pattern strings ONCE at construction time (not per progression change)
      subOnPat  = slow 4 $ parseBP_E subOnStr
      subOffPat = slow 4 $ parseBP_E subOffStr
      kickPat   = slow 4 $ parseBP_E kickStr
      progPat = fmap PC.triadLayer (kProg kin)
      -- Exact cache domain from the form's own progression list (see
      -- 'Harmonic.Interface.Tidal.Bridge.arrange' — no Arc-window
      -- sampling), forced at construction like the other Layer-D emitters
      -- so no voicing work runs on the audio thread.
      uniqueProgs = nub (map PC.triadLayer (kProgs kin))
      cache = [(p, let raw = voiceFunc p
                       norm = map normalizeToSubRange raw
                       nc = length norm
                   in (norm, nc))
              | p <- uniqueProgs]
      cacheForced = foldr (\(_, (ns, cnt)) acc -> sum ns `seq` cnt `seq` acc) () cache
      lookupCache prog = case lookup prog cache of
        Just hit -> hit
        Nothing  -> let raw = voiceFunc prog
                    in (map normalizeToSubRange raw, length raw)
  in cacheForced `seq` innerJoin $ fmap (\prog ->
       subKickCoreP (lookupCache prog) subOnPat subOffPat kickPat chordPat dyn k maxDur
     ) progPat

-- |Cached subKick core: takes pre-computed (normPitches, nChords) and
-- pre-parsed patterns, and carries all CC64\/sustain\/timing logic. LEDs are
-- not emitted here — the SC-side coordinator derives them from outgoing
-- MIDI on ch 10.
subKickCoreP :: ([Int], Int)
             -> Pattern Bool             -- ^ Pre-parsed sub on pattern
             -> Pattern Bool             -- ^ Pre-parsed sub off pattern
             -> Pattern Bool             -- ^ Pre-parsed kick pattern
             -> Pattern Int              -- ^ Chord selection pattern
             -> Pattern Double           -- ^ Dynamics
             -> IK
             -> Time                     -- ^ Max sub duration
             -> Pattern ValueMap
subKickCoreP (normPitches, nChords) subOnPat subOffPat kickPat chordPat dyn k maxDur
  | nChords == 0 = silence
  | otherwise =
  let
    -- CC helper
    midiCC num cval = midicmd "control" # ctlNum num # control cval

    -- LED feedback helper (only used for the kick high-C indicator on CC 32;
    -- the 12 pitch-class LEDs CC 20-31 are driven by the SC-side coordinator)
    ledCC num cval = midicmd "control"
                  # ctlNum (fromIntegral num)
                  # control (fromIntegral cval)

    -- Convert dynamics to boolean gate for mask
    dynGate = fmap (> 0) dyn

    -- MIDI routing: channel 10 (0-indexed = midichan 9) on "thru" device
    thru = s "thru" # midichan 9

    -- 0-indexed chord index from 1-indexed input, wrapping modulo nChords
    chordIdx = fmap (\i -> (i - 1) `mod` nChords) chordPat

    -- Sub pattern: note-ons gated by dynamics and structured by subOnPat
    subPattern = mask dynGate $ struct subOnPat $
      innerJoin (fmap (\ci ->
        midinote (pure $ fromIntegral (normPitches !! (ci `mod` nChords)))
        # sustain 0.01 # amp dyn
      ) chordIdx)

    -- Kick pattern: fixed C3 (MIDI 48), one-shot
    kickPattern = struct kickPat $ midinote 48 # sustain 0.01 # amp 1

    -- Sustain pedal: CC 64 = 127 continuous background
    -- 1\/128 offset avoids timestamp collision with note-on events
    sustainOn = (1/128) ~> segment 16 (midiCC 64 127)

    -- Auto note-off: CC 64 = 0 shifted by maxDur after each note-on
    autoOff
      | maxDur >= 1 = silence
      | otherwise   = struct ((pure (maxDur * 4)) ~> subOnPat) $ midiCC 64 0

    -- Manual note-off: CC 64 = 0 at user-specified boundaries
    manualOff = struct subOffPat $ midiCC 64 0

    -- Kick LED: 1\/64 offset puts the CC on its own SuperDirt dispatch tick
    -- so it doesn't collide with the kick note under MIDI burst load.
    -- Pulse extended to 1\/8 cycle for reliable visual response.
    kickLedOn  = (1/64) ~> (struct kickPat $ ledCC (32 :: Int) (1 :: Int))
    kickLedOff = (1/64) ~> (struct ((pure (1/8)) ~> kickPat) $ ledCC (32 :: Int) (0 :: Int))

    -- Sub group: sub pattern + CC64 sustain
    subGroup = ki (0.1, 1) k $ stack
      [ subPattern # thru
      , sustainOn # thru, autoOff # thru, manualOff # thru
      ]

    -- Kick group: kick pattern + kick LED (CC 32, high-C indicator)
    kickGroup = ki (0.2, 1) k $ stack
      [ kickPattern # thru
      , kickLedOn # thru, kickLedOff # thru
      ]

    -- Pedal up: CC64=0 when sub is inactive (kinetics below threshold)
    -- Resets physical instrument to default touch behaviour
    pedalUp = mask (fmap (< 0.1) (kSignal (fst k))) $ segment 1 (midiCC 64 0) # thru

  in stack [subGroup, kickGroup, pedalUp]

-- | Explicit 16-step (2-bar) boolean grids, one stroke per position, for
-- clave and cascara feels. Use with @struct@ or @mask@ on any instrument or
-- drum part.
--
-- @32@ is the 3-2 orientation; @23@ is the 2-3 rotation (@2 \<~@ over the
-- @\/4@ two-bar span swaps the halves). Onset positions are 1-indexed.
son32, son23, rumba32, rumba23, bossa32, bossa23,
  bellpat32, bellpat23 :: Pattern Bool
son32     = "[1 0 0 1 0 0 1 0 0 0 1 0 1 0 0 0]/4"   -- son clave 3-2:   1 4 7 11 13
son23     = 2 <~ son32
rumba32   = "[1 0 0 1 0 0 0 1 0 0 1 0 1 0 0 0]/4"   -- rumba clave 3-2: 1 4 8 11 13
rumba23   = 2 <~ rumba32
bossa32   = "[1 0 0 1 0 0 0 1 0 0 1 0 0 1 0 0]/4"   -- bossa clave 3-2: 1 4 8 11 14
bossa23   = 2 <~ bossa32
bellpat32 = "[1 0 1 0 1 1 0 1 1 0 1 1 0 1 0 1]/4"   -- bell / cascara:  1 3 5 6 8 9 11 12 14 16
bellpat23 = 2 <~ bellpat32
