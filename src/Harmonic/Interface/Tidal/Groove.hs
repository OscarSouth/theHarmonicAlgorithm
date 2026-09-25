-- |
-- Module      : Harmonic.Interface.Tidal.Groove
-- Description : Rhythm section interface for sub-bass and kick patterns
--
-- Provides 'subKick' (sub-bass and kick, held by note duration) and 'fund'
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
-- Each sub note rings from its onset to the next kill boundary by its own note
-- length ('holdToNext' + @legato 1@) — see 'subKick' for why the hold is by
-- duration and not by a CC64 pedal.

module Harmonic.Interface.Tidal.Groove
  ( fund
  , subKick
  , holdToNext
  , noteoff
    -- * Clave and cascara grids
  , son32, son23, rumba32, rumba23, bossa32, bossa23, bellpat32, bellpat23
  ) where

import qualified Harmonic.Rules.Types.Pitch as Pitch
import qualified Harmonic.Rules.Import.Jazz as J
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Progression as P
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import Harmonic.Interface.Tidal.Form (Kinetics(..), IK, ki)
import Data.List (nub, sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Foldable (toList)
import Sound.Tidal.Context

-- | Extract harmonic roots regardless of inversion — the harmonic
-- FUNDAMENTAL uniformly, where 'Harmonic.Interface.Tidal.Arranger.root'
-- is uniformly the sounding bass. Triads go through inversion detection
-- (a first-inversion bar reports its harmonic root, not its bass). Bars
-- of more than three tones ask the jazz namer for the rotation: a slash
-- structure (Bb7 over Ab) reports its true root (Bb), matching the
-- chart-convention grid name; a root-position extended chord — and any
-- set the namer cannot read — keeps its stored anchor. The walking bass
-- is unaffected either way: it anchors on the stored root internally
-- and reads a voice function only for beat-1 pitch choice.
fund :: P.Progression -> [[Int]]
fund prog =
  let cadenceStates = toList (P.unProgression prog)
  in map fundToInt cadenceStates
  where
    fundToInt :: H.CadenceState -> [Int]
    fundToInt cs
      | length (H.cadenceIntervals (H.stateCadence cs)) > 3 =
          let anchor = Pitch.unPitchClass (Pitch.pitchClass (H.stateCadenceRoot cs))
              ivs    = map Pitch.unPitchClass (H.cadenceIntervals (H.stateCadence cs))
          in case J.jazzFunctionalityR ivs of
               Just (_, d) | d /= 0 -> [(anchor + d) `mod` 12]
               _                    -> [anchor]
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

-- | Hold each onset of @pat@ until the next onset in @bounds@, rewriting the
-- event whole to @[onset, nextBoundary)@ (continuation fragments across cycles
-- are preserved). A boundary is where the note must stop, so the note stops
-- exactly there. Pair with @# legato 1@ so the emitted MIDI note-off lands on the
-- boundary. Values are sampled once at the onset and held for the whole note
-- (no mid-note re-sampling). @look@ is the lookahead\/lookback in cycles; note
-- lengths must not exceed it.
holdToNext :: Time -> Pattern Bool -> Pattern a -> Pattern a
holdToNext look bounds pat =
  splitQueries $ pat { query = f, steps = Nothing, pureValue = Nothing }
  where
    f st =
      let a    = arc st
          c0   = sam (start a)
          -- Onsets from up to `look` cycles back may still be sounding now.
          ons  = filter eventHasOnset
                   $ query pat st { arc = Arc (c0 - look) (c0 + 1) }
          bnds = sortOn id
                   $ map (start . wholeOrPart)
                   $ filter (\e -> eventHasOnset e && value e)
                   $ query bounds st { arc = Arc (c0 - look) (c0 + look + 1) }
          nextB s0 = case filter (> s0) bnds of
                       (x:_) -> x
                       []    -> c0 + look + 1
          build ev =
            let s0 = start (wholeOrPart ev)
                w  = Arc s0 (nextB s0)
            in (\pt -> ev { whole = Just w, part = pt }) <$> subArc a w
      in mapMaybe build ons

-- | Groove interface: kick and sub bass locked to the harmony, with chord
-- selection and kinetics gating from 'IK'.
--
-- Sub on\/off patterns and the kick pattern are bar-relative:
-- @\"[1]\/2\"@ = one onset every 2 bars, @\"1*4\"@ = 4 kicks per bar.
--
-- Each sub note is held by its own duration: from its onset to the next kill
-- boundary — the manual offs (@subOffPat@) and, for @maxDur < 1@, each onset
-- shifted by @maxDur*4@ — emitted with @legato 1@ so the note-off lands on the
-- boundary. The hold is by note length, not by a CC64 pedal: the MPC sub
-- program does not treat CC64 as a damper (a MIDI note-off arriving under a
-- held pedal is stranded), so no CC64 is emitted here at all.
--
-- Chord selection uses 'innerJoin' — we WANT new note-ons when the chord
-- changes. Sub is gated at @(0.1, 1)@ and kick at @(0.2, 1)@ via 'ki'. The
-- launcher dynamic is the only dynamic applied — 'subKick' deliberately
-- ignores the form's 'kDynamic' envelope, so the sub holds its level while the
-- orchestra swells and ducks around it.
subKick :: Pattern Double
         -> IK
         -> (P.Progression -> [[Int]])
         -> (Time, String, String, String)
         -> Pattern ValueMap
subKick dyn k voiceFunc (maxDur, subOnStr, subOffStr, kickStr) =
  let (kin, chordPat) = k
      subOnPat  = slow 4 $ parseBP_E subOnStr
      subOffPat = slow 4 $ parseBP_E subOffStr
      kickPat   = slow 4 $ parseBP_E kickStr
      progPat = fmap PC.triadLayer (kProg kin)
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
-- pre-parsed patterns, and carries the note-duration \/ kill-boundary logic.
-- LEDs are not emitted here — the SC-side coordinator derives them from
-- outgoing MIDI on ch 10.
subKickCoreP :: ([Int], Int)
              -> Pattern Bool
              -> Pattern Bool
              -> Pattern Bool
              -> Pattern Int
              -> Pattern Double
              -> IK
              -> Time
              -> Pattern ValueMap
subKickCoreP (normPitches, nChords) subOnPat subOffPat kickPat chordPat dyn k maxDur
  | nChords == 0 = silence
  | otherwise =
  let
    ledCC num cval = midicmd "control"
                  # ctlNum (fromIntegral num)
                  # control (fromIntegral cval)

    dynGate = fmap (> 0) dyn
    thru = s "thru" # midichan 9
    chordIdx = fmap (\i -> (i - 1) `mod` nChords) chordPat

    -- Kill boundaries: manual offs (subOffPat) plus auto offs (each onset +
    -- maxDur*4). maxDur >= 1 means manual-off only.
    killBounds
      | maxDur >= 1 = subOffPat
      | otherwise   = stack [subOffPat, (pure (maxDur * 4)) ~> subOnPat]

    -- Note-ons with pitch sampled once per onset, then each whole extended to
    -- the next kill boundary and emitted as a real note (legato 1 -> SuperDirt
    -- sends the note-off on the boundary).
    subOnsets = mask dynGate $ struct subOnPat $
      innerJoin (fmap (\ci ->
        midinote (pure $ fromIntegral (normPitches !! (ci `mod` nChords)))
        # amp dyn
      ) chordIdx)
    subPattern = holdToNext 8 killBounds subOnsets # legato 1

    -- Kick: fixed C3 (MIDI 48), one-shot.
    kickPattern = struct kickPat $ midinote 48 # sustain 0.01 # amp 1
    kickLedOn  = (1/64) ~> (struct kickPat $ ledCC (32 :: Int) (1 :: Int))
    kickLedOff = (1/64) ~> (struct ((pure (1/8)) ~> kickPat) $ ledCC (32 :: Int) (0 :: Int))

    subGroup  = ki (0.1, 1) k $ subPattern # thru
    kickGroup = ki (0.2, 1) k $ stack
      [ kickPattern # thru
      , kickLedOn # thru, kickLedOff # thru
      ]

  in stack [subGroup, kickGroup]

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
