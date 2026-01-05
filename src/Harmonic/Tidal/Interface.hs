{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Harmonic.Tidal.Interface
-- Description : TidalCycles interface for harmonic progressions
-- 
-- This module provides the bridge between the harmonic generation engine
-- and TidalCycles live coding. Key design principles:
--
-- 1. Pattern-based lookup with modulo wrap: Indices wrap around the
--    progression length, so `run 4` on a 16-bar progression loops the
--    first 4, and `iter` creates rotating windows.
--
-- 2. Preserve launcher ergonomics: The `arrange` function maintains
--    backward compatibility with the instrument launcher paradigm
--    (juno f s r d, moog f s r d, etc.)
--
-- 3. Voice extraction: Uses voicing paradigms from Arranger module
--    (flow, tall, slim, wide, lite) for different instrumental roles.

module Harmonic.Tidal.Interface 
  ( -- * Voice Functions
    VoiceFunction
  , rootNotes
  , bassNotes
  
    -- * Pattern Application
  , arrange
  , applyProg
  , voiceRange
  
    -- * Pattern-Based Lookup (New)
  , lookupProgression
  , lookupChord
  , VoiceType(..)
  , voiceBy
  , harmony
  
    -- * Utilities
  , overlapF
  ) where

-- Phase B imports
import qualified Harmonic.Core.Progression as P
import qualified Harmonic.Core.Harmony as H
import qualified Harmonic.Core.Pitch as Pitch
import qualified Harmonic.Tidal.Arranger as A

import Sound.Tidal.Context hiding (voice)  -- Hide to avoid conflict
import qualified Data.List as L
import Data.Foldable (toList)

-------------------------------------------------------------------------------
-- Voice Function Types (Using Phase B Progression)
-------------------------------------------------------------------------------

-- |Voice function type: extracts integer pitch sequences from progression
type VoiceFunction = P.Progression -> [[Int]]

-- |Extract root note only (for bass lines, lead melodies)
-- Renamed from 'root' to avoid conflict with Arranger exports
rootNotes :: VoiceFunction
rootNotes prog = 
  let cadenceStates = toList (P.unProgression prog)
  in map rootToInt cadenceStates
  where
    rootToInt :: H.CadenceState -> [Int]
    rootToInt cs = 
      let rootNoteName = H.stateCadenceRoot cs
          rootPc = Pitch.pitchClass rootNoteName
      in [Pitch.unPitchClass rootPc]

-- |Extract bass note (first interval in chord voicing)
-- Renamed from 'bass' to avoid conflict with Arranger exports
bassNotes :: VoiceFunction
bassNotes prog = map bassToInt (P.progChords prog)
  where
    bassToInt :: H.Chord -> [Int]
    bassToInt chord = 
      case H.chordIntervals chord of
        []    -> [0]
        (p:_) -> [fromIntegral p]

-------------------------------------------------------------------------------
-- Pattern Application (Legacy Compatible)
-------------------------------------------------------------------------------

-- |Filter pattern events by MIDI note range
voiceRange :: (Int, Int) -> Pattern Int -> Pattern Int
voiceRange (lo, hi) = filterValues (\v -> v >= lo && v <= hi)

-- |Apply progression to pattern with toScale and temporal stretching.
-- 
-- The key operation: for each chord in the progression, the input pattern
-- is mapped through that chord's scale (toScale). The patterns are then
-- concatenated with `cat` and slowed to span the desired cycle length.
applyProg :: VoiceFunction -> P.Progression -> Pattern Time -> Pattern Int -> Pattern ValueMap
applyProg voiceFunc prog len pat =
  slow (4 * len) (cat $ note <$>
    (`toScale` fast (4 * len) pat) <$>
    fmap fromIntegral <$> voiceFunc prog)

-- |Main arrange function combining voice extraction and pattern application.
-- 
-- This preserves the launcher paradigm:
--   juno f s r d = p "juno" $ f $ arrange A.flow s r (-9,9) ["pattern"]
--
-- The progression is applied with modulo wrap internally, so indices
-- exceeding the progression length wrap around.
arrange :: VoiceFunction 
        -> P.Progression 
        -> Pattern Time      -- ^ Repetitions/cycle length
        -> (Int, Int)        -- ^ MIDI note range filter
        -> [Pattern Int]     -- ^ Input patterns to harmonize
        -> Pattern ValueMap
arrange voiceFunc prog rep register pats =
  let stacked = stack pats
      ranged = voiceRange register stacked
   in applyProg voiceFunc prog rep ranged

-------------------------------------------------------------------------------
-- Pattern-Based Lookup (New Phase C Interface)
-------------------------------------------------------------------------------

-- |Voice type for extraction strategy
data VoiceType = Roots | Bass | Harmony | Voiced
  deriving (Show, Eq)

-- |Lookup a chord from a progression by index with modulo wrap.
-- 
-- This is the fundamental operation enabling pattern-based harmonic access.
-- The index wraps around the progression length:
--   lookupChord prog 0  -> first chord
--   lookupChord prog 16 -> chord at (16 mod len)
lookupChord :: P.Progression -> Int -> H.Chord
lookupChord prog idx =
  let len = P.progLength prog
      chords = P.progChords prog
      wrappedIdx = idx `mod` len
  in chords !! wrappedIdx

-- |Lookup progression as a pattern of indices.
-- 
-- Maps a pattern of integers to a pattern of pitch class lists.
-- Uses modulo wrap so `run 4` on a 16-chord progression loops first 4.
--
-- Example:
--   lookupProgression prog "<0 [1 2] 3>" 
--   -> Pattern of pitch class lists following that rhythm
lookupProgression :: P.Progression -> Pattern Int -> Pattern [Int]
lookupProgression prog idxPat =
  let len = P.progLength prog
      voicings = A.flow prog  -- Use Arranger's flow (voice-led)
  in fmap (\idx -> voicings !! (idx `mod` len)) idxPat

-- |Extract specific voice type from a progression pattern.
-- 
-- Takes a pattern of indices and extracts the appropriate voice:
--   Roots: Root note only
--   Bass: Lowest note of voicing
--   Harmony: All notes (lite - no voice leading)
--   Voiced: All notes with voice leading applied (flow paradigm)
voiceBy :: VoiceType -> P.Progression -> Pattern Int -> Pattern [Int]
voiceBy vtype prog idxPat =
  let voiceFunc = case vtype of
        Roots   -> rootNotes
        Bass    -> bassNotes
        Harmony -> A.lite    -- Raw intervals
        Voiced  -> A.flow    -- Voice-led
      voicings = voiceFunc prog
      len = length voicings
  in fmap (\idx -> voicings !! (idx `mod` len)) idxPat

-- |Convenience function: lookup progression and convert to note pattern.
-- 
-- This combines lookupProgression with note conversion for immediate use:
--   d1 $ note (harmony prog "<0 1 2 3>") # s "superpiano"
harmony :: P.Progression -> Pattern Int -> Pattern ValueMap
harmony prog idxPat =
  let voicings = lookupProgression prog idxPat
  in note $ fmap (fromIntegral . head) voicings  -- Take first note for simplicity

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- |Overlap function for pattern phrasing.
-- 
-- scl=0: Full overlap (sustain)
-- scl=1: No overlap (staccato)
overlapF :: Double -> Pattern Double -> Pattern Double
overlapF scl pat = pat * pure (1 - scl)

