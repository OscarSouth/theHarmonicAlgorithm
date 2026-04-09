{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Harmonic.Interface.Tidal.Orchestra
-- Description : Orchestral instrument functions for TidalCycles
--
-- Voice line system (SATB + octave variants) and instrument functions
-- with built-in MIDI range clipping and octave handling.
-- Each instrument is a thin 'arrange' wrapper — clip and octave shift
-- are internal; the composer sees only voice assignment and kinetics range.

module Harmonic.Interface.Tidal.Orchestra (
    -- Voice lines
    Voice(..), VoiceLines(..), voiceLines, vlGet, voiceOct,
    -- Pitched instruments
    flute, oboe, clarinet, bassoon,
    horn, trombone, basstrom,
    harp, timpani,
    violin1, violin2, viola, cello, contrabass,
    -- Unpitched percussion
    bassdrum, tamtam,
    -- Articulations
    pizz, spicc, marc, legg, arco,
    -- Internal (for testing only)
    clip,
) where

import Sound.Tidal.Context hiding (voice, clip)
import qualified Data.Map.Strict as Map
import Harmonic.Interface.Tidal.Bridge (VoiceFunction, arrange, overlapF)
import Harmonic.Interface.Tidal.Arranger (flow, root, grid)
import Harmonic.Interface.Tidal.Form (IK)
import Harmonic.Interface.Tidal.Instruments (ch, vel)
import Harmonic.Interface.Tidal.Utils (oct)

-------------------------------------------------------------------------------
-- Voice type (SATB + octave variants)
-------------------------------------------------------------------------------

data Voice
  = Soprano | Alto | Tenor | Bass                                      -- loco (normal register)
  | Soprano8va  | Alto8va  | Tenor8va  | Bass8va                      -- octave up
  | Soprano15va | Alto15va | Tenor15va | Bass15va                     -- two octaves up
  | Soprano8vb  | Alto8vb  | Tenor8vb  | Bass8vb                     -- octave down
  | Soprano15vb | Alto15vb | Tenor15vb | Bass15vb                    -- two octaves down
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- VoiceLines
-------------------------------------------------------------------------------

data VoiceLines = VoiceLines
  { _vl     :: Pattern Int     -- structural placeholder (always silence)
  , soprano :: Pattern Int     -- Soprano
  , alto    :: Pattern Int     -- Alto
  , tenor   :: Pattern Int     -- Tenor
  , bass    :: Pattern Int     -- Bass
  }

voiceLines :: VoiceLines
voiceLines = VoiceLines
  { _vl     = "~"
  , soprano = "3"       -- root 8va
  , alto    = "1"       -- 2nd degree
  , tenor   = "2"       -- 3rd degree
  , bass    = "0"       -- root
  }

-------------------------------------------------------------------------------
-- vlGet / voiceOct
-------------------------------------------------------------------------------

vlGet :: Voice -> VoiceLines -> Pattern Int
vlGet v = case v of
  Soprano -> soprano; Soprano8va -> soprano; Soprano15va -> soprano; Soprano8vb -> soprano; Soprano15vb -> soprano
  Alto -> alto; Alto8va -> alto; Alto15va -> alto; Alto8vb -> alto; Alto15vb -> alto
  Tenor -> tenor; Tenor8va -> tenor; Tenor15va -> tenor; Tenor8vb -> tenor; Tenor15vb -> tenor
  Bass -> bass; Bass8va -> bass; Bass15va -> bass; Bass8vb -> bass; Bass15vb -> bass

voiceOct :: Voice -> Int
voiceOct v = case v of
  Soprano -> 0; Alto -> 0; Tenor -> 0; Bass -> 0
  Soprano8va  -> 1;  Alto8va  -> 1;  Tenor8va  -> 1;  Bass8va  -> 1
  Soprano15va -> 2;  Alto15va -> 2;  Tenor15va -> 2;  Bass15va -> 2
  Soprano8vb  -> (-1); Alto8vb  -> (-1); Tenor8vb  -> (-1); Bass8vb  -> (-1)
  Soprano15vb -> (-2); Alto15vb -> (-2); Tenor15vb -> (-2); Bass15vb -> (-2)

-------------------------------------------------------------------------------
-- clip (MIDI range enforcement — internal)
-------------------------------------------------------------------------------

clip :: (Int, Int) -> ControlPattern -> ControlPattern
clip (lo, hi) = filterValues (\vm ->
    case Map.lookup "note" vm of
        Just (VF v) -> v >= fromIntegral lo && v <= fromIntegral hi
        Just (VN n) -> let v = unNote n in v >= fromIntegral lo && v <= fromIntegral hi
        _ -> True)

-------------------------------------------------------------------------------
-- instrument (internal helper)
-------------------------------------------------------------------------------

-- Pipeline: arrange → # ch → |+ oct → clip (outermost, filters AFTER octave shift)
instrument :: (Int, Int) -> Int -> (Double, Double) -> IK -> VoiceLines -> VoiceFunction -> Voice -> ControlPattern
instrument range channel ki k vl vf v =
    clip range $ arrange ki k (-9,9) vf (overlapF 0) [vlGet v vl] # ch channel |+ oct (voiceOct v)

-------------------------------------------------------------------------------
-- Pitched instruments (partial application of instrument)
-------------------------------------------------------------------------------

-- Winds (channels 1–4)
-- Ranges in Tidal note space (MIDI − 60); physical ranges noted in comments
flute, oboe, clarinet, bassoon :: (Double, Double) -> IK -> VoiceLines -> VoiceFunction -> Voice -> ControlPattern
flute      = instrument (-12, 26) 1   -- C3–D6  (MIDI 48–86)
oboe       = instrument ( -2, 33) 2   -- Bb3–A6 (MIDI 58–93)
clarinet   = instrument (-22, 34) 3   -- D2–Bb6 (MIDI 38–92)
bassoon    = instrument (-28, 15) 4   -- Bb1–Eb5 (MIDI 32–75)

-- Brass (channels 5–6)
horn, trombone, basstrom :: (Double, Double) -> IK -> VoiceLines -> VoiceFunction -> Voice -> ControlPattern
horn       = instrument (-29, 17) 5   -- B1–F5  (MIDI 31–77)
trombone   = instrument (-28, 17) 6   -- Bb1–F5 (MIDI 32–77)
basstrom   = instrument (-39, -5) 6   -- A0–G3  (MIDI 21–55)

-- Harp (channel 7)
harp :: (Double, Double) -> IK -> VoiceLines -> VoiceFunction -> Voice -> ControlPattern
harp       = instrument (-29, 42) 7   -- B1–F#7 (MIDI 31–102)

-- Pitched percussion (channel 8)
timpani :: (Double, Double) -> IK -> VoiceLines -> VoiceFunction -> Voice -> ControlPattern
timpani    = instrument (-22,  0) 8   -- D2–C4  (MIDI 38–60)

-- Strings (default arco = channel 16)
violin1, violin2, viola, cello, contrabass :: (Double, Double) -> IK -> VoiceLines -> VoiceFunction -> Voice -> ControlPattern
violin1    = instrument ( -5, 45) 16  -- G3–A7  (MIDI 55–105)
violin2    = instrument ( -5, 45) 16  -- G3–A7  (MIDI 55–105)
viola      = instrument (-12, 28) 16  -- C3–E6  (MIDI 48–88)
cello      = instrument (-24, 24) 16  -- C2–C5  (MIDI 36–84)
contrabass = instrument (-36,  0) 16  -- C1–C4  (MIDI 24–60)

-------------------------------------------------------------------------------
-- Unpitched percussion (struct-based)
-------------------------------------------------------------------------------

bassdrum :: Pattern Bool -> ControlPattern
bassdrum pat = struct pat $ midinote 36 # ch 9 # sustain 0.05

tamtam :: Pattern Bool -> ControlPattern
tamtam pat = struct pat $ midinote 31 # ch 11 # sustain 0.5

-------------------------------------------------------------------------------
-- String articulations (channel aliases)
-------------------------------------------------------------------------------

pizz, spicc, marc, legg, arco :: ControlPattern
pizz  = ch 12    -- pizzicato
spicc = ch 13    -- spiccato
marc  = ch 14    -- marcato
legg  = ch 15    -- legato
arco  = ch 16    -- arco (same as default)
