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
import Harmonic.Interface.Tidal.Form (Kinetics(..))
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
instrument :: (Int, Int) -> Int -> (Double, Double) -> VoiceFunction -> Voice -> Pattern Int -> Kinetics -> VoiceLines -> ControlPattern
instrument range channel ki vf v r k vl =
    clip range $ arrange ki vf (overlapF 0) r k (-9,9) [vlGet v vl] # ch channel |+ oct (voiceOct v)

-------------------------------------------------------------------------------
-- Pitched instruments (partial application of instrument)
-------------------------------------------------------------------------------

-- Winds (channels 1–4)
flute, oboe, clarinet, bassoon :: (Double, Double) -> VoiceFunction -> Voice -> Pattern Int -> Kinetics -> VoiceLines -> ControlPattern
flute      = instrument (48, 86)  1
oboe       = instrument (46, 81)  2
clarinet   = instrument (26, 82)  3
bassoon    = instrument (22, 63)  4

-- Brass (channels 5–6)
horn, trombone, basstrom :: (Double, Double) -> VoiceFunction -> Voice -> Pattern Int -> Kinetics -> VoiceLines -> ControlPattern
horn       = instrument (23, 65)  5
trombone   = instrument (28, 65)  6
basstrom   = instrument (21, 55)  6

-- Harp (channel 7)
harp :: (Double, Double) -> VoiceFunction -> Voice -> Pattern Int -> Kinetics -> VoiceLines -> ControlPattern
harp       = instrument (23, 90)  7

-- Pitched percussion (channel 8)
timpani :: (Double, Double) -> VoiceFunction -> Voice -> Pattern Int -> Kinetics -> VoiceLines -> ControlPattern
timpani    = instrument (26, 48)  8

-- Strings (default arco = channel 16)
violin1, violin2, viola, cello, contrabass :: (Double, Double) -> VoiceFunction -> Voice -> Pattern Int -> Kinetics -> VoiceLines -> ControlPattern
violin1    = instrument (55, 105) 16
violin2    = instrument (55, 105) 16
viola      = instrument (48, 88)  16
cello      = instrument (36, 84)  16
contrabass = instrument (24, 60)  16

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
