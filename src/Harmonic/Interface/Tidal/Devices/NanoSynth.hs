-- |
-- Module      : Harmonic.Interface.Tidal.Devices.NanoSynth
-- Description : Alesis NanoSynth orchestral rig, General MIDI kit and continuo addressing
--
-- The NanoSynth is a 64-voice General MIDI module on the Alesis QS engine. It
-- answers on all sixteen channels at once, one program per channel, and a
-- channel is silenced by assigning User bank program 127, the module's
-- hard-wired blank program. The orchestra sits one section per channel, with
-- a single pizzicato channel shared by every string section, so the
-- articulation tags other than @pizz@ leave a line on its own channel.
--
-- +----+-------------------------+----------------+------------+
-- | ch | instrument              | program \/ bank | patch      |
-- +====+=========================+================+============+
-- | 1  | flute                   | 74 \/ 3         | SingleFlut |
-- | 2  | oboe                    | 68 \/ 3         | Ebony Oboe |
-- | 3  | bassoon                 | 70 \/ 2         | SoloBasoon |
-- | 4  | horn                    | 60 \/ 1         | FlugelSolo |
-- | 5  | trombone, bass trombone | 57 \/ 1         | Bone Tone  |
-- | 6  | timpani (with tuba)     | 60 \/ 4         | Documentar |
-- | 7  | strings pizzicato       | 45 \/ 3         | Pizz Pluck |
-- | 8  | orch hit, bass drum     | 55 \/ 3         | Danz Hitz  |
-- | 9  | harp (continuo)         | 46 \/ 3         | Harpitz    |
-- | 10 | service channel (MPC)   | 127 \/ 1        | blank      |
-- | 11 | clarinet                | 71 \/ 2         | 1stClarnet |
-- | 12 | silent                  | 127 \/ 1        | blank      |
-- | 13 | violin 1                | 49 \/ 0         | Slo String |
-- | 14 | violin 2                | 49 \/ 0         | Slo String |
-- | 15 | cello                   | 43 \/ 3         | Celli      |
-- | 16 | double bass, viola      | 41 \/ 1         | BiViola    |
-- +----+-------------------------+----------------+------------+
--
-- A program is reached by Bank Select on CC0 alone — 0 General MIDI, 1 User,
-- 2 to 4 the Preset banks; the module ignores CC32 — followed by a Program
-- Change 0-127. A bank select on its own takes effect at once, keeping the
-- program number. Ranges are the instruments\' own
-- ("Harmonic.Interface.Tidal.Orchestra"); the zones the 2021 patches were
-- found to sound in are tabled in @documents\/NANOSYNTH.md@.
--
-- Channel 10 is the MPC's service lane (sub, kit, click, count, display), so
-- the module is blank there and the kit is one message away: @once $ nanoKit@
-- puts 15ips Kit on 10 when the module plays alone. The kit answers to the
-- General MIDI percussion map, so the voices here carry the @gm@ prefix and
-- serve any GM module.
module Harmonic.Interface.Tidal.Devices.NanoSynth
  ( -- * Orchestral rig
    nanoSynth

    -- * Configuration
  , nanoPrograms, nanoLevel, nanoPan, nanoDepth, nanoMod, nanoExpr
  , nanoProgram, nanoTable, nanoSetup, nanoKit

    -- * Continuo
  , nanoVoice, nanoHarpV, nanoHarpsichordV

    -- * General MIDI kit (channel 10)
  , gmKick, gmKick2, gmSnare, gmSnare2, gmSidestick
  , gmHatClosed, gmHatPed, gmHatOpen
  , gmCrash, gmCrash2, gmRide, gmRide2, gmRideBell, gmChina, gmSplash
  , gmTamb, gmTriOpen, gmTriMute, gmChimes, gmClave, gmClave2
  ) where

import Sound.Tidal.Context
import Harmonic.Interface.Tidal.Instruments (ch)
import Harmonic.Interface.Tidal.Orchestra (Rig(..), noArt)
import Harmonic.Interface.Tidal.Devices.JV1010 (ContinuoVoice)

-- | The NanoSynth orchestra: one section per channel, pizzicato shared on 7,
-- continuo on the harp channel. The tam-tam has no voice here.
nanoSynth :: Rig
nanoSynth = Rig
  { rigName       = "NanoSynth"
  , rigFlute      = 1
  , rigOboe       = 2
  , rigClarinet   = 11
  , rigBassoon    = 3
  , rigHorn       = 4
  , rigTrombone   = 5
  , rigBasstrom   = 5
  , rigHarp       = 9
  , rigTimpani    = 6
  , rigViolin1    = 13
  , rigViolin2    = 14
  , rigViola      = 16
  , rigCello      = 15
  , rigContrabass = 16
  , rigBassdrum   = Just (12, 8)    -- the orchestra hit, four octaves down
  , rigTamtam     = Nothing
  , rigPizz       = ch 7
  , rigSpicc      = noArt
  , rigMarc       = noArt
  , rigLegg       = noArt
  , rigArco       = noArt
  , rigContinuo   = 9
  , rigSetup      = nanoSetup
  }

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

-- | Channel to @(program, bank)@: the 2021 concert map, one patch per channel.
nanoPrograms :: [(Int, (Int, Int))]
nanoPrograms =
  [ ( 1, ( 74, 3))   -- SingleFlut
  , ( 2, ( 68, 3))   -- Ebony Oboe
  , ( 3, ( 70, 2))   -- SoloBasoon
  , ( 4, ( 60, 1))   -- FlugelSolo
  , ( 5, ( 57, 1))   -- Bone Tone
  , ( 6, ( 60, 4))   -- Documentar (timpani with tuba)
  , ( 7, ( 45, 3))   -- Pizz Pluck
  , ( 8, ( 55, 3))   -- Danz Hitz (orchestra hit)
  , ( 9, ( 46, 3))   -- Harpitz
  , (10, (127, 1))   -- blank: the MPC owns channel 10; 'nanoKit' puts the kit here
  , (11, ( 71, 2))   -- 1stClarnet
  , (12, (127, 1))   -- blank: the module's silent program
  , (13, ( 49, 0))   -- Slo String
  , (14, ( 49, 0))   -- Slo String
  , (15, ( 43, 3))   -- Celli
  , (16, ( 41, 1))   -- BiViola (double bass with viola)
  ]

-- | Channel volume (CC7), 0-1: the section balance.
nanoLevel :: [(Int, Double)]
nanoLevel = zip [1 ..] [0.36, 0.32, 0.40, 0.60, 0.90, 0.90, 0.50, 1.00, 0.10, 0.70, 0.32, 0, 0.40, 0.30, 0.50, 0.20]

-- | Pan (CC10), &#8722;1 left to 1 right: firsts far left, seconds far right,
-- the winds and brass spread between.
nanoPan :: [(Int, Double)]
nanoPan = zip [1 ..] [-0.2, 0.2, 0.25, -0.4, 0.1, -0.3, 0.15, 0, -0.2, -0.2, -0.25, 0, -0.8, 0.8, 0.3, -0.3]

-- | Reverb send (CC91), 0-1: the hall depth per section.
nanoDepth :: [(Int, Double)]
nanoDepth = zip [1 ..] [0.85, 0.90, 0.90, 0.95, 0.80, 0.85, 0.90, 0.90, 0.95, 0.85, 0.85, 0, 0.80, 0.95, 0.85, 0.95]

-- | Modulation (CC1): off everywhere except the orchestra hit, which the
-- wheel opens.
nanoMod :: [(Int, Double)]
nanoMod = [ (c, if c == 8 then 1 else 0) | c <- [1 .. 16] ]

-- | Expression (CC11): full on every sounding channel, closed on the silent one.
nanoExpr :: [(Int, Double)]
nanoExpr = [ (c, if c == 12 then 0 else 1) | c <- [1 .. 16] ]

-- A control change by number, level 0-1 scaled to 0-127.
ctl :: Int -> Double -> ControlPattern
ctl num v = control (pure (v * 127)) # midicmd "control" # ctlNum (pure (fromIntegral num))

-- | Bank Select then Program Change on one channel, inside one cycle: the
-- bank on CC0 at the start, the program a quarter-cycle later. Fire with
-- @once@.
nanoProgram :: Int -> (Int, Int) -> ControlPattern
nanoProgram c (prog, bk) = stack
  [ control (pure (fromIntegral bk)) # midicmd "control" # ctlNum 0
  , (1/4) ~> (midicmd "program" # progNum (pure (fromIntegral prog)))
  ] # ch c

-- | One controller across the sixteen channels, each a sixteenth of a cycle
-- after the last so the module never queues. Fire with @once@.
nanoTable :: Int -> [(Int, Double)] -> ControlPattern
nanoTable num rows = stack [ (fromIntegral i / 16) ~> (ctl num v # ch c) | (i, (c, v)) <- zip [0 :: Int ..] rows ]

-- | The module's own kit on channel 10 — 15ips Kit, 118 \/ Preset 3 — for when
-- it plays alone and the MPC is off the chain. Fire with @once@; the level,
-- pan and depth rows for 10 are already set by 'nanoSetup'.
nanoKit :: ControlPattern
nanoKit = nanoProgram 10 (118, 3)

-- | Cold configuration in receive order: every channel's bank and program,
-- then level, pan, depth, modulation and expression. Pan is sent as 0-1.
nanoSetup :: [ControlPattern]
nanoSetup = [ nanoProgram c pb | (c, pb) <- nanoPrograms ]
         ++ [ nanoTable 7 nanoLevel
            , nanoTable 10 [ (c, (pn + 1) / 2) | (c, pn) <- nanoPan ]
            , nanoTable 91 nanoDepth
            , nanoTable 1 nanoMod
            , nanoTable 11 nanoExpr ]

-- | A NanoSynth patch as a continuo voice: the bank rides in the msb slot and
-- the lsb is unused, since the module reads CC0 only.
nanoVoice :: Int -> Int -> ContinuoVoice
nanoVoice prog bk = (bk, 0, prog)

-- | Harp 46 \/ Preset 3 and harpsichord 6 \/ User, the two continuo colours the
-- 2021 concert chose between.
nanoHarpV, nanoHarpsichordV :: ContinuoVoice
nanoHarpV        = nanoVoice 46 3   -- Harpitz
nanoHarpsichordV = nanoVoice 6 1    -- 8'4'Harpsi

-- | The kit answers on MIDI channel 10.
kitChan :: Pattern ValueMap
kitChan = ch 10

-- | One General MIDI percussion voice each, taking a boolean rhythm.
gmKick, gmKick2, gmSnare, gmSnare2, gmSidestick, gmHatClosed, gmHatPed, gmHatOpen,
  gmCrash, gmCrash2, gmRide, gmRide2, gmRideBell, gmChina, gmSplash,
  gmTamb, gmTriOpen, gmTriMute, gmChimes, gmClave, gmClave2
  :: Pattern Bool -> ControlPattern
gmKick      pat = struct pat $ midinote "36" # kitChan # sustain 0.05
gmKick2     pat = struct pat $ midinote "35" # kitChan # sustain 0.05
gmSnare     pat = struct pat $ midinote "40" # kitChan # sustain 0.05
gmSnare2    pat = struct pat $ midinote "38" # kitChan # sustain 0.05
gmSidestick pat = struct pat $ midinote "37" # kitChan # sustain 0.05
gmHatClosed pat = struct pat $ midinote "42" # kitChan # sustain 0.05
gmHatPed    pat = struct pat $ midinote "44" # kitChan # sustain 0.05
gmHatOpen   pat = struct pat $ midinote "46" # kitChan # sustain 0.05
gmCrash     pat = struct pat $ midinote "49" # kitChan # sustain 0.05
gmCrash2    pat = struct pat $ midinote "57" # kitChan # sustain 0.05
gmRide      pat = struct pat $ midinote "51" # kitChan # sustain 0.05
gmRide2     pat = struct pat $ midinote "59" # kitChan # sustain 0.05
gmRideBell  pat = struct pat $ midinote "53" # kitChan # sustain 0.05
gmChina     pat = struct pat $ midinote "52" # kitChan # sustain 0.05
gmSplash    pat = struct pat $ midinote "55" # kitChan # sustain 0.05
gmTamb      pat = struct pat $ midinote "54" # kitChan # sustain 0.05
gmTriOpen   pat = struct pat $ midinote "81" # kitChan # sustain 0.05
gmTriMute   pat = struct pat $ midinote "80" # kitChan # sustain 0.05
gmChimes    pat = struct pat $ midinote "84" # kitChan # sustain 0.05
gmClave     pat = struct pat $ midinote "75" # kitChan # sustain 0.05
gmClave2    pat = struct pat $ midinote "87" # kitChan # sustain 0.05
