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
-- | 3  | clarinet                | 71 \/ 2         | 1stClarnet |
-- | 4  | bassoon                 | 70 \/ 2         | SoloBasoon |
-- | 5  | horn                    | 60 \/ 1         | FlugelSolo |
-- | 6  | the Mother-32 (blank)   | 127 \/ 1        | blank      |
-- | 7  | harp (continuo)         | 46 \/ 3         | Harpitz    |
-- | 8  | timpani (with tuba)     | 60 \/ 4         | Documentar |
-- | 9  | orch hit, bass drum     | 55 \/ 3         | Danz Hitz  |
-- | 10 | kit, shared with MPC    | 118 \/ 3        | 15ips Kit  |
-- | 11 | trombone, bass trombone | 57 \/ 1         | Bone Tone  |
-- | 12 | strings pizzicato       | 45 \/ 3         | Pizz Pluck |
-- | 13 | violin 1                | 49 \/ 0         | Slo String |
-- | 14 | violin 2                | 49 \/ 0         | Slo String |
-- | 15 | cello                   | 43 \/ 3         | Celli      |
-- | 16 | double bass, viola      | 41 \/ 1         | BiViola    |
-- +----+-------------------------+----------------+------------+
--
-- The order is the JV-1010's wherever the module can follow it: winds 1–5,
-- harp 7, timpani 8, bass drum 9, the MPC lane 10, pizzicato 12 and the
-- strings 13–16, so @# pizz@ is channel 12 on both rigs and the continuo is 7
-- on both. Channel 6 belongs to the Moog Mother-32, which shares this chain
-- through the MPC's CV out, so the module is blank there and the trombone
-- takes 11 (the JV's tam-tam channel, a voice the module lacks).
--
-- A program is reached by Bank Select on CC0 alone — 0 General MIDI, 1 User,
-- 2 to 4 the Preset banks; the module ignores CC32 — followed by a Program
-- Change 0-127. A bank select on its own takes effect at once, keeping the
-- program number. Ranges are the instruments\' own
-- ("Harmonic.Interface.Tidal.Orchestra"); the zones the 2021 patches were
-- found to sound in are tabled in @documents\/NANOSYNTH.md@.
--
-- Channel 10 carries the orchestra's percussion (15ips Kit) on the channel the
-- MPC's service lane also uses; the two share it by note band — the MPC's pads
-- keep 0–48, the kit voices the rig names ('rigTambourine' 54, 'rigWoodblock'
-- 76, 'rigTriangle' 81) sit above. @nanoKit@ re-sends the kit after a program
-- change. The kit answers to the General MIDI percussion map, so the voices
-- below carry the @gm@ prefix and serve any GM module; the ones in the MPC's
-- band (kick, snare, hats) are not part of the orchestra.
module Harmonic.Interface.Tidal.Devices.NanoSynth
  ( -- * Orchestral rig
    nanoSynth

    -- * Configuration
  , nanoMix, nanoPrograms, nanoLevel, nanoPan, nanoDepth, nanoMod, nanoExpr
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

-- | The NanoSynth orchestra: one section per channel, pizzicato shared on 12,
-- continuo on the harp channel. The tam-tam has no voice here.
nanoSynth :: Rig
nanoSynth = Rig
  { rigName       = "NanoSynth"
  , rigFlute      = 1
  , rigOboe       = 2
  , rigClarinet   = 3
  , rigBassoon    = 4
  , rigHorn       = 5
  , rigTrombone   = 11
  , rigBasstrom   = 11
  , rigHarp       = 7
  , rigTimpani    = 8
  , rigViolin1    = 13
  , rigViolin2    = 14
  , rigViola      = 16
  , rigCello      = 15
  , rigContrabass = 16
  , rigBassdrum   = Just (12, 9)    -- the orchestra hit, four octaves down
  , rigTamtam     = Nothing
  , rigTambourine = Just (54, 10)   -- 15ips Kit, General MIDI numbers, all above the MPC's band (0–48)
  , rigTriangle   = Just (81, 10)
  , rigWoodblock  = Just (76, 10)
  , rigPizz       = ch 12
  , rigSpicc      = noArt
  , rigMarc       = noArt
  , rigLegg       = noArt
  , rigArco       = noArt
  , rigContinuo   = 7
  , rigSetup      = nanoSetup
  }

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

-- | The mix, one row per channel: @(channel, (program, bank), level, pan,
-- depth, modulation, expression)@ — level, depth, modulation and expression
-- 0–1, pan &#8722;1 left to 1 right. The values are the 2021 concert's and
-- travel with their program; the channel column is the layout. Firsts far
-- left, seconds far right, the winds and brass spread between; the wheel open
-- on the orchestra hit alone; the Mother-32's channel closed.
nanoMix :: [(Int, (Int, Int), Double, Double, Double, Double, Double)]
nanoMix =
  [ ( 1, ( 74, 3), 0.36, -0.20, 0.85, 0, 1)   -- SingleFlut: flute
  , ( 2, ( 68, 3), 0.32,  0.20, 0.90, 0, 1)   -- Ebony Oboe: oboe
  , ( 3, ( 71, 2), 0.32, -0.25, 0.85, 0, 1)   -- 1stClarnet: clarinet
  , ( 4, ( 70, 2), 0.40,  0.25, 0.90, 0, 1)   -- SoloBasoon: bassoon
  , ( 5, ( 60, 1), 0.60, -0.40, 0.95, 0, 1)   -- FlugelSolo: horn
  , ( 6, (127, 1), 0,     0,    0,    0, 0)   -- blank: the Mother-32's channel
  , ( 7, ( 46, 3), 0.10, -0.20, 0.95, 0, 1)   -- Harpitz: harp, the continuo
  , ( 8, ( 60, 4), 0.90, -0.30, 0.85, 0, 1)   -- Documentar: timpani, with tuba
  , ( 9, ( 55, 3), 1.00,  0.00, 0.90, 1, 1)   -- Danz Hitz: orchestra hit, the bass drum at MIDI 12
  , (10, (118, 3), 0.70, -0.20, 0.85, 0, 1)   -- 15ips Kit: tambourine 54, woodblock 76, triangle 81; shared with the MPC (its pads keep 0–48)
  , (11, ( 57, 1), 0.90,  0.10, 0.80, 0, 1)   -- Bone Tone: trombone, bass trombone
  , (12, ( 45, 3), 0.50,  0.15, 0.90, 0, 1)   -- Pizz Pluck: strings pizzicato
  , (13, ( 49, 0), 0.40, -0.80, 0.80, 0, 1)   -- Slo String: violin 1
  , (14, ( 49, 0), 0.30,  0.80, 0.95, 0, 1)   -- Slo String: violin 2
  , (15, ( 43, 3), 0.50,  0.30, 0.85, 0, 1)   -- Celli: cello
  , (16, ( 41, 1), 0.20, -0.30, 0.95, 0, 1)   -- BiViola: double bass, with viola
  ]

-- | Channel to @(program, bank)@, from 'nanoMix'.
nanoPrograms :: [(Int, (Int, Int))]
nanoPrograms = [ (c, pb) | (c, pb, _, _, _, _, _) <- nanoMix ]

-- | Channel volume (CC7), pan (CC10, &#8722;1 to 1), reverb send (CC91),
-- modulation (CC1) and expression (CC11), each a column of 'nanoMix'.
nanoLevel, nanoPan, nanoDepth, nanoMod, nanoExpr :: [(Int, Double)]
nanoLevel = [ (c, v) | (c, _, v, _, _, _, _) <- nanoMix ]
nanoPan   = [ (c, v) | (c, _, _, v, _, _, _) <- nanoMix ]
nanoDepth = [ (c, v) | (c, _, _, _, v, _, _) <- nanoMix ]
nanoMod   = [ (c, v) | (c, _, _, _, _, v, _) <- nanoMix ]
nanoExpr  = [ (c, v) | (c, _, _, _, _, _, v) <- nanoMix ]

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

-- | The kit on channel 10 — 15ips Kit, 118 \/ Preset 3 — re-sent on its own
-- after a program change there. Fire with @once@; the level, pan and depth
-- rows for 10 are already set by 'nanoSetup'.
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
