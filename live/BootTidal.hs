-- |
-- Minimal TidalCycles V3 BootTidal.hs for Harmonic Algorithm
--
-- This boot file provides:
--   1. Stream setup (d01-d16 MIDI, d1-d9 SuperDirt)
--   2. MIDI helpers (ch, cc, vel, ped)
--   3. Transport/timing (sync, bpm, setbpm)
--   4. Pattern utilities (oct, humanise, pullBy, pushBy)
--   5. Harmonic.Lib integration for Phase B/C/D types
--
-- NOT included (define per-performance in .tidal files):
--   - Scales (define in state.tidal)
--   - Instrument launchers pXX (define per-track)
--   - Legacy MusicData imports

:set -XOverloadedStrings
:set -XFlexibleContexts
:set prompt ""
:set prompt-cont ""
:set -Wno-operator-whitespace-ext-conflict
import Sound.Tidal.Context hiding (defaultConfig)
import qualified Sound.Tidal.Config as TidalConfig

import System.IO (hSetEncoding, stdout, utf8)
hSetEncoding stdout utf8

-------------------------------------------------------------------------------
-- Stream Configuration (V3 with Link + Editor)
-------------------------------------------------------------------------------

let editorTarget = Target {oName = "editor", oAddress = "127.0.0.1", oPort = 6013, oLatency = 0.02, oSchedule = Pre BundleStamp, oWindow = Nothing, oHandshake = False, oBusPort = Nothing }
let editorShape = OSCContext "/editor/highlights"

tidal <- startStream (TidalConfig.defaultConfig {cFrameTimespan = 1/50, cEnableLink = False}) [(superdirtTarget {oLatency = 0.15}, [superdirtShape]), (editorTarget, [editorShape])]

-------------------------------------------------------------------------------
-- Core Stream Definitions
-------------------------------------------------------------------------------

:{
let p = streamReplace tidal
    hush' = streamHush tidal
    list = streamList tidal
    mute = streamMute tidal
    unmute = streamUnmute tidal
    solo = streamSolo tidal
    unsolo = streamUnsolo tidal
    once = streamOnce tidal
    asap = once
    nudgeAll = streamNudgeAll tidal
    all = streamAll tidal
    resetCycles = streamResetCycles tidal
    setcps = asap . cps
    -- Transitions
    xfade i = transition tidal True (Sound.Tidal.Transition.xfadeIn 4) i
    xfadeIn i t = transition tidal True (Sound.Tidal.Transition.xfadeIn t) i
    histpan i t = transition tidal True (Sound.Tidal.Transition.histpan t) i
    wait i t = transition tidal True (Sound.Tidal.Transition.wait t) i
    waitT i f t = transition tidal True (Sound.Tidal.Transition.waitT f t) i
    jump i = transition tidal True (Sound.Tidal.Transition.jump) i
    jumpIn i t = transition tidal True (Sound.Tidal.Transition.jumpIn t) i
    jumpIn' i t = transition tidal True (Sound.Tidal.Transition.jumpIn' t) i
    jumpMod i t = transition tidal True (Sound.Tidal.Transition.jumpMod t) i
    mortal i lifespan release = transition tidal True (Sound.Tidal.Transition.mortal lifespan release) i
    interpolate i = transition tidal True (Sound.Tidal.Transition.interpolate) i
    interpolateIn i t = transition tidal True (Sound.Tidal.Transition.interpolateIn t) i
    clutch i = transition tidal True (Sound.Tidal.Transition.clutch) i
    clutchIn i t = transition tidal True (Sound.Tidal.Transition.clutchIn t) i
    anticipate i = transition tidal True (Sound.Tidal.Transition.anticipate) i
    anticipateIn i t = transition tidal True (Sound.Tidal.Transition.anticipateIn t) i
    forId i t = transition tidal False (Sound.Tidal.Transition.mortalOverlay t) i
    -- SuperDirt streams: d1-d9 on orbits 101-109
    d1 = p 101
    d2 = p 102
    d3 = p 103
    d4 = p 104
    d5 = p 105
    d6 = p 106
    d7 = p 107
    d8 = p 108
    d9 = p 109
    -- MIDI streams: d01-d16 on channels 1-16
    d01 = p 1
    d02 = p 2
    d03 = p 3
    d04 = p 4
    d05 = p 5
    d06 = p 6
    d07 = p 7
    d08 = p 8
    d09 = p 9
    d10 = p 10
    d11 = p 11
    d12 = p 12
    d13 = p 13
    d14 = p 14
    d15 = p 15
    d16 = p 16
:}

-------------------------------------------------------------------------------
-- Hush (customise per-performance in state.tidal)
-------------------------------------------------------------------------------

:{
hush = mapM_ ($ silence) [
  d1,d2,d3,d4,d5,d6,d7,d8,d9,
  d01,d02,d03,d04,d05,d06,d07,d08,d09,d10,d11,d12,d13,d14,d15,d16
  ,
  p "sinewave",
  p "piano",
  p "boeingdrone",
  p "boeingimpact",
  p "bassovertones",
  p "tubeblip",
  p "909kit",
  p "mpckit",
  p "moogDFAM",
  p "moogMother32",
  p "sh101",
  p "juno"
  ]
:}

-------------------------------------------------------------------------------
-- MIDI Helpers
-------------------------------------------------------------------------------

midi = s "thru"
ch n = (midi #midichan (n-1))
ccScale = (*127)
cc n val = control (ccScale val) #io n where io n = (midicmd "control" #ctlNum n)
cc' c n val = control (ccScale val) #io n c where io n c = (midi #midicmd "control" #midichan (c-1) #ctlNum (n))
setCC c n val = once $ control (val) #io n c where io n c = (midi #midicmd "control" #midichan (c-1) #ctlNum (n))
ped = cc 64
vel = pF "amp"

-------------------------------------------------------------------------------
-- Pattern Helpers
-------------------------------------------------------------------------------

lfo wave lo hi = segment 64 $ range lo hi wave
pullBy = (<~)
pushBy = (~>)
(|=) = (#)
oct n = note (12*n)
humanise n = vel $ (range (-0.09 * n) (0.09 * n) $ rand)

-------------------------------------------------------------------------------
-- Transport / Timing
-------------------------------------------------------------------------------

bar b1 b2 p = ((b1+2)*4, (b2+3)*4, p)
phrase = bar
midiClock out = bar 0 out $ midicmd "midiClock*24" #midi
initSync = bar 0 0 $ midicmd "stop" #midi
startSync = bar 6 6 $ midicmd "start" #midi
stopSync out = bar (out+1) (out+1) $ midicmd "stop" #midi
sync out = [midiClock out, initSync, startSync, stopSync out]
bpm t = cps (t/60)
setbpm tempo = p "t" $ bpm tempo
resetCycles = streamResetCycles tidal

-------------------------------------------------------------------------------
-- Musical Subdivisions
-------------------------------------------------------------------------------

hemidemisemiquaver = 1/64
demisemiquaver = 1/32
semiquaver = 1/16
quaver = 1/8
crotchet = 1/4
minim = 1/2

-------------------------------------------------------------------------------
-- Harmonic Algorithm Integration
--
-- Import Harmonic.Lib for access to:
--   * Progression type (Phase B)
--   * rotate, excerpt, insert, switch, clone, extract (Arranger)
--   * transposeP, reverse, fuse, expand (Arranger)
--   * progOverlap, progOverlapF, progOverlapB (Arranger)
--   * root, flow, lite (Voicing paradigms - 3 strategies via cyclic DP)
--   * arrange, applyProg, voiceRange (Pattern application)
--   * overlapF (Pattern sustain/legato)
--   * PitchClass, NoteName, Chord, Cadence, CadenceState (Core types)
--   * initCadenceState, fromCadenceStates (Construction)
--   * generate, generateWith (Phase C database generation)
--   * HarmonicContext, GeneratorConfig (Context/config)
--
-- Voicing paradigms:
--   * root: Cyclic DP, root always in bass, smooth compact voice leading
--   * flow: Cyclic DP, any inversion allowed for smoothest motion
--   * lite: Literal intervals, no voice leading applied
--
-- All types are Phase B (MusicData has been deprecated).
-------------------------------------------------------------------------------

import Harmonic.Lib

putStrLn "theHarmonicAlgorithm V3 boot complete."

-- additions

kick pat = struct pat $ midinote "0" #ch 10 #sustain 0.05
snap pat = struct pat $ midinote "1" #ch 10 #sustain 0.05
hhcl pat = struct pat $ midinote "2" #ch 10 #sustain 0.05
hhop pat = struct pat $ midinote "3" #ch 10 #sustain 0.05
ride pat = struct pat $ midinote "4" #ch 10 #sustain 0.05
crash pat = struct pat $ midinote "5" #ch 10 #sustain 0.05
click pat = struct pat $ midinote "6" #ch 10 #sustain 0.05
snare pat = struct pat $ midinote "7" #ch 10 #sustain 0.05
rimshot pat = struct pat $ midinote "[6,7]" #ch 10 #sustain 0.05

:{
hh pat = do
  let bars = 1
      ps = [
            ("x", midinote 2 #ch 10 #sustain 0.05 #vel 0.5),
            ("1", midinote 2 #ch 10 #sustain 0.05 #vel 0.5),
            ("o", midinote 3 #ch 10 #sustain 0.05 #vel 0.5),
            ("2", midinote 3 #ch 10 #sustain 0.05 #vel 0.5)
            ]
      fs   = []
   in ur bars pat ps fs
:}

-------------------------------------------------------------------------------
-- End BootTidal.hs
-------------------------------------------------------------------------------
