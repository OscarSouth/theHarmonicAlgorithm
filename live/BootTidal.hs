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
-- Warning classes that are idiom in a live session, not defects: rebinding a
-- name (start, k, s) is the central gesture, numeric literals default
-- constantly, and a bare `d1 $ ...` is a discarded do-bind by design. The
-- library itself still compiles under bare -Wall and stays clean; these apply
-- only to this boot file and to code evaluated at the prompt. Errors are
-- unaffected.
:set -Wno-name-shadowing
:set -Wno-type-defaults
:set -Wno-unused-do-bind
:set -Wno-unused-local-binds
:set -Wno-unused-matches
:set -Wno-x-partial
import Sound.Tidal.Context hiding (defaultConfig)
import qualified Sound.Tidal.Config as TidalConfig
import Harmonic.Lib

import System.IO (hSetEncoding, stdout, utf8)
hSetEncoding stdout utf8

-------------------------------------------------------------------------------
-- Stream Configuration (V3 with Link + Editor)
-------------------------------------------------------------------------------

let editorTarget = Target {oName = "editor", oAddress = "127.0.0.1", oPort = 6013, oLatency = 0.02, oSchedule = Pre BundleStamp, oWindow = Nothing, oHandshake = False, oBusPort = Nothing }
let editorShape = OSCContext "/editor/highlights"

tidal <- startStream (TidalConfig.defaultConfig {cFrameTimespan = 1/30, cEnableLink = False}) [(superdirtTarget {oLatency = 0.15}, [superdirtShape]), (editorTarget, [editorShape])]

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
    unmuteAll = streamUnmuteAll tidal
    unsoloAll = streamUnsoloAll tidal
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
launch = mapM_ ($ silence) [
  -- d1,d2,d3,d4,d5,d6,d7,d8,d9,
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
  p "grooveKit",
  p "moogDFAM",
  p "moogMother32",
  p "sh101",
  p "juno",
  p "drumbruteImpact",
  p "subKick",
  p "lineHarmony",
  p "displayClock",
  p "click",
  p "count",
  p "rise",
  p "wind",
  p "brss",
  p "strg",
  p "perc",
  p "chalumeau",
  p "pastorale",
  p "brillante",
  p "maestoso",
  p "tutti",
  p "rolandS1",
  p "p6Sample",
  p "p6Kybd",
  p "p6Gran"
  ]
:}

-------------------------------------------------------------------------------
-- MIDI Helpers
-------------------------------------------------------------------------------

midi = s "thru"
ccScale = (*127)
cc n val = control (ccScale val) #io n where io n = (midicmd "control" #ctlNum n)
cc' c n val = control (ccScale val) #io n c where io n c = (midi #midicmd "control" #midichan (c-1) #ctlNum (n))
setCC c n val = once $ control (val) #io n c where io n c = (midi #midicmd "control" #midichan (c-1) #ctlNum (n))
ped = cc 64

-------------------------------------------------------------------------------
-- Continuo voice (ch 7) — the plucked/keyboard colour
--
-- The orchestra is a fixed, deliberate configuration. The one permitted
-- variation is the ch-7 continuo voice, which may be sounded as any pitched
-- polyphonic plucked / keyboard / mallet / choral colour.
--
-- Set ONCE per movement, in the launcher's mapM_ list — never hot-swapped
-- mid-flow. `setContinuo` is IO (), so it sits alongside `hush` and `setbpm`:
--
--   mapM_ id [hush, setbpm tempo
--     , setContinuo harpsichordV        -- Baroque continuo
--     , strg f k  $ d 0.8
--     ]
--
-- The JV-1010 selects a patch with Bank Select (CC0 msb / CC32 lsb) followed
-- by a Program Change, so a continuo voice carries its full bank address. Quote
-- Orchestral-card numbers straight off Roland's 001-255 listing via `orch`;
-- reach the internal sets with `presetA` / `presetB`.
-------------------------------------------------------------------------------

-- Bank select (msb, lsb) + program change on one channel; fires once.
:{
progSel :: Int -> Int -> Int -> Int -> IO ()
progSel c msb lsb pc = mapM_ id
  [ setCC (fromIntegral c) 0  (fromIntegral msb)
  , setCC (fromIntegral c) 32 (fromIntegral lsb)
  , once $ midicmd "program" #progNum (fromIntegral pc) #ch (fromIntegral c)
  ]
:}

-- Bank addressing and the named continuo voices (`orch`, `presetA`/`presetB`,
-- harpV, harpsichordV, organV ...) live in
-- Harmonic.Interface.Tidal.Devices.JV1010.
:{
setContinuo :: ContinuoVoice -> IO ()
setContinuo (msb, lsb, pc) = progSel 7 msb lsb pc
:}

allNotesOff = setCC "[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]" "[123,64]" 0
subPedalOff = setCC "10" "64" 0
launch' = launch >> subPedalOff
hush = launch >> subPedalOff >> allNotesOff
launch'' = hush
hush' = streamHush tidal >> subPedalOff
panic = allNotesOff >> hush'
hush'' = panic

-------------------------------------------------------------------------------
-- Pattern Helpers
-------------------------------------------------------------------------------

lfo wave lo hi = segment 16 $ range lo hi wave
(|=) = (#)


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
runSeq = (0, 1, silence)
steptrig pat = mono $ midinote (toScale [-1, 0, 2, 4, 5, 7, 9, 10] $ (((pat-1) `mod` 8)+1)) |= vel 1 #ch 13

-------------------------------------------------------------------------------
-- Harmonic Algorithm Integration
--
-- Import Harmonic.Lib for access to:
--   * Progression type (Phase B)
--   * rotate, excerpt, insert, switch, clone, extract (Arranger)
--   * transposeP, reverse, fuse, expand (Arranger)
--   * progOverlap, progOverlapF, progOverlapB (Arranger)
--   * grid, flow, lite, literal, root (Voicing paradigms; grid/flow via cyclic DP)
--   * arrange, applyProg, voiceRange (Pattern application)
--   * overlapF (Pattern sustain/legato)
--   * PitchClass, NoteName, Chord, Cadence, CadenceState (Core types)
--   * initCadenceState, fromCadenceStates (Construction)
--   * generate, generateWith (Phase C database generation)
--   * HarmonicContext (R constraints); Layer selectors T/S/M + TS/TM/SM/TSM/PT
--
-- Voicing paradigms:
--   * grid: Cyclic DP, root always in bass, smooth compact voice leading
--   * flow: Cyclic DP, any inversion allowed for smoothest motion
--   * lite/literal: Literal intervals, no voice leading applied
--   * root: bass pitch class per bar (bass-line extraction; no DP)
--   * fund: harmonic fundamental per bar, inversion-invariant (sub/kick)
--   (>=6-PC bars auto-route to strataModeFlow's degree semantics)
--
-- All types are Phase B (MusicData has been deprecated).
-------------------------------------------------------------------------------

putStrLn "theHarmonicAlgorithm V3 boot complete."

-- LED display feed for the 12 Step. The CC arithmetic lives in
-- Harmonic.Interface.Tidal.Display; these name the stream that carries it.
-- Add a single line to your launcher's mapM_ list:  ,display k
--   display  — counter cells show elapsed SECONDS in the form loop
--   display' — counter cells show the current BAR NUMBER instead
display  k = p "displayClock" $ displayClock  k
display' k = p "displayClock" $ displayClock' k

-- The JV-1010 drum map (kick, snap, hhcl/hhop, ride, crash, snare, hh ...)
-- lives in Harmonic.Interface.Tidal.Devices.JV1010.

-------------------------------------------------------------------------------
-- Roland AIRA S-1 (ch 6) and P-6 (ch 3/4/5/16)
--
-- The full CC maps for both devices live in
-- Harmonic.Interface.Tidal.Devices.S1 and .P6. Configure the S-1 to MIDI
-- channel 6, and the P-6's Auto/S.CH/G.CH to 3/4/5 — the modules assume it.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Rhythm library and pattern helpers
--
-- The clave/cascara grids (`son32`, `rumba32`, `bossa32`, `bellpat32` and
-- their 2-3 rotations) live in Harmonic.Interface.Tidal.Groove; `swing8` /
-- `swing16`, `over` / `-->` and `binaryrange` live in
-- Harmonic.Interface.Tidal.Utils.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Motivic development
--
-- Motifs are plain patterns: a `Pattern Bool` rhythm, a `Pattern Int` contour
-- (voicing-index degrees — realised against the active harmony by `arrange`).
-- The classic developments are just Tidal:
--   retrograde  = rev        augmentation = slow n      transposition = |+ / |-
--   diminution  = fast n     rotation     = <~ / ~>     combination   = struct
-- The operators that fill the gaps — `>:<`, `mirror`, `retro`, `retroN` —
-- live in Harmonic.Interface.Tidal.Motif.
-------------------------------------------------------------------------------

-- Preloaded motif slots (rhythm / contour / motif, with ' and '' tiers).
-- Each tuple binds all three in one statement (motif = rhythm >:< contour).
-- Arrangement files override the non-primed trio; unset tiers fall back here.
-- Default is a neutral root pulse (rhythm ungated, contour = root).
(rhythm  , contour  , motif  ) = ("1/4", "0/4", rhythm   >:< contour  ) :: (Pattern Bool, Pattern Int, Pattern Int)
(rhythm' , contour' , motif' ) = ("1/4", "0/4", rhythm'  >:< contour' ) :: (Pattern Bool, Pattern Int, Pattern Int)
(rhythm'', contour'', motif'') = ("1/4", "0/4", rhythm'' >:< contour'') :: (Pattern Bool, Pattern Int, Pattern Int)

-------------------------------------------------------------------------------
-- Performance Utilities (count, metronome)
-------------------------------------------------------------------------------

metronome ks d = p "click" $ click (slow 4 $ fast ks "1") |= vel "[1 0.2@126]/4" |* vel d

count k d = p "count" $ midinote (fromIntegral . (+7) <$> snd k) # ch 10 # sustain 0.05 |* vel d

-------------------------------------------------------------------------------
-- Q-Link Controller Bridge (CC 100-110 via qlink-bridge.scd → OSC port 6010)
-------------------------------------------------------------------------------

qlink1 = cF 0 "100"
qlink2 = cF 0 "101"
qlink3 = cF 0 "102"
qlink4 = cF 0 "103"

tgl1 = cF 0 "104"
tgl2 = cF 0 "105"
tgl3 = cF 0 "106"
tgl4 = cF 0 "107"
tgl5 = cF 0 "108"

xyX = cF 0 "109"
xyY = cF 0 "110"

exP = cF 0 "111"
exp = exP

:{
let setI = streamSetI tidal
    setF = streamSetF tidal
    setS = streamSetS tidal
    setB = streamSetB tidal
:}

(o1,o2,o3) = (o,o,o) where o = slow 16 $ lfo tri 0 1
(q1,q2,q3,p4) = (qlink1, qlink2, qlink3, qlink4)
(t1,t2,t3,t4,t5) = (tgl1, tgl2, tgl3, tgl4, tgl5)

-------------------------------------------------------------------------------
-- End BootTidal.hs
-------------------------------------------------------------------------------
