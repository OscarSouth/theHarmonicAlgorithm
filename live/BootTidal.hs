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
ch n = (midi #midichan (n-1))
ccScale = (*127)
cc n val = control (ccScale val) #io n where io n = (midicmd "control" #ctlNum n)
cc' c n val = control (ccScale val) #io n c where io n c = (midi #midicmd "control" #midichan (c-1) #ctlNum (n))
setCC c n val = once $ control (val) #io n c where io n c = (midi #midicmd "control" #midichan (c-1) #ctlNum (n))
ped = cc 64
vel = pF "amp"

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
runSeq = (0, 1, silence)
steptrig pat = mono $ midinote (toScale [-1, 0, 2, 4, 5, 7, 9, 10] $ (((pat-1) `mod` 8)+1)) |= vel 1 #ch 13

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

-- 4-character LED display feed for the 12 Step.
-- Broadcasts the truth from Tidal-side; SC just paints what it receives.
--   * CC 113 = bar number (1..8) at each chord onset; 0 on bar-off (1/8 cycle later).
--   * CC 114 / CC 115 = 14-bit form loop length in seconds (high << 7 | low).
--     0 = atemporal (lK or single-node iK) -> counter cells stay blank.
--   * CC 117 / CC 118 = 14-bit *current form-local seconds*, sampled 16x per
--     cycle. Derived from the same cycle time that drives the form interpolation
--     (formContinuous's `slow totalCycles`), so the displayed value is always
--     in lockstep with the form's kinetics — no anchor signal needed.
-- SC owns CC 50-53 (the actual display cells); these CCs are just signals.
-- Add a single line to your launcher's mapM_ list:  ,display k
:{
display k =
  let loopSecs  = kLoopSecs (fst k)
      cps       = kCps (fst k)
      loopInt   = floor loopSecs :: Int
      hiByte    = fromIntegral (loopInt `div` 128) :: Double
      loByte    = fromIntegral (loopInt `mod` 128) :: Double
      thruCh10     = s "thru" # midichan 9             -- 1 event / cycle (constant CCs, struct-driven onsets)
      thruCh10Fast = fast 30 (s "thru") # midichan 9   -- 30 events / cycle (~56 Hz; for the seconds counter only)

      -- 1-indexed second counter, phase-locked to cycle 0. The displayed
      -- value is a continuous function of cycle time. Emission rate (~56 Hz
      -- at cps 1.867) comes from the *structural* side via thruCh10Fast on
      -- the CC 117/118 lines below — Tidal's `#` is `|>`, which reads
      -- structure from the left and only samples values from the right.
      -- Putting `segment 30` on this sig has no effect on emission rate;
      -- the leftmost pattern in the chain determines onsets.
      -- Values are 1..n inclusive (first second of the loop displays "1").
      -- Atemporal forms broadcast 0 continuously (SC treats this as blank).
      n = floor loopSecs :: Int
      currentSecsPat = if n >= 1 && cps > 0
        then sig $ \t ->
               let cyclesNow     = realToFrac t :: Double
                   secondsNow    = cyclesNow / cps
                   formLocalSecs = secondsNow - fromIntegral (floor (secondsNow / loopSecs) :: Int) * loopSecs
                   displayed     = (floor formLocalSecs :: Int) + 1
               in fromIntegral displayed :: Double
        else pure 0

      secsHiPat = fmap (\x -> fromIntegral (floor x `div` 128) :: Double) currentSecsPat
      secsLoPat = fmap (\x -> fromIntegral (floor x `mod` 128) :: Double) currentSecsPat

  in p "displayClock" $ stack
       [ -- Bar onset: CC 113 = current bar (clamped to 1..8)
         (1/64) ~> (struct (fmap (const True) (snd k)) $
           thruCh10 # midicmd "control" # ctlNum 113
             # control (fmap (fromIntegral . min 8) (snd k)))
         -- Bar off: CC 113 = 0 at +1/8 cycle (P1 blank between flashes)
       , (1/64) ~> (struct ((pure (1/8)) ~> fmap (const True) (snd k)) $
           thruCh10 # midicmd "control" # ctlNum 113 # control 0)
         -- Loop length, high byte
       , thruCh10 # midicmd "control" # ctlNum 114 # control (pure hiByte)
         -- Loop length, low byte
       , thruCh10 # midicmd "control" # ctlNum 115 # control (pure loByte)
         -- Current form-local seconds, high byte (30 events/cycle from thruCh10Fast; ≤18 ms jitter)
       , thruCh10Fast # midicmd "control" # ctlNum 117 # control secsHiPat
         -- Current form-local seconds, low byte (30 events/cycle from thruCh10Fast; ≤18 ms jitter)
       , thruCh10Fast # midicmd "control" # ctlNum 118 # control secsLoPat
       ]
:}

-- additions

kick pat = struct pat $ midinote "0" #ch 10 #sustain 0.05
kick' pat = struct pat $ midinote "60" #ch 10 #sustain 0.05
kick'' pat = struct pat $ midinote "70" #ch 10 #sustain 0.05
kick2 pat = struct pat $ midinote "70" #ch 10 #sustain 0.05
kick2' pat = struct pat $ midinote "80" #ch 10 #sustain 0.05
snap pat = struct pat $ midinote "1" #ch 10 #sustain 0.05
hhcl pat = struct pat $ midinote "2" #ch 10 #sustain 0.05
hhcl' pat = struct pat $ midinote "62" #ch 10 #sustain 0.05
hhop pat = struct pat $ midinote "3" #ch 10 #sustain 0.05
hhop' pat = struct pat $ midinote "63" #ch 10 #sustain 0.05
ride pat = struct pat $ midinote "4" #ch 10 #sustain 0.05
ride' pat = struct pat $ midinote "64" #ch 10 #sustain 0.05
crash pat = struct pat $ midinote "5" #ch 10 #sustain 0.05
click pat = struct pat $ midinote "6" #ch 10 #sustain 0.05
click' pat = struct pat $ midinote "66" #ch 10 #sustain 0.05
snare pat = struct pat $ midinote "7" #ch 10 #sustain 0.05
snare' pat = struct pat $ midinote "67" #ch 10 #sustain 0.05
cowbell pat = struct pat $ midinote "8" #ch 10 #sustain 0.05
fm pat = struct pat $ midinote "9" #ch 10 #sustain 0.05
fm' pat = struct pat $ midinote "69" #ch 10 #sustain 0.05
rimshot pat = struct pat $ midinote "[6,7]" #ch 10 #sustain 0.05
rimshot' pat = struct pat $ midinote "[66,67]" #ch 10 #sustain 0.05

:{
hh pat = stack [
  struct (fmap (`elem` ["x","1"]) pat) $ midinote 2 #ch 10 #sustain 0.05 #vel 0.5,
  struct (fmap (`elem` ["o","2"]) pat) $ midinote 3 #ch 10 #sustain 0.05 #vel 0.5
  ]
:}

:{
hh' pat = stack [
  struct (fmap (`elem` ["x","1"]) pat) $ midinote 62 #ch 10 #sustain 0.05 #vel 0.5,
  struct (fmap (`elem` ["o","2"]) pat) $ midinote 63 #ch 10 #sustain 0.05 #vel 0.5
  ]
:}

-------------------------------------------------------------------------------
-- Roland AIRA S-1 Tweak Synthesizer (ch 6)
-- Configure S-1 MIDI channel to 6 on device.
-------------------------------------------------------------------------------

-- Pattern selection
s1pat p        = midicmd "program" #progNum p #ch 6
s1seq pat bk   = s1pat ((pat-1)+((bk-1)*8))

-- Global & Performance
s1modwheel v   = cc 1   v #ch 6
s1portatime v  = cc 5   v #ch 6
s1pan v        = cc 10  v #ch 6
s1expression v = cc 11  v #ch 6
s1portamode v  = cc 31  v #ch 6
s1damper v     = cc 64  v #ch 6
s1portasw v    = cc 65  v #ch 6
s1finetune v   = cc 76  v #ch 6
s1transpose v  = cc 77  v #ch 6

-- Oscillator
s1osclfo v     = cc 13  v #ch 6
s1oscrange v   = cc 14  v #ch 6
s1oscpwm v     = cc 15  v #ch 6
s1oscpwmsrc v  = cc 16  v #ch 6
s1oscbend v    = cc 18  v #ch 6
s1square v     = cc 19  v #ch 6
s1saw v        = cc 20  v #ch 6
s1sub v        = cc 21  v #ch 6
s1suboct v     = cc 22  v #ch 6
s1noise v      = cc 23  v #ch 6
s1noisemode v  = cc 78  v #ch 6

-- Filter & Amp
s1cutoff v     = cc 74  v #ch 6
s1res v        = cc 71  v #ch 6
s1filterenv v  = cc 24  v #ch 6
s1filterlfo v  = cc 25  v #ch 6
s1keytrack v   = cc 26  v #ch 6
s1filterbend v = cc 27  v #ch 6
s1ampmode v    = cc 28  v #ch 6

-- LFO & Envelope
s1lforate v    = cc 3   v #ch 6
s1lfowave v    = cc 12  v #ch 6
s1lfomod v     = cc 17  v #ch 6
s1lfomode v    = cc 79  v #ch 6
s1lfokeytrg v  = cc 105 v #ch 6
s1lfosync v    = cc 106 v #ch 6
s1attack v     = cc 73  v #ch 6
s1decay v      = cc 75  v #ch 6
s1sustain v    = cc 30  v #ch 6
s1release v    = cc 72  v #ch 6
s1envtrig v    = cc 29  v #ch 6

-- Chord Mode
s1polymode v    = cc 80 v #ch 6
s1voice2sw v    = cc 81 v #ch 6
s1voice3sw v    = cc 82 v #ch 6
s1voice4sw v    = cc 83 v #ch 6
s1voice2shift v = cc 85 v #ch 6
s1voice3shift v = cc 86 v #ch 6
s1voice4shift v = cc 87 v #ch 6

-- Effects
s1revlvl v     = cc 91  v #ch 6
s1revtime v    = cc 89  v #ch 6
s1dellvl v     = cc 92  v #ch 6
s1deltime v    = cc 90  v #ch 6
s1chorus v     = cc 93  v #ch 6

-- Advanced Oscillator
s1drawsw v     = cc 107 v #ch 6
s1drawmul v    = cc 102 v #ch 6
s1overtone v   = cc 103 v #ch 6
s1chopcomb v   = cc 104 v #ch 6

-------------------------------------------------------------------------------
-- Roland AIRA P-6 Creative Sampler
-- All three play channels are reassigned from the device defaults (reconfigure on device):
--   Auto CH = 3  (keyboard / chromatic sample playback — device default 15)
--   S.CH    = 4  (global sample trigger — 48 pads at default pitch — device default 11)
--   G.CH    = 5  (granular engine — notes + CC — device default 4)
--   Program = 16 (preset select — fixed on device, not configurable)
-- CC numbers below verified against the official Roland P-6 MIDI chart.
-------------------------------------------------------------------------------

-- Preset selection (ch 16, range 0-63)
p6prog p = midicmd "program" #progNum p #ch 16

-- S.CH sample trigger helpers (ch 4)
-- Notes 48-95 map to bank A pad 1 through bank H pad 6 (8 banks × 6 pads)
p6note b s     = 48 + (b-1)*6 + (s-1)
p6trig b s pat = struct pat $ midinote (fromIntegral (p6note b s)) #ch 4 #sustain 0.1
p6pad  n   pat = struct pat $ midinote (fromIntegral (48+n))       #ch 4 #sustain 0.1

-- Granular engine CC functions (G.CH = ch 5)
-- Core grain parameters
p6grainSize v   = cc 23  v #ch 5
p6headPos v     = cc 19  v #ch 5
p6headSpeed v   = cc 20  v #ch 5
p6grains v      = cc 21  v #ch 5
p6grainShape v  = cc 15  v #ch 5
p6spread v      = cc 25  v #ch 5
p6grainJitter v = cc 68  v #ch 5
p6grainRev v    = cc 3   v #ch 5
p6grainTimeKF v = cc 16  v #ch 5
-- Pitch & tuning
p6coarseTune v  = cc 76  v #ch 5
p6fineTune v    = cc 18  v #ch 5
p6detune v      = cc 13  v #ch 5
-- Filter
p6filterType v  = cc 12  v #ch 5
p6cutoff v      = cc 74  v #ch 5
p6res v         = cc 71  v #ch 5
p6filterEnv v   = cc 24  v #ch 5
p6filterKF v    = cc 26  v #ch 5
p6filterVel v   = cc 78  v #ch 5
-- Amplitude envelope (T.Env)
p6attack v      = cc 73  v #ch 5
p6decay v       = cc 75  v #ch 5
p6sustain v     = cc 30  v #ch 5
p6release v     = cc 72  v #ch 5
p6envMode v     = cc 29  v #ch 5
p6envTimeKF v   = cc 77  v #ch 5
p6ampSwitch v   = cc 28  v #ch 5
p6startMode v   = cc 79  v #ch 5
-- Level & dynamics
p6level v       = cc 7   v #ch 5
p6pan v         = cc 10  v #ch 5
p6autoPan v     = cc 9   v #ch 5
p6levelJitter v = cc 14  v #ch 5
-- Lo-Fi
p6lofi v        = cc 17  v #ch 5
p6lofiSw v      = cc 87  v #ch 5
-- Sample selection & routing
p6sample v      = cc 88  v #ch 5
p6outputBus v   = cc 84  v #ch 5
p6sendDelay v   = cc 85  v #ch 5
p6sendReverb v  = cc 86  v #ch 5
-- Effects
p6revTime v     = cc 89  v #ch 5
p6delTime v     = cc 90  v #ch 5
p6revLevel v    = cc 91  v #ch 5
p6delLevel v    = cc 92  v #ch 5

-------------------------------------------------------------------------------
-- Rhythm Library (ported from legacy BootTidal.hs)
--
-- Short mininotation patterns for clave/cascara-style feels, usable with
-- struct/mask on any instrument or drum part.
-------------------------------------------------------------------------------

son32     = "[1 [0 1] 0 1 . 0 1 1 0]/4"
son23     = 2 <~ son32
rumba32   = "[1 [0 1] 0 1 . 0 1 1 0]/4"
rumba23   = 2 <~ rumba32
bossa32   = "[1 [0 1] 0 [0 1] . 0 1 [0 1] 0]/4"
bossa23   = 2 <~ bossa32
bellpat32 = "[1 1 [1 1] [0 1] [1 0] [1 1] [0 1] [0 1]]/4"
bellpat23 = 2 <~ bellpat32

binaryrange lo hi = binary $ lo |+ irand (hi - lo)

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

:{
let over :: Pattern Double -> [a] -> Pattern a
    over _ [] = silence
    over ctrl xs =
      let n = length xs
          step = 1 / fromIntegral n
      in fmap (\x -> xs !! max 0 (floor (min (fromIntegral (n - 1)) (x / step)))) ctrl
    (-->) = over
:}

(o1,o2,o3) = (o,o,o) where o = slow 16 $ lfo tri 0 1
(q1,q2,q3,p4) = (qlink1, qlink2, qlink3, qlink4)
(t1,t2,t3,t4,t5) = (tgl1, tgl2, tgl3, tgl4, tgl5)

-------------------------------------------------------------------------------
-- End BootTidal.hs
-------------------------------------------------------------------------------
