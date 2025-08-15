:set -XOverloadedStrings
:set -XFlexibleContexts
-- :set -XAllowAmbiguousTypes
:set prompt ""
:set prompt-cont ""
:set -Wno-operator-whitespace-ext-conflict
import Sound.Tidal.Context
-- total latency = oLatency + cFrameTimespan
tidal <- startTidal (superdirtTarget {oLatency = 0.15, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cFrameTimespan = 1/20, cEnableLink = False})
-- tidal < startTidal (superdirtTarget {oLatency = 0.40, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cFrameTimespan = 1/10, cEnableLink = False})
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
    d1 = p 101
    d2 = p 102
    d3 = p 103
    d4 = p 104
    d5 = p 105
    d6 = p 106
    d7 = p 107
    d8 = p 108
    d9 = p 109
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

:{

hush = mapM_ ($ silence) [
  p "click", p "count", p "rise",
  d01,d02,d03,d04,d05,d06,d07,d08,d09,d10,d11,d12,d13,d14,d15,d16,
  p "sinewave",
  p "piano",
  p "boeingdrone",
  p "boeingimpact",
  p "bassovertones",
  p "tubeblip",
  p "sampled909",
  p "mpckit",
  p "moogDFAM",
  p "moogMother32",
  p "sh101",
  p "juno"
  ]

:}

hushVis = mapM_ ($ silence) [p "visual"]

-- hush stops only sound
-- hush' stops everything
-- hush'' stops everything including transport

-- din = s "din"
-- midiPort = s "[din, thru]"
-- ch n = (din #midichan (n-1))
-- thru n = (midi #midichan (n-1))
midi = s "thru"
ch n = (midi #midichan (n-1))
ccScale = (*127)
cc n val = control (ccScale val) #io n where io n = (midicmd "control" #ctlNum n)
cc' c n val = control (ccScale val) #io n c where io n c = (midi #midicmd "control" #midichan (c-1) #ctlNum (n))
setCC c n val = once $ control (val) #io n c where io n c = (midi #midicmd "control" #midichan (c-1) #ctlNum (n))
setCC' c n val = control (val) #io n c where io n c = (midi #midicmd "control" #midichan (c-1) #ctlNum (n))
midiScale n = 0.9 + n*0.03
lfo wave lo hi = segment 64 $ range lo hi wave
lfo' wave lo hi = segment 64 $ rangex (s lo) (s hi) $ wave where s n | n > 0 = n | n <= 0 = 0.001
ped = cc 64
vel = pF "amp"
humanise n = vel $ (range (-0.09 * n) (0.09 * n) $ rand)
patToList n pat = fmap value $ queryArc pat (Arc 0 n)
pullBy = (<~)
pushBy = (~>)
(|=) = (#)
resetCycles = streamResetCycles tidal

steptrig p = mono $ 0.036 <~ midinote (toScale [-1, 0, 2, 4, 5, 7, 9, 10] $ (((p-1) `mod` 8)+1)) |= vel 1
oct n = note (12*n)
out = 4
bar b1 b2 p = ((b1+2)*4, (b2+3)*4, p)
phrase = bar
rh = phrase
runSeq = (0, 1, silence)
midiClock out = bar 0 out $ midicmd "midiClock*24" #midi -- #din
initSync = bar 0 0 $ midicmd "stop" #midi -- #din
startSync = bar 6 6 $ midicmd "start" #midi
stopSync out = bar (out+1) (out+1) $ midicmd "stop" #midi -- #din
inKey k b p = note (slow b $ k p)
sync out = [midiClock out, initSync, startSync, stopSync out]
bpm t = cps (t/60)
setbpm tempo = p "t" $ bpm tempo
meter t m s = cps (t/((m/s)*60))
binaryrange min max = (binary $ min |+ irand max-min)

n \\\ s = toScale $ fromIntegral . (+ i n) . toInteger <$> s

hemidemisemiquaver = 1/64
demisemiquaver = 1/32
semiquaver = 1/16
quaver = 1/8
crotchet = 1/4
minim = 1/2

:{

mMaj = [0,2,4,5,7,9,11]
mMin = [0,2,3,5,7,9,11]
hMaj = [0,2,3,5,7,8,11]
hMin = [0,2,4,5,7,8,11]
ionian = [0,2,4,5,7,9,11]
dorian = [0,2,3,5,7,9,10]
phrygian = [0,1,3,5,7,8,10]
lydian = [0,2,4,6,7,9,11]
mixolydian = [0,2,4,5,7,9,10]
aeolian = [0,2,3,5,7,8,10]
locrian = [0,1,3,5,6,8,10]
melMin = [0,2,3,5,7,9,11]
melMin2 = [0,1,3,5,7,9,10]
melMin3 = [0,2,4,6,8,9,11]
melMin4 = [0,2,4,6,7,9,10]
melMin5 = [0,2,4,5,7,8,10]
melMin6 = [0,2,3,5,6,8,10]
melMin7 = [0,1,3,4,6,8,10]
harmMin = [0,2,3,5,7,8,11]
harmMin2 = [0,1,3,5,6,9,10]
harmMin3 = [0,2,4,5,8,9,11]
harmMin4 = [0,2,3,6,7,9,10]
harmMin5 = [0,1,4,5,7,8,10]
harmMin6 = [0,3,4,6,7,9,11]
harmMin7 = [0,1,3,4,6,8,9]
penta = [0,2,4,7,9]
penta2 = [0,2,5,7,10]
penta3 = [0,3,5,8,10]
penta4 = [0,2,5,7,9]
penta5 = [0,3,5,7,10]
dimWhole = [0,2,3,5,6,8,9,11]
dimHalf = [0,1,3,4,6,7,9,10]
wholeTone = [0,2,4,6,8,10]

mode m key pat = key (pat |+ m)
-- transpose st key pat = (key pat) |+ st

program' prog bank = stack [
  midicmd "program" #progNum prog,
  pushBy 0.001 $ control bank #midicmd "control" #ctlNum 0
  ]

program prog = midicmd "program" #progNum prog #ch 14

type Section = ((Pattern Int, Pattern Double),(Pattern Int, Pattern Double), Int)

(keySig,a,b) = (
  C \\\ mMaj
  ,(f aTheme, f aHarm, 0) :: Section
  ,(f bTheme, f bHarm, 1) :: Section
  )
    where
      f a = (fastcat $ fst a, fastcat $ snd a)
      aTheme = ([ -- A THEME
        "0"
        ],[ -- TRANSPOSE
        "0"
        ])
      aHarm = ([ -- A HARMONY
        "0"
        ],[ -- TRANSPOSE
        "0"
        ])
      bTheme = ([ -- B THEME
        "0"
        ],[ -- TRANSPOSE
        "0"
        ])
      bHarm = ([ -- B HARMONY
        "0"
        ],[ -- TRANSPOSE
        "0"
        ])

timeFuncs mult = [("minim", fast 2),
                  ("2", fast 2),
                  ("crotchet", fast 4),
                  ("4", fast 4),
                  ("quaver", fast 8),
                  ("8", fast 8),
                  ("semiquaver", fast 16),
                  ("16", fast 16),
                  ("demisemiquaver", fast 32),
                  ("32", fast 32),
                  ("echo", stut 2 0.75 (quaver/mult)),
                  ("echos", stut 2 0.75 (crotchet/mult)),
                  ("echoq", stut 2 0.75 (minim/mult)),
                  ("echom", stut 2 0.75 (1/mult)),
                  ("lead", ((rev) . (stut 2 0.75) (quaver/mult))),
                  ("leads", ((rev) . (stut 2 0.75) (crotchet/mult))),
                  ("leadq", ((rev) . (stut 2 0.75) (minim/mult))),
                  ("leadm", ((rev) . (stut 2 0.75) (1/mult))),
                  ("pull", pullBy (quaver/mult)),
                  ("push", pushBy (quaver/mult))
                ]

:}

:{

dynamicMarks chan = [
    ("x", note "0"),
    ("ffff", note "0" #vel 1 #ch chan),
    ("fff", note "0" #vel 0.9 #ch chan),
    ("ff", note "0" #vel 0.75 #ch chan),
    ("f", note "0" #vel 0.65 #ch chan),
    ("mf", note "0" #vel 0.55 #ch chan),
    ("mp", note "0" #vel 0.45 #ch chan),
    ("p", note "0" #vel 0.40 #ch chan),
    ("pp", note "0" #vel 0.35 #ch chan),
    ("ppp", note "0"  #vel 0.30 #ch chan),
    ("pppp", note "0" #vel 0.25 #ch chan)
  ]

mixScale = 1
fllvl = (*mixScale) 0.36 -- ch 1
oblvl = (*mixScale) 0.32 -- ch 2
bnlvl = (*mixScale) 0.40 -- ch 3
hnlvl = (*mixScale) 0.60 -- ch 4
tbnlvl = (*mixScale) 0.90 -- ch 5
timplvl = (*mixScale) 1.00 -- ch 6
pizzlvl = (*mixScale) 0.50 -- ch 7
bdlvl = (*mixScale) 1.00 -- ch 8
hpslvl = (*mixScale) 0.20 -- ch 9
hplvl = (*mixScale) 0.05 -- ch 9
perclvl = (*mixScale) 0.70 -- ch 10
cllvl = (*mixScale) 0.32 -- ch 11
vn1lvl = (*mixScale) 0.40 -- ch 13
vn2lvl = (*mixScale) 0.30 -- ch 14
vclvl = (*mixScale) 0.50 -- ch 15
dblvl = (*mixScale) 0.20 -- ch 16

midiInstrument chan mixlvl dyn = do
  let bars = 1
      mult = fromList [(fromIntegral . ceiling) bars] :: Pattern Time
      fs   = (timeFuncs mult) ++ [
                (".", (#legato 0.125))
                ]
   in ur bars dyn (dynamicMarks chan) fs

:}

-- divisi = 2/3
flute = midiInstrument 1 fllvl
oboe = midiInstrument 2 oblvl
bassoon = midiInstrument 3 bnlvl
horn = midiInstrument 4 hnlvl
trombone = midiInstrument 5 tbnlvl
tuba = midiInstrument 6 timplvl
pizzicato = midiInstrument 7 pizzlvl
bassdrum = midiInstrument 8 bdlvl
-- harpsichord = midiInstrument 9 bdlvl
harp = midiInstrument 9 bdlvl
-- koala = midiInstrument 11 1
clarinet = midiInstrument 11 cllvl
moog = midiInstrument 12 1
violin1 = midiInstrument 13 vn1lvl
violin2 = midiInstrument 14 vn2lvl
viola = midiInstrument 15 dblvl
cello = midiInstrument 16 vclvl
contrabass = midiInstrument 15 dblvl

:{

timpani dyn = do
  let bars = 1
      chan = 6
      mult = fromList [(fromIntegral . ceiling) bars] :: Pattern Time
      fs   = (timeFuncs mult) ++ []
      ps chan = [
          ("ffff", note "0" #vel 1 #ch chan),
          ("fff", note "0" #vel 0.9 #ch chan),
          ("ff", note "0" #vel 0.75 #ch chan),
          ("f", note "0" #vel 0.65 #ch chan),
          ("mf", note "0" #vel 0.55 #ch chan),
          ("mp", note "0" #vel 0.45 #ch chan),
          ("p", note "0" #vel 0.40 #ch chan),
          ("pp", note "0" #vel 0.35 #ch chan),
          ("ppp", note "0" #vel 0.30 #ch chan),
          ("pppp", note "0" #vel 0.25 #ch chan)
        ]
    in ur bars dyn (ps chan) fs #sustain 0.00000001

perc bars pat = do
  let --bars = 1
      ps = [("tamb", midinote 54 #vel 0.5 #ch 10),
           ("crash", midinote 49 #vel 0.5 #ch 10),
           ("crash2", midinote 57 #vel 0.5 #ch 10),
           ("ride", midinote 51 #vel 0.5 #ch 10),
           ("ride2", midinote 59 #vel 0.5 #ch 10),
           ("ridebell", midinote 53 #vel 0.5 #ch 10),
           ("china", midinote 52 #vel 0.5 #ch 10),
           ("splash", midinote 55 #vel 0.5 #ch 10),
           ("sidestick", midinote 37 #vel 0.5 #ch 10),
           ("snare", midinote 40 #vel 0.5 #ch 10),
           ("snare2", midinote 38 #vel 0.5 #ch 10),
           ("triopen", midinote 81 #vel 0.5 #ch 10),
           ("trimute", midinote 80 #vel 0.5 #ch 10),
           ("hatopen", midinote 46 #vel 0.5 #ch 10),
           ("hatclosed", midinote 42 #vel 0.5 #ch 10),
           ("hatped", midinote 44 #vel 0.5 #ch 10),
           ("chimes", midinote 84 #vel 0.5 #ch 10),
           ("kick", midinote 36 #vel 0.5 #ch 10),
           ("kick2", midinote 55 #vel 0.5 #ch 10),
           ("clave", midinote 75 #vel 0.5 #ch 10),
           ("clave2", midinote 87 #vel 0.5 #ch 10)
          ]
      mult = fromList [(fromIntegral . ceiling) bars] :: Pattern Time
      fs   = (timeFuncs mult) ++ [
                ("ffff", (#vel 1)),
                ("fff", (#vel 0.9)),
                ("ff", (#vel 0.75)),
                ("f", (#vel 0.65)),
                ("mf", (#vel 0.55)),
                ("mp", (#vel 0.45)),
                ("p", (#vel 0.35)),
                ("pp", (#vel 0.35)),
                ("ppp", (#vel 0.30)),
                ("pppp", (#vel 0.25))
                ]
   in ur bars pat ps fs

-- sidestick pat = midinote (pat |= 37) #ch 10
-- tamb pat = midinote (pat |= 54) #ch 10

:}

fl = ch 1
ob = ch 2
bn = ch 3
hn = ch 4
tbn = ch 5
timp = ch 6
tba = ch 6
pizz = ch 7
bd = ch 8
hp = ch 9
pcn = ch 10
cl = ch 11
m32 = ch 12
vn1 = ch 13
vn2 = ch 14
va = ch 16
vc = ch 15
db = ch 16

allNotesOff = setCC "[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]" 123 1
panic = allNotesOff >> hush'
hush'' = panic

:{
fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, x, _) = x

snd'' :: (a, b, c) -> c
snd'' (_, _, x) = x
:}

on1 = within (0,0.25)
up1 = within (0.125,0.25)
on2 = within (0.25,0.5)
up2 = within (0.375,0.5)
on3 = within (0.5,0.75)
up3 = within (0.625,0.75)
on4 = within (0.75,1)
up4 = within (0.825,1)

:{

let setI = streamSetI tidal
    setF = streamSetF tidal
    setS = streamSetS tidal
    setR = streamSetI tidal
    setB = streamSetB tidal

:}


:{

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

over :: Pattern Double -> [a] -> Pattern a
over ctrl [] = silence
over ctrl xs =
  let step = 1 / fromIntegral (length xs)
  in fmap (\x -> xs !! floor (min (fromIntegral (length xs - 1)) (x / step))) ctrl

(-->) ctrl xs = over ctrl xs
(<--) pat ctrlMap = pat |+ segment 128 ctrlMap

:}

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

adagio = 71
andante = 85
moderato = 110
allegro = 135
metronome d = p "click" $ click "1" |= vel "[0.8 0.4!3]/4" |* vel d
son32 = "[1 [0 1] 0 1 . 0 1 1 0]/4"
son23 = 2 <~ son32
rumba32 ="[1 [0 1] 0 1 . 0 1 1 0]/4"
rumba23 = 2 <~ rumba32
bossa32 = "[1 [0 1] 0 [0 1] . 0 1 [0 1] 0]/4"
bossa23 = 2 <~ bossa32
bellpat32 = "[1 1 [1 1] [0 1] [1 0] [1 1] [0 1] [0 1]]/4"
bellpat23 = 2 <~ bellpat32

ecbc prog len pat =
  slow (4*len) (cat $ midinote <$>
  ( fmap filterHarmonicPat (
    (`toScale` (fast (4*len) pat)) <$>
    fmap fromInteger <$> ecbcHarmony prog
    ))
  )

applyProg :: VoiceFunction -> Progression -> Pattern Time -> Pattern Int -> Pattern ValueMap
applyProg voiceFunc prog len pat =
  slow (4*len) (cat $ note <$>
  (`toScale` (fast (4*len) pat)) <$>
    fmap fromInteger <$> voiceFunc prog
  )

voiceRange :: (Int, Int) -> Pattern Int -> Pattern Int
voiceRange (lo,hi) = filterEvents (\e -> value e >= lo && value e <= hi)

arrange :: VoiceFunction -> Progression -> Pattern Time -> (Int, Int) -> [Pattern Int] -> Pattern ValueMap
arrange voiceFunc prog rep register pats =
  let pat = stack pats
      rangedPattern = voiceRange register pat
      applyProg voiceFunc prog len pat =
        slow (4*len) (cat $ note <$>
          (`toScale` (fast (4*len) pat)) <$>
          fmap fromInteger <$> voiceFunc prog
          )
   in applyProg voiceFunc prog rep rangedPattern

:}

:{

hh pat = do
  let bars = 1
      ps = [
            ("x", midinote 2 #ch 10 #sustain 0.05 #vel 0.5),
            ("1", midinote 2 #ch 10 #sustain 0.05 #vel 0.5),
            ("o", midinote 3 #ch 10 #sustain 0.05 #vel 0.5),
            ("2", midinote 3 #ch 10 #sustain 0.05 #vel 0.5)
            ]
      mult = fromList [(fromIntegral . ceiling) bars] :: Pattern Time
      fs   = (timeFuncs mult) ++ [
                ("ffff", (#vel 1)),
                ("fff", (#vel 0.9)),
                ("ff", (#vel 0.75)),
                ("f", (#vel 0.65)),
                ("mf", (#vel 0.55)),
                ("mp", (#vel 0.45)),
                ("p", (#vel 0.35)),
                ("pp", (#vel 0.35)),
                ("ppp", (#vel 0.30)),
                ("pppp", (#vel 0.25))
                ]
   in ur bars pat ps fs

:}

assignable prog = midicmd "program" #progNum (prog+70) #ch 14
m32seq pat bank = midicmd "program" #progNum (pat+((bank-1)*8)) #ch 14
m32seq' absPat = midicmd "program" #progNum absPat #ch 14
perpetuate v = cc 64 v #ch 14
-- portamento i v = cc 65 i #cc 5 v #ch 14
portamento v = cc 65 1 #cc 5 v #ch 14
assignaccent = assignable 1 #ch 14
assignclock0 = assignable 2 #ch 14
assignclock2 = assignable 3 #ch 14
assignclock4 = assignable 4 #ch 14
assignramp = assignable 5 #ch 14
assignsaw = assignable 6 #ch 14
assigntri = assignable 7 #ch 14
assignrandom = assignable 8 #ch 14
assigntrigger = assignable 9 #ch 14
assignvel = assignable 10 #ch 14
assignpressure = assignable 11 #ch 14
assignbend = assignable 12 #ch 14
assigncc1 = assignable 13 #ch 14
assigncc2 = assignable 14 #ch 14
assigncc4 = assignable 15 #ch 14
assigncc7 = assignable 16 #ch 14

-- Select a pattern by its absolute number (0-63) on channel 15
s1pat p = midicmd "program" #progNum p #ch 15

-- Select a pattern by bank (1-8) and pattern-within-bank (1-8) on channel 15
s1seq pat bank = midicmd "program" #progNum (pat-1+((bank-1)*8)) #ch 15

-- All parameter controls on Channel 15
--
-- Global & Performance
modwheel v = cc 1 v #ch 15        -- CC 1: Modulation Wheel
portatime v = cc 5 v #ch 15       -- CC 5: Portamento Time
pan v = cc 10 v #ch 15            -- CC 10: Pan
expression v = cc 11 v #ch 15     -- CC 11: Expression
portamode v = cc 31 v #ch 15      -- CC 31: Portamento Mode
damper v = cc 64 v #ch 15         -- CC 64: Damper Pedal (Sustain)
portasw v = cc 65 v #ch 15        -- CC 65: Portamento On/Off Switch
finetune v = cc 76 v #ch 15       -- CC 76: Fine Tune
transposesw v = cc 77 v #ch 15    -- CC 77: Transpose Switch

-- Oscillator (OSC)
osclfo v = cc 13 v #ch 15         -- CC 13: OSC LFO Depth
oscrange v = cc 14 v #ch 15       -- CC 14: OSC Range
oscpwm v = cc 15 v #ch 15         -- CC 15: OSC Pulse Width
oscpwmsrc v = cc 16 v #ch 15      -- CC 16: OSC PWM Source
oscbendsens v = cc 18 v #ch 15    -- CC 18: OSC Bend Sensitivity
oscsquarelvl v = cc 19 v #ch 15   -- CC 19: OSC Square Wave Level
oscsawlvl v = cc 20 v #ch 15      -- CC 20: OSC Saw Wave Level
oscsublvl v = cc 21 v #ch 15      -- CC 21: OSC Sub Level
oscsuboct v = cc 22 v #ch 15      -- CC 22: OSC Sub Octave Type
oscnoiselvl v = cc 23 v #ch 15    -- CC 23: OSC Noise Level
noisemode v = cc 78 v #ch 15      -- CC 78: Noise Mode

-- Filter & Amplifier (AMP)
cutoff v = cc 74 v #ch 15         -- CC 74: Filter Cutoff Frequency
res v = cc 71 v #ch 15            -- CC 71: Filter Resonance
filterenv v = cc 24 v #ch 15      -- CC 24: Filter Envelope Depth
filterlfo v = cc 25 v #ch 15      -- CC 25: Filter LFO Depth
keytrack v = cc 26 v #ch 15 -- CC 26: Filter Keyboard Follow
filterbendsens v = cc 27 v #ch 15 -- CC 27: Filter Bend Sensitivity
ampenvmode v = cc 28 v #ch 15     -- CC 28: Amp Envelope Mode Switch

-- LFO & Envelope (ENV)
lforate v = cc 3 v #ch 15         -- CC 3: LFO Rate
lfowave v = cc 12 v #ch 15        -- CC 12: LFO Wave Form
lfomoddepth v = cc 17 v #ch 15    -- CC 17: LFO Modulation Depth
lfomode v = cc 79 v #ch 15        -- CC 79: LFO Mode
lfokeytrg v = cc 105 v #ch 15     -- CC 105: LFO Key Trigger
lfosync v = cc 106 v #ch 15       -- CC 106: LFO Sync
attack v = cc 73 v #ch 15         -- CC 73: Envelope Attack
decay v = cc 75 v #ch 15          -- CC 75: Envelope Decay
envsustain v = cc 30 v #ch 15     -- CC 30: Envelope Sustain
release v = cc 72 v #ch 15        -- CC 72: Envelope Release
envtrgmode v = cc 29 v #ch 15     -- CC 29: Envelope Trigger Mode

-- Chord Mode
polymode v = cc 80 v #ch 15         -- CC 80: Poly Mode
chordvoice2sw v = cc 81 v #ch 15    -- CC 81: Chord Voice 2 Switch
chordvoice3sw v = cc 82 v #ch 15    -- CC 82: Chord Voice 3 Switch
chordvoice4sw v = cc 83 v #ch 15    -- CC 83: Chord Voice 4 Switch
chordvoice2shift v = cc 85 v #ch 15 -- CC 85: Chord Voice 2 Key Shift
chordvoice3shift v = cc 86 v #ch 15 -- CC 86: Chord Voice 3 Key Shift
chordvoice4shift v = cc 87 v #ch 15 -- CC 87: Chord Voice 4 Key Shift

-- Effects (FX)
revlvl v = cc 91 v #ch 15         -- CC 91: Reverb Level
revtime v = cc 89 v #ch 15        -- CC 89: Reverb Time
dellvl v = cc 92 v #ch 15         -- CC 92: Delay Level
deltime v = cc 90 v #ch 15        -- CC 90: Delay Time
choruslvl v = cc 93 v #ch 15      -- CC 93: Chorus Level

-- Advanced Oscillator Functions
drawsw v = cc 107 v #ch 15        -- CC 107: OSC Draw Switch
drawmultiply v = cc 102 v #ch 15  -- CC 102: OSC Draw Multiply
chopovertone v = cc 103 v #ch 15  -- CC 103: OSC Chop Overtone
chopcomb v = cc 104 v #ch 15      -- CC 104: OSC Chop Comb

-- Piano
wonky v = cc 1 v #ch 03

-- t303
slope v = cc 100 v #ch 8
bright v = cc 101 v #ch 8
reso v = cc 102 v #ch 8
contour v = cc 103 v #ch 8
waveform v = let v' = v --> [0.34, 0.67] in cc 104 v' #ch 8

:{

tempo = 120

count s rep d =
  let progLen (Progression (chords, _, _)) = length chords
      len = pure $ fromIntegral (progLen s)
      bars = run (pure $ progLen s) |+ 8
      pattern = slow (4 * len * fromIntegral rep)
              $ midinote (fromIntegral <$> bars) |* vel d
   in p "count" $ pattern # ch 10

rise len s r d = d01 $ do
  id $
    slow (len*4) $
      stack [n "~"
          -- --
        ,arrange flow s r (-9,9) ["~"
        ,"[-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9]/2"
        ] |* vel 0.99
          -- --
        ,segment 256 $ cc 1 $ fastcat [ 0
          ,0,0,0,0
          ,lfo saw 0 0.1
          ,lfo saw 0.1 0.3
          ,lfo saw 0.3 0.6
          ,lfo saw 0.6 1
          ]
          -- --
      ]# ch 01
      |* vel d

 -- DRUM MACHINE INIT
p10 f d = d10 $ id
  $ f
  $ stack [ silence
    ,kick "1" #vel 0.3
    ] |* vel d

 -- DFAM STEPTRIG INIT
p11 f = d11 $ id
  $ mono
  $ f
  $ stack [ silence
    ,steptrig $ "[1 2 3 4 5 6 7 8]/2"
  ] # ch 11

 -- DFAM OSC 1 INIT
p12 f s r d = d12 $ do
  id $
    f $
      arrange flow s r (-9,9) ["~"
        ,"[0]"
      ]# ch 12
      |- oct 1
      |* vel d

 -- DFAM OSC 2 INIT
p13 f s r d = d13 $ do
  id $
    f $
      arrange flow s r (-9,9) ["~"
        ,"[0]"
      ]# ch 13
      |- oct 1
      |* vel d

 -- M32 INIT
p14 f s r d = d14 $ do
  id $
    f $
      arrange flow s r (-9,9) ["~"
        ,"[0*4]/2"
      ]# ch 14
      |* vel d

 -- S-1 INIT
p15 f s r d = d15 $ do
  id $
    f $
      arrange root s r (-9,9) ["~"
        ,"[-1,0,1]/4"
      ]# ch 15
      -- |= legato 0.95
      |* vel d

 -- SYNHARP INIT
p16 f s r d = d16 $ do
  id $
    f $
      arrange root s r (-9,9) ["~"
        ,"[-1,0,1]/4"
      ]# ch 16
      |* vel d

state = prog flat $ take 4 $ cycle [[0,7,10]]

:}


:set prompt "tidal> "
