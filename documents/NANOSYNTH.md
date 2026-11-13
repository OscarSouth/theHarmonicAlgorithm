# Alesis NanoSynth

The studio orchestra. A 64-voice General MIDI module on the Alesis QS engine,
driven from the same performer surface as the Roland JV-1010 at home: declare
`rig = nanoSynth` in `live/local/rig.hs` and `violin1 T (0,1) k vl flow Soprano # pizz`
sounds on it unchanged. This document is the device reference — the rig as
configured, how the module is addressed, and its complete program list.
Methodology lives in [ALGORITHMIC_ORCHESTRATION.md](ALGORITHMIC_ORCHESTRATION.md);
the studio rig around it in [LIVE_ENVIRONMENT.md](LIVE_ENVIRONMENT.md).

Source for the module facts: *Alesis NanoSynth Reference Manual* (49 pp.),
pages cited inline; the PDF is at
`http://pdf.textfiles.com/manuals/STARINMANUALS/Alesis/Manuals/Nanosynth.pdf`.
Its text layer is unreadable, so the program list was transcribed from the page
images. The rig itself is the September 2021 concert configuration
(`udagan-sensoria-concert/state/baroque_mix.tidal`), carried over verbatim.

## Signal chain

Code → TidalCycles → SuperDirt → iCA4+ DIN → CME U6 → NanoSynth → orchestral music

One port, sixteen channels. The U6 splits the DIN to the MPC and to the nano
(everything, both) and merges the MPC's out into the nano line; the Moog
Mother-32 hangs off the MPC's CV out on channel 6, which the module leaves
blank. The chain and its rules are in
[LIVE_ENVIRONMENT.md](LIVE_ENVIRONMENT.md). The JV-1010 and the rest of the
studio synth rig are never live with it.

## The rig

Definitions: `Harmonic.Interface.Tidal.Devices.NanoSynth` (`nanoSynth`,
`nanoMix` — the table below as one row list, from which `nanoPrograms` and
the five controller tables derive — and `nanoSetup`). Walk-through and
range tests: `live/docs/NANOSYNTH_CATALOGUE.tidal`.

| ch | instrument | program / bank | patch | CC7 | pan | CC91 | CC1 | CC11 |
|---:|---|---|---|---:|---:|---:|---:|---:|
| 1 | flute | 74 / Preset 3 | SingleFlut | 0.36 | −0.20 | 0.85 | 0 | 1 |
| 2 | oboe | 68 / Preset 3 | Ebony Oboe | 0.32 | 0.20 | 0.90 | 0 | 1 |
| 3 | clarinet | 71 / Preset 2 | 1stClarnet | 0.32 | −0.25 | 0.85 | 0 | 1 |
| 4 | bassoon | 70 / Preset 2 | SoloBasoon | 0.40 | 0.25 | 0.90 | 0 | 1 |
| 5 | horn | 60 / User | FlugelSolo | 0.60 | −0.40 | 0.95 | 0 | 1 |
| 6 | the Mother-32's channel: blank | 127 / User | the blank program | 0 | 0 | 0 | 0 | 0 |
| 7 | harp, the continuo | 46 / Preset 3 | Harpitz | 0.10 | −0.20 | 0.95 | 0 | 1 |
| 8 | timpani, with tuba | 60 / Preset 4 | Documentar | 0.90 | −0.30 | 0.85 | 0 | 1 |
| 9 | orchestra hit, as bass drum | 55 / Preset 3 | Danz Hitz | 1.00 | 0.00 | 0.90 | 1 | 1 |
| 10 | percussion: tambourine 54, woodblock 76, triangle 81 (shared with the MPC) | 118 / Preset 3 | 15ips Kit | 0.70 | −0.20 | 0.85 | 0 | 1 |
| 11 | trombone, bass trombone | 57 / User | Bone Tone | 0.90 | 0.10 | 0.80 | 0 | 1 |
| 12 | strings pizzicato | 45 / Preset 3 | Pizz Pluck | 0.50 | 0.15 | 0.90 | 0 | 1 |
| 13 | violin 1 | 49 / GM | Slo String | 0.40 | −0.80 | 0.80 | 0 | 1 |
| 14 | violin 2 | 49 / GM | Slo String | 0.30 | 0.80 | 0.95 | 0 | 1 |
| 15 | cello | 43 / Preset 3 | Celli | 0.50 | 0.30 | 0.85 | 0 | 1 |
| 16 | double bass, with viola | 41 / User | BiViola | 0.20 | −0.30 | 0.95 | 0 | 1 |

The layout is the JV-1010's wherever the module can follow it — winds 1–5,
harp 7, timpani 8, bass drum 9, the MPC lane 10, pizzicato 12, strings
13–16 — so `# pizz` is channel 12 and the continuo channel 7 on both rigs.
Two channels differ: 6 is the Mother-32's (see below), and the trombone,
which the JV keeps on 6, takes 11, the JV's tam-tam channel (a voice the
module lacks). The string block is sections where the JV's is techniques:
the module has one patch per channel and no technique sub-channels. The
voices and their mix are the 2021 concert's, unchanged; each row's values
travel with its program, not its channel.

Pan is written −1 (left) to 1 (right) and sent as 0–127. Two channels carry
one patch for two instruments: 8 (timpani below, tuba sustaining above) and
16 (double bass and viola); the bass-trombone line shares the trombone's
channel through its own range. The tam-tam has no voice here. The continuo
is the harp channel, so `setContinuo nanoHarpV` and
`setContinuo nanoHarpsichordV` (8'4'Harpsi, 6 / User) never touch the
pizzicato on 12.

Channel 10 carries the orchestra's percussion — 15ips Kit, 118 / Preset 3,
answering General MIDI numbers — on the channel the MPC's service lane also
uses (sub, kit, click, count, display). The two coexist by note band: the MPC's
pads take 0–48 and the `kmap` notes 108–119, so the kit's voices are chosen
above that — tambourine 54, woodblock 76, triangle 81 (`tambourine`,
`woodblock`, `triangle` in the orchestra) — and a sub-bass note never lands on
a kit pad; the kit's own kick, snare and hats (35–48) are not part of the
orchestra, because they sit in the MPC's band. `nanoKit` remains as the
one-line way to put the kit back after a program change.

Channel 6 is the Moog Mother-32's. It receives on 6 from the MPC's CV out —
the MPC hears every channel on the U6's Out 1 — and the module, which hears
the same channel on Out 2, holds the blank program there at level and
expression 0, so the orchestra's chain carries one analogue voice without
the module doubling it. The 2021 pieces' synth lines were written for the
Mother-32 on this chain; they address channel 6. With the trombone on 11
and the pizzicato on 12, the MPC's `hmnx` and `slce` programs (11, 12) are
not live while the orchestra plays. The alternative worth auditioning for channel 16 is
41 / Preset 3, **C-Bs&Viola**, a contrabass-and-viola split; the catalogue has
it as a one-line swap.

### Articulations

`pizz` re-routes any string line to channel 12, as on the JV-1010. `spicc`, `marc`, `legg` and
`arco` are identity here — the line stays on its own channel and sounds as
written — so a block written for the JV-1010 loads unchanged.

### Ranges

An instrument's range is its own, not the module's: the bounds live with the
instrument functions in `Harmonic.Interface.Tidal.Orchestra` and are shared
with the JV-1010 rig (Tidal note space, MIDI − 60):

| instrument | range | | instrument | range |
|---|---|---|---|---|
| flute | −12 … 26 | | violin 1, violin 2 | −5 … 45 |
| oboe | −2 … 33 | | viola | −12 … 28 |
| clarinet | −22 … 34 | | cello | −24 … 24 |
| bassoon | −28 … 15 | | double bass | −36 … 0 |
| horn | −29 … 17 | | harp | −29 … 42 |
| trombone | −28 … 17 | | timpani | −22 … 0 |
| bass trombone | −39 … −5 | | | |

#### Patch zones probed in 2021

What the concert's range probes found each patch sounding across. Where a
zone is narrower than the instrument's range the patch may thin out at an end;
where it differs outright (the timpani) it is a hardware question for the
catalogue's TEST 9.

| patch | zone | | patch | zone |
|---|---|---|---|---|
| SingleFlut (ch 1) | 0 … 33, piccolo 14 … 48 | | Slo String (ch 13, 14) | −5 … 43 |
| Ebony Oboe (ch 2) | −2 … 31, cor anglais −8 … 24 | | BiViola, mid (ch 16) | −12 … 33 |
| SoloBasoon (ch 4) | −26 … 15 | | Celli (ch 15) | −24 … 19 |
| FlugelSolo (ch 5) | −27 … 17 | | BiViola, low (ch 16) | −36 … −2 |
| Bone Tone (ch 11) | −8 … 24, bass trombone −26 … 8 | | Documentar timpani (ch 8) | −34 … −15 |
| 1stClarnet (ch 3) | −10 … 31, bass clarinet −22 … 14 | | Documentar solo tuba (ch 8) | −6 … 7 |
| trumpet, unbound | −8 … 24 | | Documentar snares (ch 8) | −10, −9, −8 |

### Cold configuration

`setupRig` sends, in order: Bank Select then Program Change on every channel,
then the CC7, CC10, CC91, CC1 and CC11 tables, a tenth of a second between
messages. Run it once after power-up. The catalogue walks the same messages
one block at a time so a wrong patch is found by ear.

## Addressing the module

- **Programs: 640 in five banks of 128** (p. 22). Bank Select is **CC0 alone**
  with values 0 General MIDI, 1 User, 2 Preset 2, 3 Preset 3, 4 Preset 4 (p. 30);
  CC32 is not read. Program numbers are 0–127 as printed in the list below.
- **Bank Select takes effect at once**, keeping the program number: Bank 0
  Program 33 followed by Bank Select 1 plays Bank 1 Program 33 (p. 23). A
  Program Change on its own changes the program within the current bank. The
  manual's advice is Bank Select then Program Change, every time (p. 22).
- **Front panel reaches only GM and User**; the CATEGORY knob picks a group of
  sixteen, the PROGRAM knob positions 1–8 are GM and 9–16 User. Banks 2–4 are
  reachable only by MIDI, and touching a knob drops the channel back to GM or
  User (pp. 22–23).
- **Every channel always answers.** To silence one, assign **User bank
  program 127**, a hard-wired blank program that no bank load overwrites
  (pp. 26, 45).
- **Recognised controllers** (p. 46): 1 modulation, 7 volume, 10 pan,
  11 expression, 64 sustain, 91 effect level, 123 all notes off. Program
  change 0–127. No aftertouch, no pitch bend, no SysEx in the chart.
- **Voices**: 64, dynamically allocated; a program may take one, two or four
  voices per note, so polyphony is 64, 32 or 16 (pp. 25–26). Multitimbral in
  QS Mix Mode on all sixteen channels (p. 25).
- **Engine**: 16-bit 48 kHz sample ROM, 8 MB, 462 sample families; four
  effect busses; MIDI In and Out/Thru; a serial port for Macintosh or PC
  (p. 25). The sound architecture is the Alesis QS6's, so it can be programmed
  in program mode or drum mode with a QS editor (p. 34).
- **Drum kits are programs.** In GM bank the Drums/Perc rows are GM percussion
  instruments; in the other banks rows 112–119 are kits (D4 Pwr Kit, Real Rock,
  15ips Kit …), and a kit answers to General MIDI note numbers (`gmKick` …).

## Program list

Transcribed from the manual's program list (pp. 30–33). Rows are MIDI program
numbers; columns are banks, with the CC0 value in the heading. GM categories
run in groups of eight.

### Piano (000–007)

| # | GM (0) | User (1) | Preset 2 (2) | Preset 3 (3) | Preset 4 (4) |
|--:|---|---|---|---|---|
| 000 | Piano 1 | 8va Piano | Solo Piano | ClasclGrnd | GrandPiano |
| 001 | Piano 2 | PianoMorph | HousePiano | HyperPiano | DancePiano |
| 002 | Elec Grand | Whirl Lee | Electratak | Syn Piano | EGrd & Pad |
| 003 | Honky-Tonk | Player Pno | SalloonKey | Balladeer | DirtyWurly |
| 004 | E.Piano 1 | 61 Tines | Suitcase | Nice Tines | Mars E Pno |
| 005 | E.Piano 2 | Rayz Roadz | Hard Roads | Smooth EP | SuperRoadz |
| 006 | Harpschrd | 8'4'Harpsi | TrueHarpsi | Octachord | Ana Harpsi |
| 007 | Clav | Clavitube | Clavislap | ProfitClav | Digi Clav |

### Chromatic (008–015)

| # | GM (0) | User (1) | Preset 2 (2) | Preset 3 (3) | Preset 4 (4) |
|--:|---|---|---|---|---|
| 008 | Celeste | Potsticker | ShortCeles | FairyBellz | Chiff Bell |
| 009 | Glockenspl | AlloyGlock | Gloknspark | GlassBells | Tambigloxx |
| 010 | Music Box | Charms | SweetBells | Clear Bell | Basic Bell |
| 011 | Vibes | Mad Vibes | Cool Vibes | Vibraphone | MorphBells |
| 012 | Marimba | BasMarimba | MarmbaIsle | Wood Sign | Bellarimba |
| 013 | Xylophone | Xylobrite | Woody Xylo | Steelophon | Brake Drum |
| 014 | TubularBel | ClockTower | Tubulous | Tubularis | Watercan |
| 015 | Santur | Britecimmr | HamrDulcmr | Dulcioto | Lunk Harp |

### Organ (016–023)

| # | GM (0) | User (1) | Preset 2 (2) | Preset 3 (3) | Preset 4 (4) |
|--:|---|---|---|---|---|
| 016 | Organ 1 | LFO Lezly | PrcsvBlues | DrawbarCtl | ShadeOpale |
| 017 | Organ 2 | Survival B | Vacuum B | MW Organ | Eng Organ |
| 018 | Organ 3 | High Life | BigBadPerc | ToneWhlPrc | BlueZorgan |
| 019 | ChurchOrgn | Full Ranks | ChurchPipe | BritePipes | SftPipeOrg |
| 020 | Reed Organ | Gothic Org | Reed Stops | DigiPump | MellowPump |
| 021 | Accordian | ClrAcrdion | FrAccrdion | WrmAcrdion | One Accord |
| 022 | Harmonica | WhammerJmr | PocketHarp | F-harmonca | BzzHarmnca |
| 023 | Bandoneon | Palermo | AhOneAnna2 | DarkHrmnca | SynAccrdn |

### Guitar (024–031)

| # | GM (0) | User (1) | Preset 2 (2) | Preset 3 (3) | Preset 4 (4) |
|--:|---|---|---|---|---|
| 024 | Nylon Gtr | ClassiclAx | ThickNylon | Flamenco 6 | Nylon&Oohs |
| 025 | Steel Gtr | SteelUrslf | DoublSteel | Acous6Strg | FolkBarGtr |
| 026 | Jazz Gtr | PedalSteel | PassGuitar | HawaiianGt | GuitarPoem |
| 027 | Clean Gtr | 818 Guitar | PulpGuitar | CountryGtr | Royal Coil |
| 028 | Mute Gtr | Chunky | Funky Mute | Total Chug | TreMellow |
| 029 | Overdrive | Rock Drive | OvrdriveGt | TurboCtrlC | CoralLezli |
| 030 | Distortion | Feedbacker | Rock Lead | DistortdGt | HeroHarmnx |
| 031 | Gt.Harmnix | DstHrmonic | ElHarmonic | AcHarmonic | Strummers |

### Bass (032–039)

| # | GM (0) | User (1) | Preset 2 (2) | Preset 3 (3) | Preset 4 (4) |
|--:|---|---|---|---|---|
| 032 | AcousBass | BigUpright | AcousBassV | FatUpright | ArndsHouse |
| 033 | FingerBass | Sure Bass | Deep Bass | Face Bass | Octaver |
| 034 | PickedBass | Heavy Bass | Mu Bass | SharpStick | 007 Bass |
| 035 | Fretless | No Frets! | VolumeKnob | SmoothNeck | Fretlissyn |
| 036 | Slap Bass1 | Slapstick | FlaminBass | Popless | Dist Bass |
| 037 | Slap Bass2 | Slap It! | GothamBass | Pop'n Bass | Slappers |
| 038 | Syn.Bass 1 | Funky Acid | Filter Wow | LatelyBass | Dee X Bass |
| 039 | Syn.Bass 2 | Fat Mini | Hypno Bass | TranceBass | PsychoBass |

### Strings (040–047)

| # | GM (0) | User (1) | Preset 2 (2) | Preset 3 (3) | Preset 4 (4) |
|--:|---|---|---|---|---|
| 040 | Violin | Stradivari | DiamondVln | Violiner | Fusion Vln |
| 041 | Viola | BiViola | Solo Viola | C-Bs&Viola | Fusion Vla |
| 042 | Cello | Cello Ros | DarkrCello | CelloRound | Dark Cello |
| 043 | Contrabass | FusinKntra | Bass&Cello | Celli | Kontrabass |
| 044 | TremoloStr | TremEnsemb | TrembleStr | StringAura | J Str Trem |
| 045 | Pizzicato | Pizzicati | PizzViolin | Pizz Pluck | SpacePluck |
| 046 | Harp | HeavenHarp | Soft Harp | Harpitz | Waterfalls |
| 047 | Timpani | TimPanic | Big O Timp | TrashyTimp | Strng&Timp |

### Ensemble (048–055)

| # | GM (0) | User (1) | Preset 2 (2) | Preset 3 (3) | Preset 4 (4) |
|--:|---|---|---|---|---|
| 048 | Strings | HugeString | ArcoStrngs | Concerto | OctoString |
| 049 | Slo String | SloOctStrg | String Pad | Slow 8s | Orchestrar |
| 050 | SynString1 | SE Motion | J Strings | AnaStrEnsm | VintageStr |
| 051 | SynString2 | Xpando Pad | Ana String | Syn Arcos | Obersphere |
| 052 | Choir Aahs | CloudChoir | Ooh LaLa | MorphChoir | Afterglow |
| 053 | Ooh Vox | VelOoz&Aaz | Chiff Oohs | Hen-Ya | Sunsrizer |
| 054 | Synvox | Nice Voice | Velocivox | Air Choir | GlideVoxMW |
| 055 | Orchst.Hit | Hitz Peak | Mortal Hit | Danz Hitz | Deja Hitz |

### Brass (056–063)

| # | GM (0) | User (1) | Preset 2 (2) | Preset 3 (3) | Preset 4 (4) |
|--:|---|---|---|---|---|
| 056 | Trumpet | UseMWandAT | TrumpetLyt | SynTrumpet | TrumpletMW |
| 057 | Trombone | Bone Tone | Solo Tromb | Tromb Ens | Fanfare |
| 058 | Tuba | Hard Tuba | Big Tuba | Round Tuba | ClsclHorns |
| 059 | Mute Trump | BriteMute | Jazz Mute | Orch Mutes | Mute&Flute |
| 060 | FrenchHorn | FlugelSolo | FHrn Ens | HornExpans | Documentar |
| 061 | Brass | EssexBrass | BrassTouch | Stab Brass | BriteBrass |
| 062 | Syn.Brass1 | Matrix Brs | Sfz Brass | Mighty5ths | BeBopHorns |
| 063 | Syn.Brass2 | El Brasso | FiltrSynth | Ooh Horns | Kick Brass |

### Reed (064–071)

| # | GM (0) | User (1) | Preset 2 (2) | Preset 3 (3) | Preset 4 (4) |
|--:|---|---|---|---|---|
| 064 | SopranoSax | G. Soprano | Saxette | MonoSoprno | Pastorale |
| 065 | Alto Sax | Sax Touch | Spit Alto | Alto Swing | Sexy Sax |
| 066 | Tenor Sax | Tenor Solo | BreathySax | Throat Sax | Sax on Wax |
| 067 | Bari Sax | SaxSection | Big O Bari | Sam's Sax | ThoseSaxes |
| 068 | Oboe | Oboe Reed | Oboe Blow | Ebony Oboe | Wind Ensmb |
| 069 | EnglishHrn | S.Eng.Horn | London Fog | StatelyOrc | Orchestr8 |
| 070 | Bassoon | DrkBassoon | SoloBasoon | Fhorn&Bssn | Oddsemble |
| 071 | Clarinet | Clarinet O | 1stClarnet | Dixi Brass | Cartoonin' |

### Pipe (072–079)

| # | GM (0) | User (1) | Preset 2 (2) | Preset 3 (3) | Preset 4 (4) |
|--:|---|---|---|---|---|
| 072 | Piccolo | Pickle O | Pick-a-low | Pic-a-fife | TronFlutes |
| 073 | Flute | LyricFlute | MoodyFlute | Deep Flute | ChiffFlute |
| 074 | Recorder | SprnoRcrdr | SftRecordr | SingleFlut | The Bosun |
| 075 | Pan Flute | PanPeople | Panz Flute | Hard Pipes | Shamanixst |
| 076 | BottleBlow | PlugdBottl | BottledAir | Bottle Pad | BlowDeTune |
| 077 | Shakuhachi | Octohachi | Phat Pipe | NativeFlut | FluteEnsem |
| 078 | Whistle | KeyWhistlr | Whistral | Wistelaan | Nautical |
| 079 | Ocarina | Rugrats | OcariNoir | Slippery | PanBristle |

### Synth Lead (080–087)

| # | GM (0) | User (1) | Preset 2 (2) | Preset 3 (3) | Preset 4 (4) |
|--:|---|---|---|---|---|
| 080 | SquareWave | Porta Lead | SquareLead | RaveSqrQS6 | 3oh3 SqrMW |
| 081 | Saw Wave | Quadratix | Saw Lead | RaveSawQS6 | 3oh3 SawMW |
| 082 | Calliope | Triangular | SynCalliop | Syn Circus | Digidee |
| 083 | Chiffer Ld | Rez Blastz | Zip Lead | Analogist | ChiffLeads |
| 084 | Charang | Screamer | Boiled Gtr | Haurang | FatAnaLead |
| 085 | Solo Voice | ShineOn... | Solo Vocks | Voice Lead | Applewine |
| 086 | 5th Saw | Saw 5X | King 5th | Brassy 5th | I Saw 5 |
| 087 | Bass&Lead | ClassicSqr | Low&High | Led Bass | Blacksmith |

### Synth Pad (088–095)

| # | GM (0) | User (1) | Preset 2 (2) | Preset 3 (3) | Preset 4 (4) |
|--:|---|---|---|---|---|
| 088 | Fantasia | Bell Pad | Marimpanad | Pluck Pad | Gothos |
| 089 | Warm Pad | Atlantis | Cool Pad | Swell Pad | Holla Pad |
| 090 | Polysynth | PolySyn | Poly-gone | Air Pad MW | Digiculver |
| 091 | SpaceVoice | SpaceVocks | BrezyChoir | Voice Bell | Scarlette |
| 092 | BowedGlass | Ice-O-tope | Bo's Glass | Tranzcndnt | SftScience |
| 093 | Metal Pad | Metallic | Tal Pad | BladeRunnr | Illusions |
| 094 | Halo Pad | HiloVolt | Holo Pad | Hilo Pad | VoltagePad |
| 095 | Sweep Pad | 7th Wave | Sweep Up | E Sweep | Ascent |

### Synth FX (096–103)

| # | GM (0) | User (1) | Preset 2 (2) | Preset 3 (3) | Preset 4 (4) |
|--:|---|---|---|---|---|
| 096 | Ice Rain | HeavenCent | Comet Rain | Acid Rain | Dew Drops |
| 097 | Soundtrack | Bali Hai | SuperScore | Legendary | Ocean Mood |
| 098 | Crystal | Crystallin | Aluminum | Strukit | Helical |
| 099 | Atmosphere | Tropospher | Atmosfear | Strafing | PhaseArray |
| 100 | Brightness | CafeDelMar | Brighter | GldnFleece | Romulux |
| 101 | Goblins | Gobbling | Elves | Nitrous | Hyperspace |
| 102 | Echo Drops | Echoes | EffectRetn | HighGlissz | Intertwine |
| 103 | Star Theme | Moonling | Sci-Fi | Moon&Womb | Spaceport |

### Ethnic (104–111)

| # | GM (0) | User (1) | Preset 2 (2) | Preset 3 (3) | Preset 4 (4) |
|--:|---|---|---|---|---|
| 104 | Sitar | WorldSitar | Sitar Buzz | Atmossitar | Raga No.1 |
| 105 | Banjo | Ethnoba | BanjoDual | DownYonder | Ethnicity |
| 106 | Shamisen | MelloShami | Shamijo | Shame-isen | Toru |
| 107 | Koto | Amakudari | Soft Koto | Kotobaba | Ethnotal |
| 108 | Kalimba | Kalimbex | Kalidark | Chasers | Kalimpanad |
| 109 | Bagpipe | The Moors | Bag O'pipe | Bagboe | Bags |
| 110 | Fiddle | Git Fiddle | Violin Ace | ZizzViolin | Violin Orc |
| 111 | Shanai | Shanaye | Shan'sReed | Tom'sO'Man | 1001Nights |

### Drums / Perc (112–119)

| # | GM (0) | User (1) | Preset 2 (2) | Preset 3 (3) | Preset 4 (4) |
|--:|---|---|---|---|---|
| 112 | TinkleBell | D4 Pwr Kit | Real Rock | Ambi Rock | Cirque |
| 113 | Agogo | Pop Up Kit | Dry70'sKit | Trashy Kit | Agogo Gone |
| 114 | Steel Drum | UrbanBliss | Hipp Kitt | T Funk Era | SteelDrama |
| 115 | Wood Block | Straight 8 | Bomb Kit | PiqueDrumz | WdBlockage |
| 116 | Taiko | 9 Time | HardcorKit | Harlem Tek | KodoDrummr |
| 117 | Melo Tom | Techno Kit | Club Kit | Big Dance | MelodicTom |
| 118 | Synth Drum | IsmellFUNK | Old School | 15ips Kit | WhistlDrum |
| 119 | ReverseCym | Gruvy Lube | Destructo! | Asylum Kit | ReverseRap |

### Effects (120–127)

| # | GM (0) | User (1) | Preset 2 (2) | Preset 3 (3) | Preset 4 (4) |
|--:|---|---|---|---|---|
| 120 | Gt.FretNze | Psyclotron | Swamp Goop | Bonk | Gutteral |
| 121 | Breath Nze | UB Riddim | Dub Lander | Go On | ThatCoyote |
| 122 | Seashore | Crenshaw | Sweat | Donut Shop | T I M E ! |
| 123 | Bird | Razor Edge | Nodes | Abuser | Heartbeat |
| 124 | Telephone | Get On | Git Along | Dino + Dog | Laboratory |
| 125 | Helicopter | Herkin It | Danger Tip | Fried | Nostromo |
| 126 | Applause | Caravania | Nanites | Insectagon | 1stContact |
| 127 | Gun Shot | *silent prog* | Cricketry | TseTse Fly | Final Dawn |

## ROM sound families

The programs are built from 462 sample families in ROM, listed in the manual
by family in program mode (pp. 35–38) and again in drum mode (pp. 39–41). The
family headings, as the manual groups them:

- **Program mode**: Piano, Chromatic, Organ, Guitar, Bass, String (StringEnsm,
  AttkString, Violin, Cello, Pizz Sectn, Pizz Split, Strng/Pizz, Pizz/Strng,
  Harp), Brass (Brass Ensm, Trumpet, MuteTrumpt, Trombone, FrenchHorn,
  Bari Horn, Tuba), Woodwind (Bassoon, Oboe, EnglishHrn, Clarinet, the saxes,
  Flute, FluteWave, Shakuhachi, the pan pipes, BottleBlow, Wood Chiff), Synth,
  Wave, Noise, Voice, Ethnic, Drums (kits: StandrdKit, Rock Kit 1, Rock Kit 2,
  Dance Kit, Brush Kit, ElctricKit, Tek Kit, Rap Kit 1, Rap Kit 2, IndustrlKt,
  Metal Kit, HvyMtl Kit, VeloMtlKit, Wild Kit, Octave Kit, OrchstraKt), then
  the single drum voices (kicks, snares, toms, hats, cymbals), Percussion
  (Timpani, Timp/Null, Agogo, Bongo, Cabasa, Castanet, Chimes, Claps, Clave,
  Conga, Cowbell, FingerSnap, Guiro, Maracas, SambaWhstl, Shaker, Sleighbell,
  Tabla, Taiko, TalkngDrum, Tambourine, Timbale, Triangle, Vibrasmack,
  Wood Block), Sound FX, Rhythm (loops).
- **Drum mode**: Kick, Snare, Tom, Cymbal, Percussion, Sound FX, Wave, Rhythm —
  the same material split per drum pad.

The full family-by-family list is on those manual pages; it is not reproduced
here.
