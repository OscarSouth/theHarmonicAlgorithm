# Algorithmic Orchestration

Scoring music for a virtual orchestra via TidalCycles live coding. This document is the operational guide for the **Algorithmic Orchestration** principle — the paradigm of abstracting musical elements into three concerns: harmony/contexts (the Harmonic Algorithm), form/constants (the Spectral Narrative), and interfaces/timbres (instrument functions, voice lines, articulations).

## Signal Chain

Code → TidalCycles → SuperDirt → MIDI → the declared rig → orchestral music

## Rigs

Every instrument function reads its channel and range from a `Rig`
(`Harmonic.Interface.Tidal.Orchestra`), and the boot file declares which module
is on the port. The tracked default is the JV-1010; a machine with another module
keeps a one-line, gitignored `live/local/rig.hs`, which the boot file reads when
it is there:

```haskell
rig = jv1010      -- live/BootTidal.hs: Roland JV-1010, the default
rig = nanoSynth   -- live/local/rig.hs in the studio: Alesis NanoSynth
```

The plain names (`flute`, `violin1`, `pizz` …) are bound to that rig in
`live/BootTidal.hs`, so a performance file never names a channel and the same
block plays on either module. A rig is channels only: an instrument's range is
its own and lives with the instrument, whatever module plays it. `setupRig` sends a module its cold configuration
where it has one (the NanoSynth); `rigName rig` is printed at boot.

### JV-1010 (16 MIDI Channels)

| Channel | Instrument      | Type              |
|---------|----------------|-------------------|
| 1       | Flute          | Wind              |
| 2       | Oboe           | Wind              |
| 3       | Clarinet       | Wind              |
| 4       | Bassoon        | Wind              |
| 5       | Horn           | Brass             |
| 6       | Trombone/Bass  | Brass             |
| 7       | Harp           | Plucked           |
| 8       | Timpani        | Pitched perc      |
| 9       | Bass Drum      | Unpitched perc    |
| 10      | subKick        | MPC sub/kick      |
| 11      | Tam-tam        | Unpitched perc    |
| 12      | Strings pizz   | String artic      |
| 13      | Strings spicc  | String artic      |
| 14      | Strings marc   | String artic      |
| 15      | Strings legg   | String artic      |
| 16      | Strings arco   | String artic      |

Strings are laid out by technique: every section shares a channel per
articulation (`pizz` 12, `spicc` 13, `marc` 14, `legg` 15, `arco` 16).
Patches are a Performance stored on the module. Definitions: `jv1010` in
`Harmonic.Interface.Tidal.Devices.JV1010`.

### NanoSynth (16 MIDI Channels)

| Channel | Instrument          | Patch (program / bank) |
|---------|---------------------|------------------------|
| 1       | Flute               | 74 / Preset 3  SingleFlut |
| 2       | Oboe                | 68 / Preset 3  Ebony Oboe |
| 3       | Bassoon             | 70 / Preset 2  SoloBasoon |
| 4       | Horn                | 60 / User  FlugelSolo |
| 5       | Trombone, bass trombone | 57 / User  Bone Tone |
| 6       | Timpani, with tuba  | 60 / Preset 4  Documentar |
| 7       | Strings pizzicato   | 45 / Preset 3  Pizz Pluck |
| 8       | Orchestra hit (bass drum) | 55 / Preset 3  Danz Hitz |
| 9       | Harp (continuo)     | 46 / Preset 3  Harpitz |
| 10      | service channel (MPC) | 127 / User; `nanoKit` for the module's kit |
| 11      | Clarinet            | 71 / Preset 2  1stClarnet |
| 12      | silent              | 127 / User |
| 13      | Violin 1            | 49 / GM  Slo String |
| 14      | Violin 2            | 49 / GM  Slo String |
| 15      | Cello               | 43 / Preset 3  Celli |
| 16      | Double bass, with viola | 41 / User  BiViola |

One section per channel; `pizz` routes any string to 7 and the other
articulation tags are identity; the tam-tam has no voice. The module is
configured from code — `setupRig` once after power-up — and the full
reference (ranges, mix, the 640-program list, how the module is addressed)
is [NANOSYNTH.md](NANOSYNTH.md). Definitions: `nanoSynth` in
`Harmonic.Interface.Tidal.Devices.NanoSynth`.

The studio rig (soft synths, DFAM/P-6, M32/S-1, MPC, drum machines) has its own map
in `documents/LIVE_ENVIRONMENT.md`. No two of these programs are ever live together.

### The continuo voice

The orchestra is a fixed, deliberate configuration — it is not extended.
The one permitted variation is the continuo channel (`rigContinuo`: JV-1010
**7**, NanoSynth **9**), which may be voiced as any pitched polyphonic
plucked, keyboard, mallet or choral colour. Set it **once per movement** in
the launcher; never hot-swap it mid-flow.

```tidal
mapM_ id [hush, setbpm tempo
  , setContinuo harpsichordV       -- Baroque continuo
  , strg f k  $ d 0.8
  ]
```

A continuo voice carries a full bank address `(msb, lsb, program)`, so any card or
internal set is reachable. Quote SR-JV80-02 Orchestral patches straight off
Roland's 001–255 listing with `orch` (it spans two 128-patch sub-banks and the
arithmetic is handled); reach the internal sets with `presetA` / `presetB`.

| Voice | Patch | Typical use |
|---|---|---|
| `harpV` | Orchestral 186 Harp 1 | default — Orpheus's lyre |
| `harpPluckedV` | Orchestral 188 Plucked Harp | drier attack |
| `harpsichordV` | Orchestral 196 Harpsichord1 | Baroque continuo (Bach, Vivaldi) |
| `pianoV` | Orchestral 192 ClasclPiano1 | — |
| `celestaV` | Orchestral 200 Celesta 1 | Venus, Neptune |
| `glockenV` | Orchestral 211 Glocken 1 | bright mallet colour |
| `tubularV` | Orchestral 216 TubulaBells1 | bell weight |
| `choirV` | Orchestral 227 Choir 1 | Neptune's wordless chorus |
| `guitarV` | Orchestral 185 Classical Gt | — |
| `organV` | internal `presetA 18` | organ weight (Mars) |

Definitions live in `Harmonic.Interface.Tidal.Devices.JV1010`. Two constants
need confirming against the device the first time this is used: `orchLSB` (the
expansion card's bank) and the `organV` patch number.

On the NanoSynth a voice is `nanoVoice program bank` (the bank in the msb
slot; the module reads CC0 only): `nanoHarpV` (Harpitz) and `nanoHarpsichordV`
(8'4'Harpsi). The JV voices are JV addresses, as the drum names are JV notes —
use the voices of the rig you declared.

## Performance Architecture

Voice leading (cyclic DP) is expensive. With 16+ stacked `arrange` calls (full orchestral mode), naive per-frame recomputation causes TidalCycles "skip" messages.

The fix is a construction-time voicing cache: `arrange` and `arrange'` query `kProg k` once when the pattern is registered, pre-compute voicings for all unique progressions (~2–3 in a typical form), and store them in an association list. Per-frame lambdas do O(1) lookup instead of running the DP solver. Result: ~800 voice leading solves/second → 2–3 (once at construction).

Frame timespan is set to `1/30` (~33ms frames). At `oLatency = 0.15` (150ms SuperDirt latency), timing resolution is dominated by output latency — 33ms frames give more than sufficient musical precision.

## Two Separate Concerns

1. **`(-9, 9)` register** — pattern index trimming. A compositional parameter passed to `arrange`. Controls how many scale degrees above/below the input patterns can reach. This is the composer's domain.

2. **`clip`** — MIDI range enforcement. Filters events whose MIDI note falls outside the physical instrument range. Internal to each instrument function — invisible to the composer.

These are independent: `(-9, 9)` controls compositional range, `clip` prevents impossible notes from reaching hardware.

## Parameter Convention: `d` Last

All orchestral blocks (and updated existing blocks) take `d` as the last parameter, enabling the `$ d` syntax:

```tidal
,wind f k         $ d 0.9
,tutti arco f k   $ d 0.9
,k909 f k         $ d 0.4
```

Where `d = (* 1)` in the launcher — a dynamics multiplier applied via `$`.

## Unified Block Template

Every block (section or blend) has the same two-part structure:

```tidal
{name} f k d = p "{name}" $ do
  let vl = voiceLines {_vl = "~"
        -- , soprano = "3"
        -- , alto    = "1"
        -- , tenor   = "2"
        -- , bass    = "0"
        }
  f
    $ stack [silence
        , instrument Layer (ki_range) k vl voiceFunc Voice
        , ...
    ] |* vel d
```

1. **Voice declaration** (`vl`): `voiceLines` with optional overrides
2. **Instrument stack**: each line is an instrument with layer (`T`/`S`/`M`, or a combination `TS`/`TM`/`SM`/`TSM`/`PT`), kinetics range, IK context, voice lines, voicing paradigm, voice assignment

## Instrument Functions

### Pitched (arrange + clip + octave)

Each instrument applies `instrument` to its own range (Tidal note space) and
the channel it reads from the rig:

```haskell
instrument :: (Int, Int) -> Int -> Instrument
flute, oboe, clarinet, … :: Rig -> Instrument
flute = instrument (-12, 26) . rigFlute   -- C3–D6 (MIDI 48–86)
```

Pipeline: `arrange` → `# ch` → `|+ oct` → `clip`. The boot file binds the
rig (`flute = O.flute rig`), so in a session `flute T (0,1) k vl flow Soprano`
is the whole call.

### Unpitched (struct-based)

```haskell
bassdrum, tamtam :: Rig -> Pattern Bool -> ControlPattern
-- JV-1010: MIDI 36 on ch 9 and MIDI 31 on ch 11; a rig without the voice is silent
```

## Instrument Catalogue

### Pitched Instruments

| Instrument | Ch | Tidal Range | MIDI Range | Pitch Range | Section |
|---|---|---|---|---|---|
| Flute | 1 | (-12, 26) | 48–86 | C3–D6 | Wind |
| Oboe | 2 | (-2, 33) | 58–93 | Bb3–A6 | Wind |
| Clarinet | 3 | (-22, 34) | 38–94 | D2–Bb6 | Wind |
| Bassoon | 4 | (-28, 15) | 32–75 | Bb1–Eb5 | Wind |
| Horn | 5 | (-29, 17) | 31–77 | B1–F5 | Brass |
| Trombone | 6 | (-28, 17) | 32–77 | Bb1–F5 | Brass |
| Bass Trombone | 6 | (-39, -5) | 21–55 | A0–G3 | Brass |
| Harp | 7 | (-29, 42) | 31–102 | B1–F#7 | Plucked |
| Timpani | 8 | (-22, 0) | 38–60 | D2–C4 | Pitched perc |
| Violin 1 | 16 | (-5, 45) | 55–105 | G3–A7 | Strings (arco) |
| Violin 2 | 16 | (-5, 45) | 55–105 | G3–A7 | Strings (arco) |
| Viola | 16 | (-12, 28) | 48–88 | C3–E6 | Strings (arco) |
| Cello | 16 | (-24, 24) | 36–84 | C2–C6 | Strings (arco) |
| Contrabass | 16 | (-36, 0) | 24–60 | C1–C4 | Strings (arco) |

Tidal note 0 = MIDI 60 = middle C. Ranges are enforced by `clip` inside each instrument function.

### Range review (pending — a practical pass at the instrument)

The numeric bounds above are tuned to the range limits of the **actual
JV1010 sampler patches**, not textbook instrument ranges — the numbers are
authoritative and must not be "corrected" to the note names. The 2026-08-25
sweep found the two columns disagreeing in four places, where the coded MIDI
floor sits below the named pitch:

| Instrument | Coded floor | Named floor | Gap |
|---|---|---|---|
| Bassoon | MIDI 32 (Ab1) | Bb1 (34) | 2 semitones |
| Horn | MIDI 31 (G1) | B1 (35) | 4 semitones |
| Trombone | MIDI 32 (Ab1) | Bb1 (34) | 2 semitones |
| Harp | MIDI 31 (G1) | B1 (35) | 4 semitones |

Separately, the flute (low C3, a physical flute's floor is C4) and clarinet
(low D2, a Bb clarinet sounds to D3) lower bounds sit an octave below the
acoustic instruments — plausible as deliberate patch headroom, unverified.

The sweep briefly aligned the four numbers to the names before the JV1010
provenance was clarified; the values were restored untouched. **Action:** a
practical per-instrument pass on the JV1010, sounding each patch at its
extremes, then setting number AND name precisely in all three copies of this
table (`Orchestra.hs`, this file, `live/docs/ORCHESTRAL_CATALOGUE.tidal`).

### Unpitched Percussion

| Instrument | Ch | MIDI Note | Usage |
|---|---|---|---|
| Bass Drum | 9 | 36 (C2) | `bassdrum pat` |
| Tam-tam | 11 | 31 (G1) | `tamtam pat` |

The studio rig's MPC instruments — `subKick` (ch 10), `hmnx` (ch 11), `slce` (ch 12) — are
not JV-1010 voices; they are documented in [LIVE_ENVIRONMENT.md](LIVE_ENVIRONMENT.md).

### String Articulations

Postfix tags. On the JV-1010 strings default to arco (ch 16) and a tag
re-routes the line; on the NanoSynth only `pizz` has a destination and the
other tags leave the line on its section's channel:

| Articulation | Alias | JV-1010 ch | NanoSynth | Usage |
|---|---|---|---|---|
| Pizzicato | `pizz` | 12 | ch 7 | `# pizz` or `tutti pizz f k $ d 0.7` |
| Spiccato | `spicc` | 13 | identity | `# spicc` |
| Marcato | `marc` | 14 | identity | `# marc` |
| Legato | `legg` | 15 | identity | `# legg` |
| Arco | `arco` | 16 | identity | Default |

### JV-1010 Pan Positions

| Ch | Instrument | Pan |
|---|---|---|
| 1 | Flute | -10 |
| 2 | Oboe | 10 |
| 3 | Clarinet | -15 |
| 4 | Bassoon | 15 |
| 5 | Horns | -22 |
| 6 | Trombones | 18 |
| 7 | Harp | -25 |
| 8 | Timpani | -18 |
| 9 | Bass drum | -6 |
| 10 | subKick/MPC | 0 |
| 11 | Tam-tam | 22 |
| 12–16 | Strings | 0 |

### Target Ensemble

2 flutes (doubling piccolo), 2 oboes (doubling cor anglais), 2 clarinets, 2 bassoons, 2 horns, 2 trombones (doubling bass trombone), 1 timpanist, 1 percussionist, 1 harp, 8 vn1, 6 vn2, 4 va, 3 vc, 2 cb

For interactive range tests, see [`live/docs/ORCHESTRAL_CATALOGUE.tidal`](../live/docs/ORCHESTRAL_CATALOGUE.tidal).

## Voice Line System

### SATB defaults

```haskell
voiceLines = VoiceLines
  { _vl  = "~"     -- structural placeholder
  , soprano = "3"     -- soprano: root 8va
  , alto    = "1"     -- alto: 2nd degree
  , tenor   = "2"     -- tenor: 3rd degree
  , bass    = "0"     -- bass: root
  }
```

### Overriding voices

```tidal
let vl = voiceLines {_vl = "~"
      , soprano = "[3 2]/4"
      -- , alto    = "1"      -- commented = use default
      -- , tenor   = "2"
      , bass    = "[0 1]/4"
      }
```

The `_vl` field enables comma-leading syntax (same principle as `stack [silence, ...]`).

## Voice Octave Variants

Standard Italian musical terminology for register shifts:

| Suffix | Offset | Meaning           |
|--------|--------|-------------------|
| (none) | 0      | Loco (as written) |
| `8va`  | +1     | Octave up         |
| `15va` | +2     | Two octaves up    |
| `8vb`  | -1     | Octave down       |
| `15vb` | -2     | Two octaves down  |

Applied to any voice: `Soprano8va`, `Tenor8vb`, `Bass15vb`, etc.

The octave shift happens inside the instrument function (via `|+ oct`) before `clip` filters. No `|- oct 1` noise in templates.

## Divisi

When a section (or one instrument+voice) splits into several independent desks,
each desk is quieter than the undivided line. Divisi handles the split and the
equal-power loudness compensation (`1/√n`), in three composable forms.

### Primed voice fields

Each SATB voice has three divisi tiers held in one `voiceLines` record — base,
`'`, and `''` — all ordinary degree patterns (same syntax as the base fields).
Unset tiers default one/two degrees above the base, so an undeclared `divisi 3`
already voices a chord:

```haskell
-- defaults: soprano 3/4/5 · alto 1/2/3 · tenor 2/3/4 · bass 0/1/2
let vl = voiceLines {_vl = "~"
      , soprano = "[3 2 1 0]/4", soprano' = "[0,1,2,3]"   -- desk 2 = cluster
      , bass    = "0",           bass'    = "[0,1,2,3]"
      }
```

### Primed voice constructors

Any `Voice` can be primed to select its desk, and the prime composes with the
octave suffix — octave and divisi tier are orthogonal:

| Voice   | Reads field | Desk |
|---------|-------------|------|
| `Soprano`   | `soprano`   | 1 (base) |
| `Soprano'`  | `soprano'`  | 2 |
| `Soprano''` | `soprano''` | 3 |
| `Bass8vb'`  | `bass'`     | 2, octave down |

### `divisi N` — auto-wrap

Prefix an instrument (space form, not `$`) to stack `N` desks reading successive
tiers, scaled `1/√N`. Octave rides the `Voice` arg as usual:

```tidal
, divisi 3 violin1 T (0, 1) k vl grid Soprano       -- Soprano / Soprano' / Soprano''
, divisi 2 contrabass T (0.9, 1) k vl grid Bass8vb  -- Bass8vb / Bass8vb'
, violin1 T (0, 1) k vl grid Soprano                -- drop `divisi N` → plain single voice
```

### `# divisi2` / `# divisi3` — volume tags

For hand-built desks that differ in articulation or entry (not uniform divisi),
duplicate the line and tag each with the matching scaler. Postfix like the
articulation aliases, composes with `# pizz`:

```tidal
, violin1 T (0, 1)   k vl flow Soprano  # divisi2   -- desk 1
, violin1 T (0.9, 1) k vl grid Soprano' # divisi2   -- desk 2 (primed tier)
```

## String Articulations

Each tag is a field of the rig — a channel where the module routes by
technique, `noArt` (identity under `#`) where it does not:

```haskell
pizz, spicc, marc, legg, arco :: Rig -> ControlPattern
-- jv1010:    ch 12, 13, 14, 15, 16
-- nanoSynth: ch 7, then noArt for the rest
```

Override with `#`:
```tidal
, violin1 T (0,1) k vl flow Soprano # pizz
```

For `tutti`, the articulation is a parameter:
```tidal
tutti arco f k $ d 0.9
tutti pizz f k $ d 0.7
```

## Sections

| Name   | Instruments                              |
|--------|------------------------------------------|
| `wind` | flute, oboe, clarinet, bassoon           |
| `brss` | horn, trombone, basstrom                 |
| `strg` | violin1, violin2, viola, cello, contrabass |
| `perc` | timpani, harp, bassdrum, tamtam          |

## Blends

| Name        | Character                        | Instruments                          |
|-------------|----------------------------------|--------------------------------------|
| `chalumeau` | Dark warmth                      | clarinet, bassoon, horn              |
| `pastorale` | Mid-register colour              | flute, oboe, clarinet                |
| `brillante` | Bright top (flute 8va at high k) | flute 8va, flute, oboe, clarinet     |
| `maestoso`  | Full winds + brass (climactic)   | wind + horn, trombone                |
| `tutti`     | Full orchestra                   | strings + winds + brass + timpani    |

## Score mode (literal / linear)

The kinetics workflow voices a progression under a form: the composer decides *what kind* of
material plays at each level of the arc and the instruments read their notes from the chords.
Score mode is the other way of writing — note for note, on a bar timeline — in the same
blocks, with the same form, rig, ranges, articulation tags and dynamics vocabulary. The
progression is the same slot: a key written as a one-bar context, or anything `gen`, `genJ`,
`recall` or `fromChords` produced.

| Piece | What it does |
|---|---|
| `inKey D "major"` | a one-bar `ProgressionContext`: the tonic triad on T (the mode's degrees 1 3 5), the key's pentatonic on S (its parent major key's, rooted on the tonic where it belongs to it — A minor gives A C D E G), the mode on M. Names: `major`/`ionian`, `minor`/`aeolian`, the church modes, `melodic minor`, `harmonic minor`, `harmonic major`, any `ModeQuality` constructor, `chromatic` (all twelve, a degree is a semitone). An unknown name is an error at declaration. Round-trips through `save`. |
| `flute' M 1 "p" k degrees` | the literal form of each instrument: a layer, an octave, a dynamic mark (or a pattern of marks, sampled at every onset) and a degree pattern read against that layer of the chord current at the onset — the selection rule of `arrange`. Degree 0 is the bar's root at the lattice's bass anchor (the root in [-12,-1]), so `flute T` and `flute' T` agree on where 0 sits; `n` indexes the bar's closed position above it and wraps by octave. The octave pattern lands before the clip: `1` the melodic register, `0` the bass, `"[1,2]"` two flutes, `"[[1] [~]]/16"` drops the notes under its rest. Same range, channel and tags as the plain form (`# pizz`); amp = mark × the form's dynamic, as in `arrange`. |
| `bars b1 b2 $ pat` | a block on score bar numbers, 1-indexed, end inclusive, four cycles per bar; a `seqP` triple, so each block's pattern starts from its own cycle 0 at the block's first beat |
| `score pre blocks` | `seqP` behind `pre` bars of pre-roll; `preroll pre k` delays the form the same way so `display k` counts score bars |
| `dyn`, `ramp` | the mark table (`ffff` 1.0 … `p` 0.40 … `pppp` 0.25); a 64-step line across one cycle (`slow 16 $ ramp 1 1.2` is a four-bar swell on one line; the form's nodes are the arc) |

**The format.** A piece is one statement: `tempo` is the only tempo number outside the list;
the movement list opens with the tempo blocks (`bars a b $ bpm t` — the base, then every change,
in bar order), then every stack written out under its bar range; a block that recurs is
`let`-bound inside the piece's `do` (it sees `k`); the launcher never calls `setbpm` — the stream
owns the clock, and `hush` clears any stale tempo pattern; `tempo` also drives `iK`, so the
display clock agrees with the base tempo.

```tidal
tempo = 88
pre   = 3
key   = inKey D "major"
form  = [rh 1 1 0.5 key, rh 3 1 0.6 key, rh 9 1 0.78 key, rh 23 1 0.5 key]   -- the arc, in score bars

andante f k d = p "score" $ do
  let ostinato = stack [silence                       -- recurs at [1] and [3]
        ,clarinet' M 1 "p" k "[[-2 1 -1 2 -2 1 -1 ~] [~ [-2 1 -1 2]] [[-2 1 -1 ~] ~]]/12"
        ]
  f $ score pre
    [bars 1 23 $ bpm tempo                            -- tempo first
    ,bars 1 9  $ ostinato
    ,bars 1 9  $ stack [silence
       ,viola'      M 1 "mp" k "[~ -5 ~ -7]/4" # pizz
       ,cello'      M 0 "p"  k "[-4 . 2@3 ~ . -6 . 0@3 ~]/4" # pizz
       ,contrabass' M 0 "p"  k "[-4 ~ -6 ~]/4" # pizz
       ]
    ,bars 3 9  $ stack [silence
       ,flute'   M "[1,2]" "p"  k "[5 [2 4 . 5] [4 3 2 4 . 5] [4 5] ~!3]/28"
       ,violin1' M 1       "mp" k "[12 [9 11 . 12] [11 10 9 11 . 12] [11 12] ~!3]/28"
       ]
    ,bars 17 22 $ ostinato
    ] |* vel d

do
  let f = id; d = (* 1)
      k = preroll pre $ iK tempo form (rep key 1)
  mapM_ id [hush, resetCycles, display k, andante f k $ d 0.8]
```

One cycle is a beat, so a three-bar ostinato is `"…/12"` and a seven-bar phrase `"…/28"`; blocks
run on their own clock, so the ostinato begins on its first bar wherever it is placed and a
`"<~ -2!2>"` alternation counts its passes from the entry. Ties over bar lines are durations
(`"[5@7 ~ 5@4 ~@2 5@22]/18"` holds B through a coda of eighteen beats), not sustain-pedal
messages. A kinetics line stacks in the same block (`,violin1 T (0,1) k vl flow Soprano`); a
literal line is gated like any other (`ki (0.5,1) k $ …`). Swap `key` for a generated or recalled
progression and the same lines follow its bars: T reads each chord, S its pentatonic, M its key
area.

The 2021 concert files map onto this line for line:

| 2021 | now |
|---|---|
| `rh a b` (3-bar pre-roll baked in), `rh 0 0` the pickup | `bars a b`, `bars 0 0`, the pre-roll in `score pre` / `preroll pre` |
| `k = mode (5-7) $ F \\ mMaj` (relative minor) | `inKey D "minor"` |
| `instr "p" +| note (k "…")` | `instr' M 1 "p" k "…"` |
| `|- 12`, `|+ 12`, `|- "[24,12]"`, `|+ "[[12] [~]]/4"` (semitones) | the octave argument: `0`, `2`, `"[-1,0]"`, `"[[1] [~]]/16"` |
| `k ("…" |+ "[0,9]")`, `k $ "…" |+ 12` (degrees) | unchanged, on the degree pattern (`|+` keeps the line's structure) |
| `transpose st key pat` | `… + st` on the degrees (the clarinets were sounding degrees already) |
| `slow 4 $ stack [… "…/3" …]` | no `slow 4`; divisors ×4 (`/3` → `/12`) |
| `lfo saw lo hi`, `cat [1.1, lfo …]` | `ramp lo hi`, or the form's nodes |
| `meter t m s`, mid-piece tempi, `setcps` | `bars a b $ bpm t` blocks at the head of the list (a 6/4 piece: `tempo = 66 * 4 / 6`, four cycles a bar); never `setbpm` in the launcher |
| `cc 11 … #hn` (expression per instrument) | `cc 11 … # ch (rigHorn rig)` |
| `ped … #vc` for a tie | a duration |
| `moog`, `cc 1 … #m32` (studio synths, ch 12) | kept on `# ch 12` under a `studio synth` comment |

The worked port is the author's Stravinsky Suite No. 1 Andante (`live/local/Suites/Stravinsky/`,
with its bar-by-bar reconciliation against the Chester score); the tracked example is
`live/examples/04-SCORE.tidal`, the opening of a 2021 concert piece.

## Motifs

Motifs are the recurring material a piece is built from. They are **plain patterns**, not a new
type — a `Pattern Bool` **rhythm** (a `struct` gate) and a `Pattern Int` **contour** (voicing-index
degrees). A contour auto-tracks the harmony: each degree indexes the active bar's voicing
(`sc !! (n mod len)`, octave `n div len`), so `"[3 2 1 0]/4"` is *relative to the chord*, re-realised
every bar — restate an idea at a new pitch by transposing the harmony under a fixed contour.

### Motivic development — one tool each

The classic developments are just TidalCycles; only inversion and combination are added:

| Development    | Tool                | Example                     |
|----------------|---------------------|-----------------------------|
| Retrograde     | `rev`               | `rev contour`               |
| Inversion      | `mirror axis`       | `mirror 3 contour` (about degree 3) |
| Augmentation   | `slow n`            | `slow 2 motif`              |
| Diminution     | `fast n`            | `fast 2 motif`              |
| Transposition  | `\|+` / `\|-`        | `contour \|+ 2`             |
| Rotation       | `<~` / `~>`         | `"<0 1>" <~ rhythm`         |
| Combination    | `struct` / `>:<`    | `rhythm >:< contour`        |

`>:<` (= `struct`) gates a fragment with a rhythm — the rhythm's onsets sample the contour's pitches.
`mirror axis d = 2*axis - d`. The clave/bell rhythm shorthands (`son32`, `rumba32`, `bossa32`,
`bellpat32`, and their `23` rotations) live in `Harmonic.Interface.Tidal.Groove`.

### The motif panel (per-piece swap)

A piece names its material in one statement — a recursive tuple binding of three names, where
`motif = rhythm >:< contour`:

```haskell
(rhythm, contour, motif) =
  ( son32                 -- rhythm gate
  , "[3 2 1 0]/4"         -- contour (voicing degrees)
  , rhythm >:< contour
  ) :: (Pattern Bool, Pattern Int, Pattern Int)
```

Primed tiers (`rhythm'`/`contour''`/…, up to `''`) fall back to BootTidal defaults, so a block can
reference any tier. Blocks use the names in their voice lines (`soprano = contour`), as gates
(`struct rhythm $ …`, `kick rhythm`), or pre-combined (`motif`). Editing a slot and re-executing the
block reprograms the piece's "genetic" material in one step.

## Studio-rig instruments

`subKick` (MPC sub/kick kit), `hmnx` (MPC harmonics keygroup) and `slce` (MPC sample pads)
belong to the author's studio rig, not to the orchestra, and are documented with its
channel map and MPC programs in [LIVE_ENVIRONMENT.md](LIVE_ENVIRONMENT.md).

## Form Declaration

A `form` is a list of nodes, each `<time> <kinetics> <dynamic> <progression>`.
Two orthogonal axes choose how a node is written — **time unit** and
**transition** — via four constructors:

| Constructor | Time unit | Transition |
|-------------|-----------|------------|
| `at`  | seconds | smooth (ramp) |
| `at'` | seconds | snap (hold, then jump) |
| `rh`  | bars (rehearsal marks, 4/4) | smooth |
| `rh'` | bars | snap |

Prime = snap; `rh` = bar-aligned nodes (bar = 4 beats). Mix freely in one form:

```tidal
form =
  [ at   0   0.0  0.0  s     -- 0s, smooth
  , rh   9   0.5  0.5  s     -- bar 8, smooth ramp
  , rh'  17  0.9  0.9  s     -- bar 16, hard SNAP (scene cut)
  , at'  120 0.2  0.2  s     -- 120s, snap
  ]
```

`snap` holds the node's value until the next node's exact time, then jumps —
for hard cuts (an explosive entry, a catastrophe). `smooth` ramps between nodes.
Display and form-relative cues follow the library's time-unit doctrine — `display` / `mark`
read bars, `display'` / `mark'` seconds (unprimed = bars, primed = seconds); the rig-side
detail is in [LIVE_ENVIRONMENT.md](LIVE_ENVIRONMENT.md).

## Kinetics Layering

Per-instrument kinetics ranges create crescendo ordering:

```tidal
tutti art f k d =
  ...
    -- Strings foundation (always)
    , violin1    T (0, 1)   k vl flow Soprano    # art
    -- Winds enter at 0.2
    , flute      T (0.2, 1) k vl flow Soprano
    -- Brass at 0.5
    , horn       T (0.5, 1) k vl flow Soprano
    -- Percussion at peak
    , timpani    T (0.8, 1) k vl grid Bass8vb
```

As kinetics rises from 0→1, instruments enter progressively.

## Suite Movement Structure

Every movement file in a suite follows the same eight-part skeleton. The
reference implementation is `live/local/Suites/Orpheus/scene2/09_pas_de_deux.tidal`;
all 57 movements across the six suites conform. The suites are the author's
own repertoire and are not distributed with the repository — the skeleton
below is the part that transfers.

| # | Part | Notes |
|---|---|---|
| 1 | `-- ====` banner | movement title / work / composer + date |
| 2 | `-- Super-structure position:` | where the movement sits in the work's dramatic arc |
| 3 | programme note + `-- Adaptation:` / `-- Reimagination:` | prose; what the source is and what we changed |
| 4 | generation | `tempo` · `lead` · `let ctx` · `s <- seek … $ attempt N K $ gen` |
| 5 | `-- Form:` + `-- Arc:` | loop length, then the kinetics journey in one line |
| 6 | `-- ── MOTIFS ──` *(optional)* | the `(rhythm, contour, motif)` panel, when the movement has a repeated cell |
| 7 | `-- ── ORCHESTRAL BLOCKS ──` | one function per section, each with a prose comment |
| 8 | `-- ── LAUNCHER ──` | `do let f/d/k` … `mapM_ id [hush, setbpm tempo, …]`, then `hush''` |

**Super-structure position** uses a dramaturgical vocabulary rather than a bare
fraction — `Establishing`, `Inciting event`, `Development`, `The Swerve`,
`GOLDEN RATIO`, `PRIMARY`/`SECONDARY`/`TERTIARY`, `Peak`, `Convergence` — given
alone, as a fraction with a parenthetical name, or as a range:

```tidal
-- Super-structure position: 3/4 (PEAK — PRIMARY) — THE LOOK; the longest
-- movement and the ballet's dramatic climax.
-- Super-structure positions: 1/8 (Establishing) through 1/4 (Inciting event)
```

**Form time base** is chosen per idiom, and the two axes are independent
(see *Form Declaration* above):

- `rh` / `rh'` — **bars**, for metrical idioms (Brandenburg, M-k191, TheGreat,
  Seasons). Bars resolve at 4/4: `bars = seconds × bpm / 240`.
- `at` / `at'` — **seconds**, for narrative or rubato timing (Orpheus, Planets).

Skeleton:

```tidal
-- ============================================================================
-- IV. MOVEMENT TITLE
-- Work, catalogue number — Composer (date)
-- ============================================================================
--
-- Super-structure position: 1/2 (Development) — one line on the dramatic role.
--
-- Programme note …
--
-- Adaptation: what this reimagination does differently.
-- ============================================================================

tempo = 108

start <- lead "F maj"

let ctx = hcKey "1b"
        $ hContext

s <- seek "bach" $ cue start $ tonal ctx $ len 8 $ entropy 0.25 $ attempt 2 6 $ gen

-- Form: ~370s loop (360s music + 10s silence)
-- Arc: ritornello -> episode -> return -> peak -> concluding ritornello
form =
  [ rh    1    0.0   0.0   s
  , rh    3    0.5   0.5   s     -- ritornello: full ensemble
  , rh  167    0.0   0.0   s     -- gap
  ]

-- ── MOTIFS ───────────────────────────────────────────────
(rhythm, contour, motif) =
  ( "[1 1 1 1 1 1]/4"          -- rhythm gate  (Pattern Bool)
  , "[3 1 3 0]/4"              -- contour      (voicing degrees)
  , rhythm >:< contour         -- rhythm gates contour
  ) :: (Pattern Bool, Pattern Int, Pattern Int)

-- ── ORCHESTRAL BLOCKS ───────────────────────────────────

-- Horns: the hunting call
horns f k d = p "horns" $ do
  let vl = voiceLines {_vl = "~"
        , soprano = motif
        }
  f
    $ stack [silence
        , horn T (0, 1) k vl flow Soprano
    ] |* vel d

-- ── LAUNCHER ────────────────────────────────────────────

do
  let
    f = id
    d = (* 1)
    k = iK tempo form (warp "[1 2 3 4]/4")
  putStrLn "WORK — IV. Movement Title"
  mapM_ id [hush, setbpm tempo
    ,horns f k  $ d 0.85
    ]

hush''
```

Rhythm gates are **bar-scoped**: one cycle is one beat, so a bar-length gate
carries `/4`. `"[1 1 1 1]*2"` is eight strokes per *beat* (32 per bar), which is
almost never what a motif wants — write `"[1 1 1 1 1 1 1 1]/4"` instead.

## Examples

### Basic: wind section

```tidal
,wind f k $ d 0.9
```

### Custom voice lines

```tidal
wind' f k d = p "wind" $ do
  let vl = voiceLines {_vl = "~"
        , soprano = "[3 2]/4"
        , bass    = "[0 1]/4"
        }
  f
    $ stack [silence
        , flute    T (0,1) k vl flow Soprano
        , oboe     T (0,1) k vl flow Alto
        , clarinet T (0,1) k vl flow Tenor8vb
        , bassoon  T (0,1) k vl flow Bass8vb
    ] |* vel d
```

### Full orchestra with articulation switching

```tidal
,tutti arco f k $ d 0.9    -- arco strings
,tutti pizz f k $ d 0.7    -- pizzicato strings
```

### Combined launcher

```tidal
do
  let
    f = (swingBy 0.04 2)
    d = (* 1)
    k = iK tempo form (warp "[1 2 3 4]/8")
  mapM_ id [hush, setbpm tempo
    ,wind f k       $ d 0.9
    ,strg f k       $ d 0.8
    ,brss f k       $ d 0.9
    ,perc f k       $ d 0.7
   ]
```
