# Algorithmic Orchestration

Scoring music for a virtual orchestra via TidalCycles live coding. This document is the operational guide for the **Algorithmic Orchestration** principle — the paradigm of abstracting musical elements into three concerns: harmony/contexts (the Harmonic Algorithm), form/constants (the Spectral Narrative), and interfaces/timbres (instrument functions, voice lines, articulations).

## Signal Chain

Code → TidalCycles → SuperDirt → MIDI → Roland JV-1010 → orchestral music

## JV-1010 Configuration (16 MIDI Channels)

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

This table is the JV-1010 rig only. The studio rig (soft synths, DFAM/P-6, M32/S-1, MPC,
drum machines) has its own map in `documents/LIVE_ENVIRONMENT.md`; there `hmnx` sits on
channel 11. The two rigs are never live together.

### The continuo voice (ch 7)

The orchestra above is a fixed, deliberate configuration — it is not extended.
The one permitted variation is **channel 7**, which may be voiced as any pitched
polyphonic plucked, keyboard, mallet or choral colour. Set it **once per
movement** in the launcher; never hot-swap it mid-flow.

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

Each instrument is a partial application of `instrument range channel`:

```haskell
instrument :: (Int, Int) -> Int -> Layer -> (Double, Double) -> IK -> VoiceLines -> VoiceFunction -> Voice -> ControlPattern
```

Pipeline: `arrange` → `# ch` → `|+ oct` → `clip`

```haskell
flute    = instrument (-12, 26)  1   -- C3-D6  (MIDI 48-86)
oboe     = instrument ( -2, 33)  2   -- Bb3-A6 (MIDI 58-93)
clarinet = instrument (-22, 34)  3   -- D2-Bb6 (MIDI 38-94)
-- etc.
```

### Unpitched (struct-based)

```haskell
bassdrum pat = struct pat $ midinote 36 # ch 9 # sustain 0.05
tamtam   pat = struct pat $ midinote 31 # ch 11 # sustain 0.5
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
table (`Orchestra.hs`, this file, `live/ORCHESTRAL_CATALOGUE.tidal`).

### Unpitched Percussion

| Instrument | Ch | MIDI Note | Usage |
|---|---|---|---|
| Bass Drum | 9 | 36 (C2) | `bassdrum pat` |
| Tam-tam | 11 | 31 (G1) | `tamtam pat` |

### subKick (separate signal chain — `"thru"` device, not JV-1010)

| Part | Ch | MIDI | Notes |
|---|---|---|---|
| Sub | 10 | 36–47 | C2–B2 (pitch class + 36, mapped from harmonic root) |
| Kick | 10 | 48 | C3 (fixed) |
| Silence | 10 | 35 | B1 (no sample) |

### String Articulations

Strings default to arco (ch 16). Override with `#`:

| Articulation | Ch | Alias | Usage |
|---|---|---|---|
| Pizzicato | 12 | `pizz` | `# pizz` or `tutti pizz f k $ d 0.7` |
| Spiccato | 13 | `spicc` | `# spicc` |
| Marcato | 14 | `marc` | `# marc` |
| Legato | 15 | `legg` | `# legg` |
| Arco | 16 | `arco` | Default (same as string channel) |

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

For interactive range tests, see [`live/ORCHESTRAL_CATALOGUE.tidal`](../live/ORCHESTRAL_CATALOGUE.tidal).

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

Channel routing with default arco:

```haskell
pizz  = ch 12    -- pizzicato
spicc = ch 13    -- spiccato
marc  = ch 14    -- marcato
legg  = ch 15    -- legato
arco  = ch 16    -- arco (default)
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

## Groove — subKick

`subKick` is a separate signal chain — it does **not** use the JV-1010. It routes via the `"thru"` device on MIDI channel 10 to an MPC (or equivalent sampler).

### Note Mapping

| Part | MIDI Note | Range |
|------|-----------|-------|
| Sub  | 36–47 | C2–B2 (pitch class → MIDI, mapped from harmonic root) |
| Kick | 48 (fixed) | C3 |
| Silence (no sample) | 35 | B1 |

Sub pitches are normalised from the progression's harmonic root pitch class: `pitch_class + 36`. This places the sub register below all orchestral instruments, leaving MIDI 48+ free for anything else sharing the channel.

### Note durations (no CC64)

Each sub note is held by its own duration: from its onset to the next kill boundary — the
manual offs (`subOffPat`) and, for `maxDur < 1`, each onset shifted by `maxDur*4` — emitted
with `legato 1` so the MIDI note-off lands exactly on the boundary (`holdToNext`). No CC64
is emitted: the MPC sub program does not treat the sustain pedal as a damper (a MIDI
note-off arriving under a held pedal is stranded and survives CC64=0 and All-Notes-Off;
an external keyboard reproduces it), so holding by pedal cannot work on this hardware.
`subPedalOff` in `launch'`/`hush` still lifts the pedal defensively.

### Voicing Cache

Voicings are pre-computed once at construction time (not per TidalCycles frame). All unique progressions in `kProg k` are resolved upfront; the per-frame lambda does a lookup instead of running the voice function. This eliminates hundreds of redundant computations per second in full orchestral mode.

### Usage

```tidal
subk f k d = p "subKick"
  $ f
    $ subKick d k root (maxDur, subOnStr, subOffStr, kickStr)
```

- `root` or `fund` — always returns the harmonic root regardless of inversion
- Sub group gates at `(0.1, 1)`, kick at `(0.2, 1)` via `ki`
- `maxDur < 1` triggers auto-off; `maxDur >= 1` means manual-off only

## Harmonics — hmnx

`hmnx` is the Harmonic Algorithm's own instrument: the harmony rendered as **natural
harmonics of the Electric Contrabass Cittern** (tuning `E A e G B`) through the MPC
harmonics keygroup on **MIDI channel 11**. The keygroup is pitch-accurate — each sampled
overtone sits at the note number of its true sounding pitch — so the keygroup, not the
code, decides what can sound.

### Strings and partials

The four playable partials of the 2016 thesis — octave, fifth, double octave, third
(partials 2 3 4 5) — sit at `+12 +19 +24 +28` from a string's fundamental:

```
open A1 = 33 → 45 52 57 61        open B2 = 47 → 59 66 71 75
open E2 = 40 → 52 59 64 68        open C3 = 48 → 60 67 72 76   (the B string, Hipshot lever at C)
open G2 = 43 → 55 62 67 71
```
Shared pitches (one sample each): 52 (A/E), 59 (E/B), 67 (G/C), 71 (G/B). Playable pitch
classes `{0,1,2,3,4,6,7,8,9,11}` — F and B♭ are unreachable and are simply omitted.

### `harmonics` — one string, a special `arrange`

`harmonics :: NoteName -> (Double,Double) -> IK -> Layer -> VoiceFunction -> (Progression -> Progression) -> [Pattern Int] -> Pattern ValueMap`

`arrange` with the string first and no register slot. The `NoteName` is the string's
fundamental, placed in reference octave 2 (36 + pitch class); the launcher puts the
string in its real octave with `|+ oct` / `|- oct`, exactly as the orchestral blocks do.
Per bar, the string's viable notes are its harmonic pitches whose pitch class is among
the bar's voiced tones; a pattern index picks the i-th by floor-mod — never an octave
wrap, these are fixed sample pitches; a bar with no viable note rests. Within a string
the contour is `mono'` (latest-note priority: a new harmonic damps the previous); across
strings the launcher's stack is polyphonic. `harmonics` is a render-time filter only — it
never reads the generation context's overtone constraint; generating inside the
instrument's overtone set (`hcOvertones "E A e G B"`) is what keeps every tone playable.

### The instrument is a launcher block

The tuning lives in the launcher: which strings you instantiate and their octaves. One
declared `pat` is the default — every string picks it up and filters it to what it can
sound; a pitch shared by two strings doubles into resonance, curated in performance.

```haskell
hmnx f k d = p "hmnx" $ do
  let o   = ch 11
      pat = "[0 1 2 3]/4"
  f
    $ stack [silence
      ,harmonics A (0,1) k T flow (overlapF 0) ["~", pat] # o |* vel 0.8 |- oct 1   -- A1
      ,harmonics E (0,1) k T flow (overlapF 0) ["~", pat] # o |* vel 0.8            -- E2
      ,harmonics G (0,1) k T flow (overlapF 0) ["~", pat] # o |* vel 0.8            -- G2
      ,harmonics B (0,1) k T flow (overlapF 0) ["~", pat] # o |* vel 0.8            -- B2
      -- ,harmonics C (0,1) k T flow (overlapF 0) ["~", pat] # o |* vel 0.8 |+ oct 1 -- lever at C
      ,arrange (0,1) k (-9,9) T flow (overlapF 0) ["~"] # o |* vel 0.6            -- unfiltered: keygroup decides
      ,cc 64 "[1@63 0]" #o                                                        -- damper: accumulation
    ]# legato 1 |* vel d
```

- **Sustain** is the outer `# legato`; **CC64** is one global damper for the whole
  instrument (harmonic accumulation), independent of legato. `hmnxPedalOff` lifts it in
  `launch'` / `hush`.
- **Shared pitches** across strings are handled by SafeMIDIOut (re-articulate, reference-
  counted note-offs) — no dedupe.
- **`mono'`** (`retrig`) is general: latest-note-priority monophony, the opposite of
  Tidal's first-note `mono`. Set a keygroup mute group per string on the MPC for a
  hardware guarantee regardless of legato.
- The 12-step pitch-class LEDs follow ch 11 (`~ledCoordinator.watch(10, (45..76))`).

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
  , rh   8   0.5  0.5  s     -- bar 8, smooth ramp
  , rh'  16  0.9  0.9  s     -- bar 16, hard SNAP (scene cut)
  , at'  120 0.2  0.2  s     -- 120s, snap
  ]
```

`snap` holds the node's value until the next node's exact time, then jumps —
for hard cuts (an explosive entry, a catastrophe). `smooth` ramps between nodes.
The 12-step display reads the form in bars by default (`display k`) or seconds (`display' k`) —
unprimed = bars, primed = seconds, the same doctrine as `mark` / `mark'` (form-relative cues:
`mark 32 k [pad 1]` fires once per loop at bar 32 as written for `rh`; `mark'` takes seconds).

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
  [ rh    0    0.0   0.0   s
  , rh    2    0.5   0.5   s     -- ritornello: full ensemble
  , rh  166    0.0   0.0   s     -- gap
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
    ,subk f k       $ d 1
   ]
```
