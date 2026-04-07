# Algorithmic Orchestration

Scoring music for a virtual orchestra via TidalCycles live coding.

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
,wind f r k       $ d 0.9
,tutti arco f r k $ d 0.9
,k909 f k         $ d 0.4
```

Where `d = (* 1)` in the launcher — a dynamics multiplier applied via `$`.

## Unified Block Template

Every block (section or blend) has the same two-part structure:

```tidal
{name} f r k d = p "{name}" $ do
  let vl = voiceLines {_vl = "~"
        -- , soprano = "3"
        -- , alto    = "1"
        -- , tenor   = "2"
        -- , bass    = "0"
        }
  f
    $ stack [silence
        , instrument (ki) vf Voice r k vl
        , ...
    ] |* vel (kDynamic k) |* vel d
```

1. **Voice declaration** (`vl`): `voiceLines` with optional overrides
2. **Instrument stack**: each line is an instrument with kinetics range, voicing paradigm, voice assignment

## Instrument Functions

### Pitched (arrange + clip + octave)

Each instrument is a partial application of `instrument range channel`:

```haskell
instrument :: (Int, Int) -> Int -> (Double, Double) -> VoiceFunction -> Voice -> Pattern Int -> Kinetics -> VoiceLines -> ControlPattern
```

Pipeline: `arrange` → `# ch` → `|+ oct` → `clip`

```haskell
flute    = instrument (48, 86)  1   -- MIDI range C3-D6
oboe     = instrument (46, 81)  2
clarinet = instrument (26, 82)  3
-- etc.
```

### Unpitched (struct-based)

```haskell
bassdrum pat = struct pat $ midinote 36 # ch 9 # sustain 0.05
tamtam   pat = struct pat $ midinote 31 # ch 11 # sustain 0.5
```

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
, violin1 (0,1) flow Soprano r k vl # pizz
```

For `tutti`, the articulation is a parameter:
```tidal
tutti arco f r k $ d 0.9
tutti pizz f r k $ d 0.7
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

## Groove — subKick

`subKick` is a separate signal chain — it does **not** use the JV-1010. It routes via the `"thru"` device on MIDI channel 10 to an MPC (or equivalent sampler).

### Note Mapping

| Part | MIDI Note | Range |
|------|-----------|-------|
| Sub  | 36–47 | C2–B2 (pitch class → MIDI, mapped from harmonic root) |
| Kick | 48 (fixed) | C3 |
| Silence (no sample) | 35 | B1 |

Sub pitches are normalised from the progression's harmonic root pitch class: `pitch_class + 36`. This places the sub register below all orchestral instruments, leaving MIDI 48+ free for anything else sharing the channel.

### CC64 Sustain Mechanism

The sub voice uses MIDI CC64 (sustain pedal) to hold notes:
- `sustain 0.01` triggers a brief note-on (the sampler latches it)
- `segment 16` CC64=127 background keeps the sustain held (~1 event/frame at 30fps)
- CC64=0 at `maxDur` or `subOffPat` boundaries releases the note

This mechanism is intentional and must not be changed.

### Voicing Cache

Voicings are pre-computed once at construction time (not per TidalCycles frame). All unique progressions in `kProg k` are resolved upfront; the per-frame lambda does a lookup instead of running the voice function. This eliminates hundreds of redundant computations per second in full orchestral mode.

### Usage

```tidal
p "subKick" $ subKick fund (rep s4 1) dyn k
  (maxDur, subOnStr, subOffStr, kickStr)
```

- `fund` — always returns the harmonic root regardless of inversion (preferred over `bass` for kick/sub)
- Sub group gates at `(0.1, 1)`, kick at `(0.2, 1)` via `ki`
- `maxDur < 1` triggers auto-off; `maxDur >= 1` means manual-off only

## Kinetics Layering

Per-instrument kinetics ranges create crescendo ordering:

```tidal
tutti art f r k d =
  ...
    -- Strings foundation (always)
    , violin1    (0, 1)   flow Soprano    r k vl # art
    -- Winds enter at 0.2
    , flute      (0.2, 1) flow Soprano    r k vl
    -- Brass at 0.5
    , horn       (0.5, 1) flow Soprano    r k vl
    -- Percussion at peak
    , timpani    (0.8, 1) root Bass8vb    r k vl
```

As kinetics rises from 0→1, instruments enter progressively.

## Examples

### Basic: wind section

```tidal
,wind f r k $ d 0.9
```

### Custom voice lines

```tidal
wind' f r k d = p "wind" $ do
  let vl = voiceLines {_vl = "~"
        , soprano = "[3 2]/4"
        , bass    = "[0 1]/4"
        }
  f
    $ stack [silence
        , flute    (0,1) flow Soprano    r k vl
        , oboe     (0,1) flow Alto       r k vl
        , clarinet (0,1) flow Tenor8vb   r k vl
        , bassoon  (0,1) flow Bass8vb    r k vl
    ] |* vel (kDynamic k) |* vel d
```

### Full orchestra with articulation switching

```tidal
,tutti arco f r k $ d 0.9    -- arco strings
,tutti pizz f r k $ d 0.7    -- pizzicato strings
```

### Combined launcher

```tidal
do
  let
    r = warp "[1 2 3 4]/8"
    f = (swingBy 0.04 2)
    d = (* 1)
    k = formK tempo form
  mapM_ id [hush, setbpm tempo
    ,wind f r k       $ d 0.9
    ,strg f r k       $ d 0.8
    ,brss f r k       $ d 0.9
    ,perc f r k       $ d 0.7
    ,subk f r k       $ d 1
   ]
```
