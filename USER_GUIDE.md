# The Harmonic Algorithm — User Guide

This is the readable version of the interactive guide. If you'd like to
experience a playable version where you can evaluate each example and hear
the results in real time, open [`live/USER_GUIDE.tidal`](live/USER_GUIDE.tidal)
in your TidalCycles editor and work through it top to bottom.

### Prerequisites

Before you begin, you'll need:
1. Neo4j running (`docker compose up -d neo4j`)
2. SuperCollider running with SuperDirt
3. A TidalCycles editor plugin (VS Code, Pulsar, or nvim)
4. `BootTidal.hs` loaded with `Harmonic.Lib`

___

## 1. Quick Start

The system has three interchangeable parts that combine in a launcher block:

1. **Environment** — tempo and tonal area
2. **Composition** — a generated progression
3. **Instrumentation** — pattern interfaces that turn harmony into sound

The basic flow is: define where you're starting harmonically, generate a
progression, set up an instrument, and launch. Here's the essence of it:

```haskell
-- Define your starting point and tonal area
let start = initCadenceState 0 "C" [0,4,7]
let ctx = hContext "*" "0#" "*"

-- Generate a 4-chord progression
s4 <- gen start 4 "*" 0.5 ctx

-- Launch on an instrument (single-state form = no arc)
let k = formK tempo [at 0 1 1 s4]
p01 id (rep s4 1) 0.9 k
```

That's it. Everything else in this guide is about the musical choices you
can make within this framework.

`[video: first launch — setting up a context, generating a 4-chord
progression from C major, hearing it play through a piano. The terminal
shows the generated chord names as the harmony cycles]`

___

## 2. Core Building Blocks

### 2.1 CadenceState — Where to Start

A `CadenceState` is your starting chord. You define the root movement
(0 for a fresh start), the root note name, the intervals that make up the
chord, and whether you prefer flat or sharp notation:

```haskell
let start = initCadenceState 0 "C" [0,4,7]
```

The intervals define the chord quality — `[0,4,7]` is major, `[0,3,7]` is
minor, `[0,3,6]` is diminished, `[0,5,7]` is sus4, `[0,4,8]` is augmented.
You can start on any root and with any quality.

`[video: creating different starting chords — major, minor, diminished,
sus4, augmented — each one generating a short progression, hearing how the
starting quality colours everything that follows]`

### 2.2 HarmonicContext — Constraints

The `HarmonicContext` defines the tonal space within which chords can appear.
It has three filters:

- **Overtones** — which pitch classes are available (think of it as the
  playable notes on your instrument)
- **Key** — a tonal area that further narrows the available pitches
- **Roots** — which bass notes are allowed

All three default to `"*"` (wildcard — no restrictions). You can constrain
any or all of them:

```haskell
let ctx = hContext "E A D G" "1#" "*"
-- Overtones of bass guitar tuning, in G major, any root
```

`[video: comparing a wide-open chromatic context against bass-tuned
overtones and then strict G major key filtering — hearing how each layer
of constraint shapes the character of the generated progressions, from
wide and unpredictable to focused and idiomatic]`

### 2.3 Filter String Syntax

Each of the three arguments to `hContext` parses its string differently,
but all three support `"*"` (wildcard) and `"-"` (removal).

**Overtones** (first argument): bare note names give you the first four
overtones of that fundamental. `"E A D G"` gives you the union of all four
overtone series. Append a prime (`'`) for a single exact pitch: `"E'"` means
just the pitch E, no overtones. You can combine both: `"G E' A' A#'"` gives
you the overtones of G plus the exact pitches E, A, and A#.

**Key** (second argument): use sharps and flats notation for key signatures.
`"0#"` is C major, `"1#"` is G major, `"2b"` is Bb major. Important: `"C"`
here means the single pitch C, not the key of C major — use `"0#"` for that.

**Roots** (third argument): same syntax as key, plus two special values —
`"key"` mirrors whatever key filter you set, and `"tones"` uses the
intersection of your overtone and key filters.

You can append `rise` or `fall` to the roots string to force the bass note
to step through the allowed set in order. `rise` always moves to the
closest note above; `fall` always moves to the closest note below (with
octave wrapping). This creates stepwise ascending or descending bass lines
within whatever root set you've defined:

```haskell
hContext "*" "0#" "0# fall"      -- descend through C major scale tones
hContext "*" "0#" "0# rise"      -- ascend through C major scale tones
hContext "*" "0#" "C E G fall"   -- descend through {C, E, G}
hContext "*" "0#" "key rise"     -- ascend through key-derived bass notes
hContext "*" "0#" "tones fall"   -- descend through effective overtones
```

You can subtract pitches with the `-` prefix: `"E A D G -Bb'"` gives you
the bass overtones minus Bb.

`[video: walking through filter syntax examples — building up from wildcards
to overtones to key filtering to root constraints to removal — each step
showing the generated output narrowing and focusing, the musical character
becoming more specific]`

### 2.4 Generation

Four generation functions share the same signature but differ in how much
they tell you about what's happening:

- **`gen`** — prints a musical header with the progression
- **`genSilent`** — no output at all (for performance)
- **`gen'`** — deeper context: per-step candidate pools, selection indices
- **`gen''`** — full trace: transform and advance computations (for debugging)

They're interchangeable — switch between them with a single word change.

**Composer specification** controls whose harmonic sensibilities guide the
generation:
- `"*"` — aggregate across all corpus composers
- `"bach"` — a single composer's learned transitions
- `"debussy:0.75 bach:0.25"` — a weighted blend
- `"ravel stravinsky mozart:2"` — equal weight with a double dose of Mozart

**Entropy** is the creativity parameter. Low values (< 0.3) produce
conservative, consonant progressions that follow the most common cadences.
High values (> 0.8) reach deeper into the probability space for surprising,
less predictable movements. Around 0.5 is balanced.

`[video: generating with different entropy levels — at 0.3 the harmony moves
cautiously through familiar territory, at 0.5 it's balanced and musical,
at 0.8 it takes unexpected turns into distant keys]`

`[video: composer blending — "bach" produces strong functional harmony,
"debussy" brings colourful modal movement, and the weighted blend
"debussy:0.75 bach:0.25" creates something neither would have written
alone — functional foundations with impressionistic colour]`

### 2.5 Dissonance Drift

You can constrain the direction of harmonic tension across a progression.
The `dissonant` modifier ensures each subsequent chord has equal or greater
dissonance than the current one. The `consonant` modifier does the
opposite — each chord must be equal or less dissonant.

```haskell
-- Progressions that build tension:
s4 <- gen start 4 "*" 0.5 (dissonant $ hContext "*" "0#" "*")

-- Progressions that resolve:
s4 <- gen start 4 "*" 0.5 (consonant $ hContext "*" "0#" "*")

-- No constraint (default):
s4 <- gen start 4 "*" 0.5 (hContext "*" "0#" "*")
```

Dissonance is measured using Hindemith's interval vector theory. Major and
minor triads score lowest (most consonant), while tritone-heavy and
chromatic clusters score highest (most dissonant). The drift applies as a
filter on the candidate pool — you still get the best musical choice
(composer style, voice leading) that meets the constraint.

Drift composes with all other context parameters:

```haskell
-- Descending bass with building tension:
s4 <- gen start 8 "*" 0.5 (dissonant $ hContext "*" "0#" "0# fall")
```

If no candidates meet the constraint at a given step (e.g., already at
maximum dissonance with `dissonant`), the filter falls back to the full
pool for that step — generation never fails.

___

## 3. Pattern Launcher Blocks

### 3.1 Pattern Block Anatomy

The launcher paradigm is the core of the TidalCycles interface. A pattern
block is a function with four parameters:

- **f** — a transformation function (id, slow 2, fast 2, rev, swingBy)
- **r** — a chord selection pattern (which chord is active when)
- **d** — dynamics (velocity scaling)
- **k** — kinetics context (form, progression, dynamics envelope)

The progression is no longer passed directly — it comes from the kinetics
context (`kProg k`), which also carries the form's kinetics and dynamic
signals.

A minimal launcher looks like this:

```haskell
p01 f r d k = d01 $ do
  let o = ch 01
  f $ arrange (0,1) flow id r k (-9,9) ["~", "0 1 2 3"]
    # o |* vel (kDynamic k) |* vel d
```

You define the launcher once, then call it with different kinetics contexts,
transformations, and dynamics throughout a session.

`[video: a minimal launcher block — defining p01, generating a progression,
launching it with id (no transformation), hearing chords cycle through
a piano sound]`

### 3.2 Multi-Voice Stacking

You can stack multiple `arrange` blocks inside a single launcher for
layered textures — each voice with its own voicing strategy, register,
dynamics, kinetics range, and note pattern:

```haskell
p03 f r d k = d03 $ do
  let o = ch 01
  f $ stack [silence
    ,arrange (0.5,1) flow id r k (-9,9) ["~", "[0,1,2,3]/4"]
      # o |* vel 0.8
    ,arrange (0,1) root id r k (-9,9) ["~", "0*2"]
      # o |* vel 1 |- oct 2
  ] |* vel (kDynamic k) |* vel d
```

The kinetics range (first argument to `arrange`) controls when each voice
activates — `(0.5,1)` means the flow chords only play when the form's
kinetics signal is above 0.5, while `(0,1)` plays always.

`[video: building up layers — starting with flow chords alone, then adding
a root bass line an octave below, hearing the texture grow from sparse
harmony to a full arrangement]`

### 3.3 Transformation Functions

The **f** parameter accepts any TidalCycles transformation. This means you
can reshape the timing and character of a progression without changing the
harmony itself:

```haskell
-- Try each:
p01 id r d k          -- no change
p01 (slow 2) r d k    -- half speed
p01 (fast 2) r d k    -- double speed
p01 rev r d k         -- reversed
p01 (swingBy 0.1 2) r d k  -- swing feel
```

You can compose transformations: `slow 2 . rev` plays the progression
backwards at half speed.

`[video: applying transformations in real time to the same progression —
normal speed, then slow 2 stretching it out, then rev playing it backwards,
then swingBy adding a groove feel — the same harmony, completely different
musical character each time]`

### 3.4 Complete Launcher

A production launcher typically has three voices — chords, bass, and melody —
each with their own voicing, register, kinetics range, and rhythmic pattern.
See [Section 3.4 of USER_GUIDE.tidal](live/USER_GUIDE.tidal) for the full
example.

`[video: a complete production launcher with flow chords in the mid range,
root bass an octave below, and a melodic flow line an octave above —
hearing a full arrangement emerge from a single generated progression]`

___

## 4. Voicing Strategies

The system provides five voicing strategies. Each one takes the same
progression and produces a different musical result:

### 4.1 flow — Smooth Voice Leading

Minimal melodic movement between chords. The algorithm picks inversions that
create the smoothest transitions. Best for pads, harmonic beds, and
situations where you want the harmony to flow without jumps.

`[video: flow voicing — a 4-chord progression played with smooth, minimal
voice movement between each chord, the upper voices barely shifting]`

### 4.2 root — Root Position

Root note always in the bass, with the upper voices still optimised for
smooth movement. Best for bass instruments and any situation where you want
a clear harmonic foundation.

`[video: root voicing — the same progression with the root firmly in the
bass every time, a solid grounded feel]`

### 4.3 lite — Literal

Chords exactly as stored, first-root normalised but no voice leading
optimisation. Best for direct control or when you want the raw intervals.

`[video: lite voicing — the same progression with literal intervals, no
optimisation, hearing the jumps and angles that the algorithm would
normally smooth out]`

### 4.4 bass — Bass Note Only

Extracts just the lowest voice as a monophonic line. Best for bass lines
and situations where you want a single-note part that follows the harmony.

`[video: bass voicing — just the bass note from each chord, a single
melodic line tracing the bottom of the harmony]`

### 4.5 fund — Harmonic Root

Always returns the harmonic root regardless of inversion. Where `bass`
might give you a third or fifth when the chord is inverted, `fund` always
gives you the true root. Best for kick drums and sub bass.

`[video: fund voicing — the harmonic root on every chord, steady and
inversion-invariant, perfect for a kick drum that always locks to the
fundamental]`

### 4.6 Comparison

All five voicings playing the same progression simultaneously reveal their
different characters — flow is smooth, root is grounded, lite is angular,
bass traces the bottom, and fund anchors everything to the harmonic root.

`[video: all five voicings playing simultaneously on separate MIDI channels
— hearing how they layer and contrast, each one contributing a different
musical quality to the same underlying harmony]`

### 4.7 Voicing in arrange

In practice, you combine voicings in a single launcher — flow for chords,
root for bass, bass or fund for a kick pattern. Each voice can have its
own kinetics range for form-driven activation:

```haskell
f $ stack [silence
  ,arrange (0.5,1) flow id r k (-9,9) ["~", "[0,1,2,3]/4"]
    # o |* vel 0.8
  ,arrange (0,1) root id r k (-9,9) ["~", "0*2"]
    # o |* vel 1 |- oct 1
  ,arrange (0,1) bass id r k (-9,9) ["~", "0*4"]
    # o |* vel 1.2 |- oct 2
] |* vel (kDynamic k) |* vel d
```

`[video: a full launcher combining flow chords, root bass, and bass kick
— hearing how the different voicing strategies create a complete, layered
arrangement from a single progression]`

___

## 5. Progression Manipulation

Once you've generated a progression, you can transform it in real time.
These functions reshape the harmonic material without regenerating:

### 5.1 rotate — Cycle Starting Position

Shifts the progression so it starts from a different chord. `rotate 2`
on an 8-chord progression starts from chord 3.

`[video: rotate — the original progression, then rotated by 2, hearing
the same chords but starting from a different point in the phrase,
the musical emphasis shifting]`

### 5.2 excerpt — Extract Range

Pulls out a section of the progression. `excerpt 4 7` extracts bars 4
through 7.

`[video: excerpt — zooming into bars 4-7 of an 8-chord progression,
hearing just that section loop, a focused fragment of the larger whole]`

### 5.3 transposeP — Shift Pitch

Shifts every chord by a number of semitones. `transposeP 5` moves
everything up a perfect fourth.

`[video: transposeP — the original progression in C, then transposed up
5 semitones to F, the same shapes and movements in a new key]`

### 5.4 reverse — Reverse Order

Plays the progression backwards. Familiar cadences sound completely
different in retrograde.

`[video: reverse — the progression forwards, then backwards, hearing
how the harmonic logic inverts — resolutions become departures,
arrivals become transitions]`

### 5.5 fuse — Concatenate Progressions

Joins multiple progressions end to end. Generate different sections
separately and fuse them into a longer journey.

```haskell
let fused = fuse [state1, state2]
```

`[video: fuse — two separately generated 4-chord progressions joined
into one 8-chord journey, hearing the transition between the two
different harmonic worlds]`

### 5.6 expandP — Repeat Chords

Repeats each chord a specified number of times. `expandP 2` doubles
the length by repeating every chord.

`[video: expandP — the original progression at normal harmonic rhythm,
then expanded so each chord plays twice, the harmony slowing down and
breathing more]`

### 5.7 progOverlapF — Add Passing Tones

Adds forward overlap between chords, creating passing tones that blur
the boundaries between one chord and the next.

`[video: progOverlapF — the progression clean, then with 1-bar forward
overlap, hearing the edges between chords soften as passing tones
anticipate the next harmony]`

### 5.8 Additional Functions

Several more manipulation functions are available:

- **interleave** — alternate chords from two progressions
- **progOverlapB** — backward passing tones
- **progOverlap** — bidirectional passing tones
- **insert** — insert a chord at a position
- **switch** — swap two chord positions
- **clone** — copy a chord to another position
- **extract** — pull out a single chord (with modulo wrap)

All positions are 1-indexed and wrap with modulo.

`[video: insert, switch, and clone in action — surgically modifying a
progression by inserting a new chord, swapping two positions, and
cloning a favourite chord to a different spot]`

### 5.9 Combining Transformations

Transformations compose naturally:

```haskell
-- Rotate, transpose, and reverse in one expression:
let s = transposeP 0 $ reverse $ transposeP 5 $ rotate 2 s8
```

`[video: chaining transformations — rotate, then transpose, then reverse
applied together, the original progression transformed beyond recognition
but still musically coherent]`

___

## 6. Chord Selection Patterns

### 6.1 warp — Mininotation Chord Selection

`warp` parses a TidalCycles mininotation string into a chord selection
pattern. Values are 1-indexed: `"1"` selects the first chord.

```haskell
warp "[1 2 3 4]/4"   -- one chord per bar over 4 bars
warp "[1 2 3 4]"     -- all chords in one bar
warp "1"             -- hold the first chord
```

`[video: warp in action — sequential chord changes one per bar, then all
four in a single bar, then holding on the first chord — hearing the
harmonic rhythm change from slow and spacious to rapid and then static]`

### 6.2 rep — Auto-Derive Sequential Pattern

`rep` automatically generates a sequential selection pattern from a
progression's length:

```haskell
rep s4 1     -- 1 bar per chord
rep s4 0.5   -- half bar per chord (twice as fast)
```

`[video: rep at different speeds — 1 bar per chord gives a relaxed pace,
0.5 doubles the harmonic rhythm, the same progression at two different
speeds of change]`

### 6.3 Musical Forms via Chord Patterns

This is where chord selection gets powerful. The full vocabulary of
TidalCycles mininotation is available for defining musical form:

```haskell
warp "[1 1 2 1]/4"       -- AABA form
warp "[1 2? 3 4]/4"      -- probabilistic (chord 2 appears randomly)
warp "[1 <1 2>]"         -- alternating per cycle
warp "[1 [1 2]]/2"       -- variable harmonic rhythm
```

`[video: building an AABA form using warp — the same 4 chords, but chord
selection creates a familiar verse-verse-bridge-verse structure, the form
emerging purely from how the chords are sequenced in time]`

### 6.4 arrange vs arrange'

Two arrangement strategies, both with kinetics range gating:

- **arrange** (onset-join) — each note maps through the chord active at
  its onset time. Sustained notes keep their pitch across chord boundaries.
  Best for melodic instruments.

- **arrange'** (squeeze) — the full pattern restarts for each chord slot.
  Best for rhythmic patterns that should repeat per chord.

Both take the same parameters: `(lo, hi)` kinetics range, voice function,
progression modifier, chord pattern, kinetics context, register, and
input patterns.

`[video: arrange vs arrange' — the same pattern through both strategies,
hearing sustained notes glide through chord changes with arrange, and
the pattern restart cleanly at each new chord with arrange']`

___

## 7. Form & Kinetics

### 7.1 Overview

The Kinetics framework encodes macro-level compositional arc as programmable
structure. A form is defined in wall-clock seconds, realized as TidalCycles
patterns, and loops endlessly. It carries three signals:

- **kSignal** — kinetics level (0–1), continuous piecewise linear
  interpolation. Used for range gating: controlling which voices are active.
- **kDynamic** — dynamic envelope (0–1), continuous piecewise linear
  interpolation. Used for velocity scaling across the form.
- **kProg** — active progression, discrete step function. Switches between
  progressions at form boundaries.

### 7.2 Defining a Form

A form is a list of `FormNode` values, each constructed with `at`:

```haskell
at :: Double -> Double -> Double -> Progression -> FormNode
at time kinetics dynamic progression
```

For example, a 30-second three-layer form:

```haskell
form =
  [ at  0  0.0  0.3 s0    -- low: root only
  , at 10  0.5  0.6 s0    -- mid: + upper voices
  , at 20  1.0  1.0 s1    -- high: full, new harmony
  , at 30  0.0  0.3 s0    -- loop back to low
  ]
```

Between nodes, kinetics and dynamic values interpolate linearly. The
progression holds as a step function — `s0` plays from 0–20s, `s1` from
20–30s. At the end of the form (30s), it loops back to the beginning.

### 7.3 Realizing a Form

`formK` converts a form definition and a BPM into a `Kinetics` value:

```haskell
k = formK tempo form
```

This produces looping TidalCycles patterns for all three signals.

### 7.4 Single-State Form (Formless)

A single `at` node produces constant signals — equivalent to having no
form at all. This is the simplest way to use the kinetics system:

```haskell
let k = formK tempo [at 0 1 1 s]
```

Here `kSignal = pure 1`, `kDynamic = pure 1`, `kProg = pure s`. Everything
passes, everything plays at full level.

### 7.5 Range Gating with ki

`ki` masks a pattern by the kinetics signal level:

```haskell
ki :: (Double, Double) -> Kinetics -> Pattern a -> Pattern a
ki (lo, hi) k pat
```

Events only pass when `kSignal` is within `[lo, hi]` (inclusive). This
is how voices activate at different points in the form:

```haskell
ki (0.5, 1.0) k chordPattern   -- only plays in upper half of form
ki (0.0, 0.3) k bassPattern    -- only plays in quiet sections
```

### 7.6 slate — Gated Stack

`slate` combines `ki` and `stack` — gate a list of patterns by a kinetics
range:

```haskell
slate :: (Double, Double) -> Kinetics -> [Pattern a] -> Pattern a
slate range k pats = ki range k $ stack pats
```

Useful for drum layers that activate at different intensity levels.

### 7.7 withForm — Progression Access

`withForm` applies a function taking `Progression` to the kinetics context,
reactively switching when the form changes progressions:

```haskell
withForm :: Kinetics -> (Progression -> Pattern ValueMap) -> Pattern ValueMap
```

### 7.8 Form Declarations

Forms are always declared inline in performance files — visible and editable
at the point of use. See `live/snippets.cson` for 7 editor snippet templates
(single-state, spectral narratives at fixed and variable durations, pop forms)
and `live/forms.tidal` for annotated reference versions.

### 7.9 Range Naming Conventions

When defining launcher blocks, a common pattern is naming kinetics ranges:

```haskell
let cresc = (0, 1)      -- always on, building
let upper = (0.5, 1)    -- upper half only
let peak  = (0.8, 1)    -- near peak only
```

___

## 8. Groove / Drums Interface

### 8.1 Basic subKick Usage

`subKick` creates MPC-style drum patterns that follow the harmonic root.
It reads the progression from the kinetics context and layers a sub bass,
a kick-off pattern, and a kick pattern with built-in ki gating:

```haskell
p "subKick" $ subKick fund (rep s4 1) 1 k
  (1/4, "[1(3,8)]/2", "[1(5,8,-2)]/2", "[1(3,8)]/2")
```

The sub group gates at `(0.1, 1)` and the kick group at `(0.2, 1)`,
so both gradually activate as the form builds.

`[video: subKick with a basic euclidean pattern — a kick drum that follows
the harmonic root, the low end locking to whatever chord is active,
creating a groove that's harmonically aware]`

### 8.2 fund vs bass for Kick Patterns

For kick drums, use `fund` rather than `bass`. When chords are inverted,
`bass` follows the lowest voice (which might be a third or fifth), while
`fund` always follows the true harmonic root.

`[video: fund vs bass for kicks on an inverted chord progression — hearing
the kick stay on the root with fund, and wander to the inversion's bass
note with bass — fund is the right choice for drums]`

### 8.3 Groove Patterns

The euclidean pattern strings and dynamics offer a wide range of groove
feels:

```haskell
-- Sparse:
(1/4, "[1(3,8)]", "[1(5,8,-2)]", "[1(3,8)]")

-- Dense with random dynamics:
subKick fund (rep s4 1) (range 0.5 1 rand) k
  (1/4, "[1(5,8)]", "[1(3,8,-1)]", "[1(4,8)]")
```

`[video: different groove patterns — a sparse euclidean feel, then a denser
pattern with random dynamics adding human variation — the same harmonic
progression, completely different rhythmic energy]`

___

## 9. Explicit Composition

### 9.1 fromChords — Building by Hand

You don't always need the algorithm to generate for you. `fromChords`
lets you build progressions explicitly from pitch-class lists:

```haskell
simpleProg = fromChords [
    [0, 4, 7],    -- C major
    [5, 9, 0],    -- F major
    [7, 11, 2],   -- G major
    [0, 4, 7]     -- C major
  ]
```

`[video: constructing a I-IV-V-I progression explicitly — entering the
pitch classes, launching it through a piano, hearing the most fundamental
cadence in Western harmony]`

### 9.2 Note Name Syntax

For readability, use note names instead of pitch-class integers:

```haskell
readableProg = fromChords [
    notesToPCs [C, E, G],     -- C major
    notesToPCs [D, F', A],    -- D major (F' = F#)
    notesToPCs [G, B, D]      -- G major
  ]
```

Primes indicate sharps: `C'` = C#, `F'` = F#, `G'` = G#.

`[video: the same progression built with note names — more readable,
same result, hearing the chords play through the piano]`

### 9.3 Form Transformation — AABA Paradigm

Define sections separately, then assemble different forms from the same
material:

```haskell
aSection = [notesToPCs [C, E, G], notesToPCs [F, A, C]]
bSection = [notesToPCs [G, B, D], notesToPCs [D, F', A]]

form1 = fromChords $ concat [aSection, aSection, bSection, aSection]  -- AABA
form2 = fromChords $ concat [aSection, bSection, aSection, bSection]  -- ABAB
```

You can also define form through chord selection with `warp` — both
approaches work, and you can combine them.

`[video: defining A and B sections, then assembling AABA and ABAB forms —
hearing the same harmonic material create two completely different musical
structures, the form shaping how the listener experiences the harmony]`

___

## 10. Advanced Techniques

### 10.1 Composer Blending

The composer specification isn't just a filter — it's a way of channelling
different harmonic sensibilities through the same system:

```haskell
prog0 <- gen start 8 "*" 0.5 ctx                    -- all composers
prog1 <- gen start 8 "bach" 0.5 ctx                  -- Bach alone
prog2 <- gen start 8 "debussy:0.75 bach:0.25" 0.5 ctx  -- weighted blend
```

Weights are normalised, so `"ravel:2 stravinsky:1 mozart:1"` gives you
50% Ravel, 25% each of Stravinsky and Mozart. Blended composers get a
creative portmanteau name in the output header.

`[video: comparing wildcard (all composers) vs "bach" (strong functional
harmony) vs "debussy" (colourful modal movement) vs the weighted blend
"debussy:0.75 bach:0.25" — same starting chord, four distinct musical
personalities, hearing how the learned transitions of different composers
shape the harmonic journey]`

### 10.2 Custom Context Patterns

Combine overtone, key, and root filtering for genre-specific contexts:

```haskell
jazzCtx  = hContext "*" "2#" "D F# A"       -- D major, jazz roots
bluesCtx = hContext "*" "1b" "F Bb C"       -- F major, I-IV-V roots
modalCtx = hContext "*" "*" "C Eb F G Bb"   -- modal, no key filter
bassCtx  = hContext "E A D G" "*" "*"       -- bass guitar overtones
```

`[video: four genre contexts in sequence — jazz in D major with specific
roots, blues in F locked to I-IV-V, modal on C with no key filter, and
bass-tuned overtones — hearing how context defines musical character as
much as the notes themselves]`

### 10.3 Entropy Tuning

Entropy is the single most expressive parameter. Small changes produce
audible differences in the character of the generated harmony:

```haskell
conservative <- gen start 8 "*" 0.3 ctx   -- safe, consonant
balanced     <- gen start 8 "*" 0.5 ctx   -- musical, balanced
exploratory  <- gen start 8 "*" 0.8 ctx   -- surprising, adventurous
```

`[video: entropy sweep — generating at 0.3, 0.5, and 0.8 from the same
starting point, hearing the harmony move from cautious and familiar through
balanced musicality to bold and surprising, the algorithm reaching deeper
into the probability space with each increase]`

___

## 11. Complete Performance Template

### 11.1 Setup

A performance session starts with a tempo and a harmonic context:

```haskell
setbpm 100
let ctx = hContext "*" "0#" "*"
```

### 11.2 Generate Sections

Generate different sections with different starting chords, composers,
and entropy levels:

```haskell
verse  <- gen (initCadenceState 0 "C" [0,4,7]) 8 "*" 0.4 ctx
chorus <- gen (initCadenceState 0 "F" [0,4,7]) 8 "*" 0.6 ctx
bridge <- gen (initCadenceState 0 "A" [0,3,7]) 8 "debussy:0.75 bach:0.25" 0.5 ctx
```

### 11.3 Launcher Blocks

Define launchers for each instrument — chords, bass, arpeggios — using
the voicing and stacking techniques from Sections 3 and 4, with kinetics
range gating from Section 7. See
[Section 11.3 of USER_GUIDE.tidal](live/USER_GUIDE.tidal) for complete
launcher definitions.

### 11.4 Performance Flow

Move through sections by launching with different progressions:

- **Intro** — verse, sparse, quiet
- **Verse** — full texture, chords + bass
- **Chorus** — transposed, add arpeggios, higher energy
- **Bridge** — contrasting, reversed, slower
- **Outro** — verse again, fading

`[video: a complete performance — moving through intro, verse, chorus,
bridge, and outro, each section with different progressions, textures,
and dynamics, hearing a full piece develop from individual generated
sections into a coherent musical arc]`

### 11.5 Live Improvisation

Generate one long progression and excerpt different sections on the fly,
using `warp` for form and transformations for variety:

```haskell
s32 <- gen start 32 "*" 0.5 ctx

-- Live: excerpt different sections
let sub = excerpt 0 7 s32
let sub2 = excerpt 8 15 s32
```

`[video: live improvisation — generating a 32-chord progression, then
excerpting different 8-bar sections in real time, applying transformations,
reshaping the music on the fly as it plays — the algorithm providing the
harmonic material, the performer sculpting the form]`

___

## 12. Quick Reference

### Generation

| Function | Output |
|----------|--------|
| `gen` | Musical header |
| `genSilent` | No output |
| `gen'` | Per-step diagnostics |
| `gen''` | Full trace |

### Context Modifiers

| Modifier | Example | Effect |
|----------|---------|--------|
| `dissonant` | `dissonant $ hContext "*" "0#" "*"` | Each chord >= current dissonance |
| `consonant` | `consonant $ hContext "*" "0#" "*"` | Each chord <= current dissonance |
| `"rise"` | `hContext "*" "0#" "0# rise"` | Bass steps up through root set |
| `"fall"` | `hContext "*" "0#" "0# fall"` | Bass steps down through root set |

### Chord Selection

| Function | Example | Effect |
|----------|---------|--------|
| `warp` | `warp "[1 2 3 4]/4"` | Mininotation → Pattern Int (1-indexed) |
| `rep` | `rep s4 1` | Auto-derive sequential (1 bar/chord) |

### Voicing

| Function | Bass | Voice Leading | Best For |
|----------|------|---------------|----------|
| `flow` | Any | Smooth (cyclic DP) | Pads, harmonic beds |
| `root` | Root | Smooth (cyclic DP) | Bass, grounded chords |
| `lite` | Any | None | Direct control |
| `bass` | Lowest | Mono | Bass lines |
| `fund` | Root | Mono | Kicks, sub bass |

### Transformation

| Function | Effect |
|----------|--------|
| `rotate n` | Cycle start position |
| `excerpt a b` | Extract range |
| `transposeP n` | Shift pitch (semitones) |
| `reverse` | Reverse order |
| `fuse [...]` | Concatenate list |
| `fuse2 a b` | Concatenate two |
| `interleave a b` | Alternate chords |
| `expandP n` | Repeat each chord n times |
| `progOverlapF n` | Forward passing tones |
| `progOverlapB n` | Backward passing tones |
| `progOverlap n` | Bidirectional passing tones |
| `insert cs n prog` | Insert chord at position |
| `switch a b prog` | Swap positions |
| `clone a b prog` | Copy position a to b |
| `extract n prog` | Pull chord (modulo wrap) |

### Explicit Construction

| Function | Example |
|----------|---------|
| `fromChords` | `fromChords [[0,4,7], [5,9,0]]` |
| `fromChords` | `fromChords [notesToPCs [C, E, G]]` |
| `prog` | `prog [[0,4,7]]` (alias for fromChords) |

### Pattern Interface

| Function | Strategy |
|----------|----------|
| `arrange range voice modifier r k register pats` | Onset-join with kinetics range gating |
| `arrange' range voice modifier r k register pats` | Squeeze with kinetics range gating |
| `subKick voice r dyn k tuple` | Sub/kick groove with kinetics gating |

### Form & Kinetics

| Function | Purpose |
|----------|---------|
| `at time kin dyn prog` | Construct a form node |
| `formK bpm nodes` | Realize form into Kinetics signals |
| `ki (lo, hi) k pat` | Range gate: mask by kinetics signal |
| `slate range k pats` | Gated stack (ki + stack) |
| `withForm k f` | Apply function with reactive progression |
| `kSignal k` | Kinetics level pattern (0–1) |
| `kDynamic k` | Dynamic envelope pattern (0–1) |
| `kProg k` | Active progression pattern |

### MIDI Helpers (BootTidal.hs)

| Helper | Usage |
|--------|-------|
| `ch n` | MIDI channel |
| `vel n` | Velocity |
| `oct n` | Octave shift (`\|+ oct 1`, `\|- oct 2`) |
| `setbpm n` | Set tempo |

___

For more examples, see:
- [`live/examples/blue_in_green.tidal`](live/examples/blue_in_green.tidal) — jazz progression with scale/melody separation
- [`live/examples/rosslyn_castle.tidal`](live/examples/rosslyn_castle.tidal) — AABA form with transformation support

For architecture and technical details, see [ARCHITECTURE.md](ARCHITECTURE.md).
For installation and project overview, see [README.md](README.md).
