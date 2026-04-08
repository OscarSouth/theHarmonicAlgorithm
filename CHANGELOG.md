# Changelog

## V3 (2026-03-13)

V3 is a ground-up rebuild of The Harmonic Algorithm. What started as a
command line interface for navigating harmonic space has become a live
performance instrument — deeply integrated with TidalCycles, capable of
channelling the harmonic sensibilities of over 80 composers from the Yale
Classical Archives Corpus, and designed to be played in real time.

### New Musical Possibilities

**Composer Blending** — You can now channel different composers' harmonic
sensibilities and blend them with weighted ratios. `"bach"` gives you strong
functional harmony. `"debussy"` brings colourful modal movement.
`"debussy:0.75 bach:0.25"` creates a stylistic fusion that neither would
have written alone. The wildcard `"*"` aggregates across the entire corpus.
Blended composers even get a creative portmanteau name in the output.

**Five Voicing Strategies** — Each one reshapes how a progression sounds
without changing the harmony itself. `flow` finds the smoothest voice leading
using cyclic dynamic programming. `grid` keeps the root locked in the bass
with optimised upper voices. `lite` gives you the raw intervals. `root`
extracts just the root note as a melodic line. `fund` always returns the
harmonic fundamental regardless of inversion — essential for kick drums and
sub bass.

**Entropy as a Creativity Dial** — A single parameter controls the balance
between the familiar and the surprising. At 0.3, the algorithm follows the
most common cadences — safe, consonant, Bach-approved. At 0.8, it reaches
deeper into the probability space for unexpected harmonic turns and distant
modulations. The same starting chord can lead to radically different
musical outcomes.

**Groove Interface** — `subKick` creates MPC-style kick and sub bass patterns
that follow the harmonic root of the current chord. Rhythm and harmony
unified — the low end always locks to the harmony, even as the progression
changes underneath.

**Explicit Construction** — You don't always need the algorithm to generate
for you. Build progressions by hand with pitch-class lists or readable note
names (`notesToPCs [C, E, G]`). Define musical sections (A, B) and assemble
them into different forms — AABA, ABAB, AABB — hearing the same material
create different structures.

**Progression Manipulation** — Reshape generated harmony in real time:
`rotate` shifts the starting point, `excerpt` pulls out a section,
`transposeP` shifts everything to a new key, `reverse` plays it backwards,
`fuse` joins progressions end to end, `expandP` slows the harmonic rhythm.
All composable — chain them together for complex transformations.

**Three-Part Filtering** — Constrain the harmonic space with overtones,
key signatures, and root motion filters. Bass guitar tuning (`"E A D G"`),
jazz contexts (`"*" "2#" "D F# A"`), blues roots (`"*" "1b" "F Bb C"`) —
the filtering system shapes the musical character as much as the notes
themselves. Removal syntax (`"-Bb'"`) subtracts specific pitches.

**Bass Direction** — Force the bass line to move in a single direction
with `"0# rise"` or `"0# fall"` in the roots string. The algorithm
selects the closest allowed bass note above (rise) or below (fall) the
current one, creating stepwise ascending or descending bass motion through
the filtered pitch space. Append a number 2–6 to skip notes: `"0# rise2"`
moves by thirds, `"0# rise3"` by fourths, up to `"0# rise6"` for sixths.
The step walks through the allowed set in order, wrapping around if it
exceeds the set size. Combine with key and root filters to shape the
path — `"*" "1b 4 5" "key rise"` produces a rising bass line through
the mixolydian degrees.

**Dissonance Drift** — Shape the harmonic tension arc across a
progression. `dissonant ctx` filters each generation step to candidates
with equal or greater dissonance than the current chord, building tension
over time. `consonant ctx` does the opposite, gradually resolving toward
simpler harmony. The filter applies after pool construction but before
selection, preserving the normal scoring hierarchy — you get the best
musical choice that meets the constraint, not just the most or least
dissonant option. Composes with bass direction:
`consonant (dissonant ctx)` — last modifier wins.

**Inversion Spacing** — Control how often chord inversions appear in
generated progressions. `invSkip 2 $ ctx` ensures at least 2
root-position chords between any two inversions. `invSkip 0` (default)
allows inversions freely. The starting state counts toward the spacing —
a root-position start with `invSkip 1` means the first generated step
can already be an inversion. Composes with all other context modifiers.

**`lead` — String-Parsed Starting State** — `lead :: String -> IO CadenceState`
constructs a starting chord from a human-readable string. Root, quality, and
movement can each be omitted to fall through to randomness. `lead "E min (5)"`
gives E minor arrived by ascending 5th. `lead "E"` gives E with a random
quality and random movement. `lead ""` is fully random. Recognised quality
shorthands include `maj`, `min`, `dim`, `aug`, `7`, `dom7`, `maj7`, `min7`,
`hdim`, `sus2`, `sus4`, `6`, `m6` — all other names try to match the internal
quality map directly. Prints `root quality` to the console on construction.

**Modifier-Based Context API** — `hContext` is now a zero-argument
chromatic default. Filters are applied as composable modifier functions:
`hcOvertones "E A D G"`, `hcKey "0#"`, `hcRoots "C E G"`. Wildcard
filters are simply omitted instead of explicitly passed as `"*"`.
Individual lines can be commented out to fall back to defaults.
`dissonant`, `consonant`, and `invSkip N` compose naturally in the
same chain: `invSkip 2 $ consonant $ hcKey "0#" $ hContext`.

**Modifier-Based Gen API** — `gen`, `gen'`, and `gen''` are now bare
config values (`GenConfig` type) instead of multi-argument functions.
Parameters are applied as composable modifiers: `cue` (starting state),
`len` (progression length), `entropy` (gamma shape), `tonal` (harmonic
context). `seek` is the terminal modifier that executes generation:
`s <- seek "*" $ cue start $ tonal ctx $ len 4 $ entropy 0.3 $ gen`.
The composer string is passed to `seek` rather than embedded in the
config, keeping it visible at the call site. Follows the same pattern
as the `hContext` modifier rework — individual modifiers can be
commented out to fall back to defaults. The old positional interface
remains available as `genPrint`, `genPrint'`, `genPrint''`, and
`genSilent`/`genStandard`/`genVerbose` are unchanged.

**Pattern Launcher Paradigm** — Define reusable instrument blocks with
four parameters: transformation, progression, chord selection, and dynamics.
Stack multiple voices with independent voicing, register, and rhythm.
Launch and relaunch through a session with different progressions and
transformations.

### Under the Hood

The generation engine was rebuilt for reliability and performance:
- Voice leading uses cyclic dynamic programming for globally optimal
  voicings across the full progression (including wrap-around)
- A shared RNG eliminates thousands of `/dev/urandom` reads per generation
- Pre-parsed harmonic contexts use `IntSet` for O(1) membership tests
- The Builder module was split into focused sub-modules (Types, Core,
  Diagnostics, Portmanteau) with a facade re-export preserving the API
- Several correctness fixes ensure the algorithm produces what it claims —
  `mostConsonant` now matches the canonical Dissonance module, unknown
  note names throw errors instead of silently defaulting, and all
  generation functions use identical candidate pools

**Form & Kinetics Framework** — Macro-level compositional arc is now
programmable structure. Define forms in wall-clock seconds that loop
endlessly, with continuous interpolation for kinetics and dynamics, and
discrete step functions for progression switching.

- `FormNode` and `Kinetics` types encode form as data: time, kinetics
  level (0–1), dynamic level (0–1), and active progression at each node
- `at` constructor and `formK` realization: define nodes with `at`,
  realize them into looping TidalCycles patterns with `formK bpm nodes`
- `ki` range gating: `ki (lo, hi) k` masks patterns by kinetics signal
  level — elements only active when `kSignal` is within range
- `slate` gated stack: combine `ki` + `stack` for drum layers that
  activate at different intensity levels
- `withForm` reactive switching: apply a function taking `Progression`
  to a `Kinetics` context via `innerJoin`
- Forms are now inline declarations with snippet templates — see
  `live/snippets.cson` for spectral narrative and pop form snippets
- Single-state forms produce constant signals, recreating "formless"
  behaviour: `formK tempo [at 0 1 1 s]`
- `arrange` and `arrange'` now take a kinetics range, progression
  modifier, and `Kinetics` context — enabling form-driven range gating
  and reactive progression switching within arrangement blocks
- `subKick` now takes `Kinetics` instead of a raw `Progression`,
  with built-in ki gating on sub (0.1–1) and kick (0.2–1) groups
- Launcher paradigm updated: `f s r d` → `f r d k` — progression is
  read from kinetics context, not passed directly

### Performance & Hardware Updates (2026-04)

**Voice Leading Cache** — `arrange` and `arrange'` now pre-compute voicings
at pattern construction time. All unique progressions in `kProg k` are
resolved once; per-frame lambdas do a list lookup instead of running the
cyclic DP solver. With 16+ stacked instrument calls (full orchestral mode),
this reduces voice leading work from ~800 solves/second to 2–3. Skip
messages in full orchestral mode drop to negligible levels.

**subKick Hardware Routing** — `subKick` now routes to MIDI channel 10
on the `"thru"` device. Sub pitches map to MIDI C2–B2 (36–47) via
pitch class → `pitch_class + 36`, placing the sub register below all
orchestral instrument ranges with no overlap. Kick is fixed at C3 (MIDI 48).
The CC64 sustain mechanism and `sustain 0.01` architecture are unchanged.

**Frame Rate & Segment Density** — Frame timespan set to `1/30` (~33ms)
for better headroom with complex pattern graphs. `segment` density reduced
across Form.hs (16), BootTidal `lfo` (16), and subKick `sustainOn` (16)
— all remain musically sufficient for their respective purposes.

### Migration from V2

- The public API is preserved through facade re-exports — existing code
  should work unchanged
- `GeneratorConfig` has been simplified to a single field (`gcPoolSize`)
- The experimental homing mechanism has been removed (it was never
  part of the musical interface)
