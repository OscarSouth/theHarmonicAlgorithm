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
using cyclic dynamic programming. `root` keeps the root in the bass with
optimised upper voices. `lite` gives you the raw intervals. `bass` extracts
just the lowest voice as a melodic line. `fund` always returns the harmonic
root regardless of inversion — essential for kick drums and sub bass.

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

### Migration from V2

- The public API is preserved through facade re-exports — existing code
  should work unchanged
- `GeneratorConfig` has been simplified to a single field (`gcPoolSize`)
- The experimental homing mechanism has been removed (it was never
  part of the musical interface)
