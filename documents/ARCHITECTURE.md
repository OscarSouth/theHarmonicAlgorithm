# theHarmonicAlgorithm — Architecture Guide

How the system is put together: the generative pipeline, the layering
that keeps it honest, and the data structures everything else is built
on. For usage, see [README.md](../README.md) and
[USER_GUIDE.md](../USER_GUIDE.md); for the harmonic theory behind the
newer subsystems, [OCTATRIPENTATONICS.md](OCTATRIPENTATONICS.md) and
[ALGORITHMIC_ORCHESTRATION.md](ALGORITHMIC_ORCHESTRATION.md).

---

## 1. Overview

theHarmonicAlgorithm generates harmonic progressions from transitions
learned across 80+ composers of the Yale Classical Archives Corpus
(YCACL), and plays them through TidalCycles. Generation:

1. Cadence transitions are stored in Neo4j as a weighted graph.
2. Harmonic constraints define what is *valid* (overtones, key, roots,
   pedal tones, drift, inversion spacing).
3. Candidates are scored — corpus weight, dissonance, voice leading.
4. The next chord is sampled probabilistically, entropy controlling how
   far down the ranking the sampler will reach.
5. The result is arranged into TidalCycles patterns.

```
                 R→E→T GENERATION PIPELINE

  Input state (root, intervals, spelling)
            │
            ▼
  ┌───────────────────────────────────────────┐
  │ R: RULES — define the valid space         │
  │   overtone · key · roots · pedal ·        │
  │   drift · inversion spacing · strata      │
  └───────────────────┬───────────────────────┘
                      ▼
  ┌───────────────────────────────────────────┐
  │ E: EVALUATION — score the valid           │
  │   graph weight · dissonance ·             │
  │   voice-leading cost                      │
  └───────────────────┬───────────────────────┘
                      ▼
  ┌───────────────────────────────────────────┐
  │ T: TRAVERSAL — choose                     │
  │   gamma sampling (entropy) ·              │
  │   cyclic-DP voicing                       │
  └───────────────────┬───────────────────────┘
                      ▼
  Output state (next chord) → appended, then repeat
```

### The three principles

1. **The Harmonic Algorithm** — the generative engine described here:
   an R→E→T pipeline whose ancestry runs from an exhaustive
   combinatorial mapping of overtone triads (2016), through a diagnosis
   under Wiggins' Creative Systems Framework (2018), to a Markov walk
   over a Neo4j graph with gamma-distribution sampling (V3).
2. **Algorithmic Orchestration** — scoring for a virtual orchestra
   through TidalCycles, with musical elements abstracted into harmony,
   form and instrument interfaces. See
   [ALGORITHMIC_ORCHESTRATION.md](ALGORITHMIC_ORCHESTRATION.md).
3. **The Spectral Narrative** — macro-level compositional arc as data:
   form nodes carrying kinetics, dynamics and progression, placed in
   wall-clock seconds or bars, with smooth or snap transitions
   (Section 6).

---

## 2. The Creative Systems Framework (R→E→T)

The architecture implements Wiggins' framework, which models creative
generation as three separable components:

| Component | Question | In this system |
|---|---|---|
| **Rules (R)** | What is *valid*? | Constraint filters over pitch-class sets, chord structures and root motion |
| **Evaluation (E)** | Which valid things are *good*? | Corpus edge weights, Hindemith dissonance, voice-leading cost |
| **Traversal (T)** | Which one do we *take*? | Gamma-distribution sampling under an entropy parameter |

The separation is the point. Mixing constraint and preference makes a
system brittle; keeping them apart means the same R can be traversed
differently (change entropy, change nothing else), and a new E can be
dropped in without touching what counts as valid.

| Layer | Modules | Responsibility |
|---|---|---|
| Rules (R) | `Rules/Types/`, `Rules/Constraints/`, `Rules/Import/` | Valid pitch-class sets, chord structures, filter parsing, corpus ingestion |
| Evaluation (E) | `Evaluation/Scoring/`, `Evaluation/Database/`, `Evaluation/Analysis/` | Dissonance, voice-leading cost, graph queries with composer weights |
| Traversal (T) | `Traversal/` | Gamma sampling; walking-bass line synthesis |
| Orchestration | `Framework/Builder*` | Runs the R→E→T loop, threads state, emits diagnostics |
| Interface | `Interface/Tidal/` | TidalCycles bridge, voicings, form, instruments |

---

## 3. Four-Layer Architecture

The codebase is organised into four vertical layers — Memory → Brain →
Hands → Voice — with dependencies flowing strictly downward.

```
┌──────────────────────────────────────────────┐
│ Layer D: VOICE — TidalCycles interface       │
│   pattern bridge · voicings · form/kinetics  │
│   groove · walking lines · orchestra         │
└──────────────────┬───────────────────────────┘
                   │ depends on
┌──────────────────▼───────────────────────────┐
│ Layer C: HANDS — evaluation & traversal      │
│   graph queries · dissonance · voice leading │
│   Markov analysis · probabilistic selection  │
└──────────────────┬───────────────────────────┘
                   │ depends on
┌──────────────────▼───────────────────────────┐
│ Layer B: BRAIN — music theory types          │
│   ℤ₁₂ pitch algebra · harmony naming ·       │
│   progressions · scales/strata vocabulary    │
└──────────────────┬───────────────────────────┘
                   │ depends on
┌──────────────────▼───────────────────────────┐
│ Layer A: MEMORY — rules & corpus ingestion   │
│   CSV parsing · transformation · graph write │
│   constraint specification · overtone sets   │
└──────────────────────────────────────────────┘
```

**Boundary rule**: B imports nothing from C or D; C may import from B
but not D; D may import from B and C. This is what keeps the music
theory reusable independently of both the database and TidalCycles.

**Layer A — Memory** (`Rules/Import/`, `Rules/Constraints/`). Ingests
the corpus and defines the valid space. The ingestion path is
`CSV → ChordSlice → Cadence → Neo4j NEXT edges`, run once via
`stack run`.

**Layer B — Brain** (`Rules/Types/`). Foundational algebra. Types
enforce invariants: `PitchClass` is a newtype over ℤ₁₂ that cannot hold
a value ≥ 12, constructed through `mkPitchClass`.

**Layer C — Hands** (`Evaluation/`, `Traversal/`, `Framework/`). Scores
and chooses. Entropy controls exploration: low values exploit the
top-weighted transitions, high values reach further down the ranking.

**Layer D — Voice** (`Interface/Tidal/`). Turns progressions into
patterns. Key mechanism: **modulo wrap** — progressions are finite,
TidalCycles patterns are infinite, so `lookupChord` wraps indices
(`chords !! (idx mod length)`), letting any pattern run over any
progression indefinitely.

---

## 4. Module Structure

```
src/Harmonic/
│
├── Lib.hs                      [public API re-export]
├── Config.hs                   [Neo4j connection + corpus paths]
│
├── Framework/                  [R→E→T orchestration]
│   ├── Builder.hs              [facade; gen/genP/genFrom families]
│   └── Builder/
│       ├── Types.hs            [HarmonicContext, GenConfig, diagnostics types]
│       ├── Core.hs             [chain building, candidate pools, filtering]
│       ├── Strata.hs           [strata-walk placement + adjacency]
│       ├── Diagnostics.hs      [diagnostic rendering]
│       └── Portmanteau.hs      [composer-name blending]
│
├── Rules/                      [R component + Layer A/B]
│   ├── Types/
│   │   ├── Pitch.hs            [ℤ₁₂ pitch-class algebra]
│   │   ├── Harmony.hs          [chord/cadence types and naming]
│   │   ├── Progression.hs      [progression monoid]
│   │   ├── Scale.hs            [modes, pentatonics, strata/tristrata vocabulary]
│   │   └── ProgressionContext.hs [the three bar-aligned layers + provenance]
│   ├── Constraints/
│   │   ├── Filter.hs           [filter-string parsing; overtone/key/root sets]
│   │   └── Overtone.hs         [triad generation from overtone sets]
│   └── Import/
│       ├── CSV.hs              [YCACL CSV parsing]
│       ├── Transform.hs        [ChordSlice → Cadence]
│       ├── Types.hs            [import data types]
│       └── Graph.hs            [Neo4j schema writes]
│
├── Evaluation/                 [E component]
│   ├── Scoring/
│   │   ├── Dissonance.hs       [Hindemith interval vectors]
│   │   ├── VoiceLeading.hs     [voice-leading cost, cyclic DP solvers]
│   │   └── Progression.hs      [whole-progression scoring for rank-and-select]
│   ├── Database/Query.hs       [Neo4j queries with composer weights]
│   └── Analysis/Markov.hs      [transition probability computation]
│
├── Traversal/                  [T component]
│   ├── Probabilistic.hs        [gamma-distribution sampling]
│   └── WalkingBass.hs          [walking-line note choice: strong beats, passing tones]
│
└── Interface/Tidal/            [Layer D]
    ├── Bridge.hs               [pattern lookup, arrange/arrange', warp/rep]
    ├── Arranger.hs             [voicing strategies + progression combinators]
    ├── Form.hs                 [kinetics framework: nodes, gating, IK context]
    ├── Groove.hs               [subKick, fund]
    ├── LineHarmony.hs          [walking bass as a Tidal interface]
    ├── Orchestra.hs            [instruments, voices, divisi, sections]
    ├── OctatripentatonicT.hs   [strata provenance reporting helpers]
    ├── Instruments.hs          [MIDI channel routing helpers]
    └── Utils.hs                [pattern/time utilities]
```

Two subsystems have their own documents and are only sketched here:

- **Octatripentatonics** (`Scale`, `ProgressionContext`, `Strata`,
  `OctatripentatonicT`) — harmony carried in three densities at once,
  walked through a curated space of interlocking pentatonic sets.
  See [OCTATRIPENTATONICS.md](OCTATRIPENTATONICS.md).
- **Walking bass** (`WalkingBass`, `LineHarmony`) — linearises harmony
  into a bass line: chord tones on strong beats, constrained passing
  motion between them, direction held until the register runs out.

---

## 5. Core Concepts

### 5.1 The zero-form invariant

All cadences are stored as **relative** intervals starting at
pitch-class 0.

```
ABSOLUTE (Chord)                RELATIVE (Cadence)
Root + intervals                Movement + zero-form intervals

C major:  C + [0,4,7]           up 0  + [P 0, P 4, P 7]
F major:  F + [0,4,7]           up 5  + [P 0, P 4, P 7]
G major:  G + [0,4,7]           up 2  + [P 0, P 4, P 7]
                                        ↑ same structure
```

Why: analysis becomes transposition-invariant, the corpus stops being
biased toward frequently-written keys, the state space shrinks by a
factor of twelve, and what gets learned is harmonic *movement* rather
than chord identity.

Enforced at Neo4j query results, at `toCadence` conversion, and in
fallback generation. Concrete pitch classes are recovered at runtime by
adding the current root.

### 5.2 State threading

Generation threads a `CadenceState` (`Rules/Types/Harmony.hs`):

```haskell
data CadenceState = CadenceState
  { stateCadence     :: Cadence             -- function, movement, zero-form intervals
  , stateCadenceRoot :: NoteName            -- absolute root
  , stateSpelling    :: EnharmonicSpelling  -- flat or sharp
  }
```

The relative cadence carries the learned structure; the absolute root
and spelling carry what the listener actually hears and what the
diagnostics print.

### 5.3 Progressions and the three layers

A `Progression` is a `Seq CadenceState` with a `Monoid` instance. The
richer `ProgressionContext` (`Rules/Types/ProgressionContext.hs`) carries
three bar-aligned layers plus optional provenance:

```haskell
data Layer = T | S | M          -- triad, strata, mode

data ProgressionContext = ProgressionContext
  { triadLayer   :: Progression
  , strataLayer  :: Progression
  , modeLayer    :: Progression
  , pcProvenance :: Maybe (Seq (Tristrata, StrataLabel))
  }
```

`arrange` takes the layer as an argument, so the same pattern can read
chords, pentatonics or modes from one generated result. The legacy
`gen` family fills all three layers with the triad; the strata-first
`genP` family populates them distinctly.

### 5.4 HarmonicContext

The constraint record (`Framework/Builder/Types.hs`) — the R component,
built by composing modifier functions over the chromatic default
`hContext`:

```haskell
data HarmonicContext = HarmonicContext
  { _hcOvertones        :: Text   -- pitch palette ("*" = all)
  , _hcKey              :: Text   -- key signature
  , _hcRoots            :: Text   -- allowed bass notes
  , _hcDrift            :: Drift  -- consonant / dissonant / none
  , _hcInversionSpacing :: Int    -- min non-inversions between inversions
  , _hcPedal            :: Text   -- required/preferred tones ("C G?")
  , _hcTristrata        :: Text   -- tristrata allow-list ("" = all twelve)
  }
```

```haskell
ctx = invSkip 2 $ consonant $ hcPedal "C?" $ hcKey "0#" $ hcOvertones "E A D G" $ hContext
```

Each filter string supports the wildcard `*`, subtraction (`-Bb'`),
prime notation for exact pitches (`E'`), and — for roots — forced
stepwise motion (`rise`/`fall`). Overtone strings expand each named
fundamental to its playable set (root, fifth, third).

---

## 6. Database Schema

**Connection**: `bolt://localhost:7687`, credentials `neo4j/password`
(local development defaults, see `Config.hs` and `docker-compose.yml`).

**Node** — one per distinct zero-form cadence, uniquely keyed by its
`show` representation:

```cypher
(:Cadence {
  show:       String,   -- canonical rendering; UNIQUE constraint
  movement:   String,   -- root movement class
  chord:      String,   -- chord rendering
  dissonance: Float     -- precomputed Hindemith score
})
```

**Relationship** — one per observed transition, carrying a per-composer
weight map:

```cypher
(:Cadence)-[:NEXT { confidence: Float, weights: <composer→weight map> }]->(:Cadence)
```

Queries match `(c:Cadence {show: $show})-[r:NEXT]->(n:Cadence)` and
resolve `r.weights` against the caller's composer blend — `"*"` sums
across the corpus, a name selects one, `"bach:30 debussy:70"` mixes with
those coefficients.

**Corpus**: Yale Classical Archives Corpus, 80+ composers. Ingestion
extracts the fundamental of each slice, generates the most consonant
triad interpretations, weights them, and cross-multiplies adjacent
slices into transitions. `Analysis/Markov.hs` normalises counts into
transition probabilities before the graph write.

---

## 7. TidalCycles Integration

Layer D turns a progression into playable pattern. Three ideas carry
most of it.

**The bridge.** `arrange (lo,hi) k (-9,9) LAYER voicing modifier
[patterns]` maps patterns of *voicing degrees* across the progression.
Degrees are indices into whatever chord is sounding, so a pattern
re-realises itself against changing harmony — which is what makes a
motif survive reharmonisation. `arrange'` compresses the whole pattern
into each chord slot instead of letting it flow across them.

**Voicings.** Five strategies (`Arranger.hs`): `flow` (any inversion,
smoothest motion, solved by cyclic dynamic programming over the
progression *including* the wrap back to the first chord), `grid` (root
locked in the bass, smooth above), `lite` (raw intervals), `root`, and
`fund` (harmonic fundamental, for sub and kick).

**Form and kinetics.** A form is a list of nodes (`Form.hs`), each
placed in seconds (`at`, `at'`) or bars (`rh`, `rh'`) and carrying three
signals: kinetics (0–1 intensity), dynamics (level), and the
progression in force. Between nodes the signals interpolate — or hold
and snap, with the primed constructors. `iK tempo nodes chordSelection`
bundles this into the `IK` context every launcher takes; `lK` builds the
same context from live signals instead of keyframes. Kinetics gate
`arrange` calls by range, so instruments enter and leave as the arc
crosses their thresholds.

---

## 8. Design Principles

**Types enforce invariants.** Illegal states are unrepresentable where
practical: `PitchClass` cannot exceed ℤ₁₂; zero-form is maintained at
every boundary where cadences enter the system.

**Pure core, effects at the edges.** Music theory, scoring and voicing
are pure functions. `IO` appears only for database access, randomness
and pattern output — which is why the theory layers are testable
without Neo4j or TidalCycles running.

**Composable modifiers over configuration objects.** Contexts and
generation configs are built by composing small functions
(`invSkip 2 $ consonant $ hcKey "0#" $ hContext`), so any modifier can
be commented out and the remainder still means something.

**Eager forcing at the boundary.** Progression voicings and walking
lines are computed and forced at REPL evaluation time, not on the audio
thread — `arrange` and `lineHarmony` build and deeply force their caches
before returning a pattern, keeping lazy evaluation from deferring work
into the moment a note is due.

**Separation of validity from preference.** The R→E→T split (Section 2)
is applied consistently: filters never rank, scorers never exclude.

---

## Appendix: Glossary

| Term | Meaning |
|---|---|
| **Cadence** | A relative harmonic transition: function, root movement, zero-form intervals |
| **CadenceState** | A cadence plus its absolute root and enharmonic spelling |
| **Chord** | An absolute sonority: root plus intervals |
| **Confidence** | Per-edge weight stored on `:NEXT`, derived from corpus transition counts |
| **Drift** | Constraint on dissonance direction across a progression (`consonant`/`dissonant`) |
| **Entropy** | Shape parameter of the gamma sampler; how far down the ranking traversal reaches |
| **Form node** | A timed point carrying kinetics, dynamics and a progression |
| **Kinetics** | Continuous 0–1 intensity signal driving range-gated arrangement |
| **Layer (T/S/M)** | Triad, strata or mode density of one progression context |
| **Modulo wrap** | Index wrapping that lets infinite patterns run over finite progressions |
| **Movement** | Classified root motion between two chords |
| **Strata / Tristrata** | Canonical pentatonic set / curated group of three whose pair-unions are diatonic |
| **Voicing** | Distribution of a chord's pitches across register |
| **Zero-form** | Intervals normalised to begin at pitch-class 0 |

---

## File Locations

| What | Where |
|---|---|
| Public API | `src/Harmonic/Lib.hs` |
| Generation entry points | `src/Harmonic/Framework/Builder.hs` |
| Constraint types | `src/Harmonic/Framework/Builder/Types.hs` |
| TidalCycles boot file | `live/BootTidal.hs` |
| Interactive guide | `live/USER_GUIDE.tidal` |
| Neo4j compose config | `docker-compose.yml` |
| Package definition | `package.yaml` |
| Tests | `test/` (mirrors `src/` structure) |
