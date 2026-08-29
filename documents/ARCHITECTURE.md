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
learned across 488 composers of the Yale Classical Archives Corpus
(YCACL), and plays them through TidalCycles. Generation:

1. Cadence transitions are stored in Neo4j as a weighted graph.
2. Harmonic constraints define what is *valid* (overtones, key, roots,
   pedal tones, drift, inversion spacing).
3. Candidates are scored — corpus weight and dissonance per step;
   voice-leading cost at progression scoring and arrangement.
4. The next chord is sampled probabilistically, entropy controlling how
   far down the ranking the sampler will reach.
5. The result is arranged into TidalCycles patterns (this is where
   cyclic-DP voicing runs).

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
  │   graph weight · dissonance               │
  └───────────────────┬───────────────────────┘
                      ▼
  ┌───────────────────────────────────────────┐
  │ T: TRAVERSAL — choose                     │
  │   gamma sampling (entropy)                │
  └───────────────────┬───────────────────────┘
                      ▼
  Output state (next chord) → appended, then repeat
```

### The three principles

1. **The Harmonic Algorithm** — the generative engine described here:
   an R→E→T pipeline whose ancestry runs from an exhaustive
   combinatorial mapping of overtone triads (2016), through a diagnosis
   under Wiggins' Creative Systems Framework (Wiggins 2006; the
   diagnosis made in the 2018 paper), to a Markov walk over a Neo4j
   graph with gamma-distribution sampling (V3).
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
| **Evaluation (E)** | Which valid things are *good*? | Corpus edge weights and Hindemith dissonance per step; voice-leading cost at the whole-progression level (`attempt`) and in the Arranger |
| **Traversal (T)** | Which one do we *take*? | The per-step graph walk, sampled by a gamma distribution under an entropy parameter |

The separation is the point. Mixing constraint and preference makes a
system brittle; keeping them apart means the same R can be traversed
differently (change entropy, change nothing else), and a new E can be
dropped in without touching what counts as valid.

### What the pipeline order actually is

In Wiggins' formalism R, E and T are three *rule sets*, and the
generator consumes all three at once — no stage ordering is implied by
the framework. "R→E→T" here is the *reading order of the simplest
step*: for plain `gen` with overtone/key/roots filters, one step of
`stepChainBody` (`Framework/Builder/Core.hs`) literally filters, then
scores, then samples. The full system deviates from that order in four
designed ways, and it is more useful to know them than to pretend the
arrow is an invariant:

1. **Soft constraints filter after scoring.** `drift`, `invSkip` and
   pedal preference run over the *scored* pool (`Core.hs` step body),
   so their path is R→E→R→T — and all three relax to the unfiltered
   pool rather than emptying it, making them advisory rather than hard
   R (the overtone/key/roots core is hard).
2. **The fallback fuses E and T.** The 660-candidate constructive
   fallback's score includes a small gamma perturbation
   (`computeFallbackScoreWithBoost`), deliberately breaking score ties
   so the pool needs no size cap. A little T lives inside that E.
3. **`genP` runs T first.** The strata walk is computed for the whole
   progression up front and then *defines* per-bar R and biases E —
   T→R→E→T. (`OCTATRIPENTATONICS.md` describes the per-bar layer;
   this is the step before it.)
4. **`attempt N K` runs E after T.** Whole progressions are generated,
   then scored by a second, differently-weighted evaluation
   (`Evaluation/Scoring/Progression.hs` — the only place voice-leading
   cost enters evaluation) and the best is kept: (R→E→T)ⁿ → E.

None of these breaks the framework — they are what "T consults R and E"
looks like in a real system. What the separation guarantees is not a
fixed stage order but that each rule set can be changed without
rewriting the others.

### Aberration, and why this generator cannot do it

Wiggins requires E to be *independent* of R: if taste can only approve
what the rules already permit, no rule-breaking output can ever be
valued, and the rules can never legitimately change. Output that
violates R is **aberration**; aberration that E values anyway is
*productive*, and it is the one mechanism by which R gets rewritten.

In this codebase R is enforced as a pre-scoring filter over both
candidate sources, so the generator structurally cannot aberrate — it
will never offer a chord outside its own rules. The aberration channel
is human: `lead` (`Interface/Tidal/Arranger.hs`) takes no
`HarmonicContext` and validates nothing, so a hand-typed chord enters
the progression regardless of the active rules; `genFrom`, splicing and
`genGrid` likewise move material without re-validation. Every widening
of R in this project's history has come through that channel. One
narrow, deliberate exception exists inside the generator itself: with
`rise`/`fall` active, the targeted bass pitch class is exempted from
the overtone check (`Core.hs`, `matchesContextWithTarget`) to allow
chromatic passing bass notes — a designed crack in R, kept small on
purpose.

The channel stays open but is no longer silent: when a Fresh or grid
generation's starting state escapes the active context, a one-line
non-fatal notice is printed at emission time
(`printCueEscapeNotice`, `Builder.hs`) — `⚠ cue escapes R: contains F#
A# C# outside the key/overtone set` — once per invocation, including
under `attempt`. The cue is honoured regardless; the notice makes the
aberration visible at the moment it happens. Relatedly, the diagnostic
trace's *Candidates* line lists only chords present in the final
sampled pool — a chord excluded by R or by an advisory filter at that
step is never displayed as if it had been available.

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

**Toolchain.** Stackage **lts-24.56** / **GHC 9.10.3**, language edition
GHC2021 with a project-wide `OverloadedStrings` default, building against
**TidalCycles 1.10.3** and **Neo4j 5.26** over the HTTP Query API.
`stack.yaml` sets `system-ghc: true`, so the compiler on PATH is the one
used. The library compiles warning-clean under `-Wall`.


```
src/Harmonic/
│
├── Lib.hs                      [public API re-export]
├── Config.hs                   [Neo4j connection + corpus paths]
├── Database.hs                 [HTTP Query API transport]
│
├── Framework/                  [R→E→T orchestration]
│   ├── Builder.hs              [facade; gen/genE/genP/genJ/genFrom families]
│   └── Builder/
│       ├── Types.hs            [HarmonicContext, GenConfig, diagnostics types]
│       ├── Core.hs             [chain building, candidate pools, filtering]
│       ├── Modifiers.hs        [the modifier chain: cue/len/seek/entropy/…]
│       ├── Strata.hs           [strata-walk placement + adjacency]
│       ├── StrataGen.hs        [the genP runner]
│       ├── JazzGen.hs          [the genJ runner]
│       ├── PolyGen.hs          [the genE runner (polytonal partner pass)]
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
│       ├── Jazz.hs             [jazz chord-symbol parser + bass vocabulary]
│       ├── Merge.hs            [transition-count merging]
│       └── Graph.hs            [Neo4j schema writes]
│
├── Evaluation/                 [E component]
│   ├── Scoring/
│   │   ├── Dissonance.hs       [Hindemith interval vectors]
│   │   ├── VoiceLeading.hs     [voice-leading cost, cyclic DP solvers]
│   │   └── Progression.hs      [whole-progression scoring for rank-and-select]
│   ├── Database/Query.hs       [Neo4j queries with composer weights]
│   └── Analysis/
│       ├── Markov.hs           [transition probability computation]
│       └── KeyArea.hs          [key-area Viterbi; chordscale S/M derivation]
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
    ├── PolytonalT.hs           [polytonal layer-view reporting (genEReport)]
    ├── ChordscaleT.hs          [chordscale key/mode/pentatonic report]
    ├── Instruments.hs          [MIDI channel routing helpers]
    ├── Motif.hs                [motivic development: >:<, mirror, retro]
    ├── Display.hs              [12 Step LED feed: bar/seconds CC arithmetic]
    ├── Devices/S1.hs           [Roland AIRA S-1 CC map]
    ├── Devices/P6.hs           [Roland AIRA P-6 CC and pad map]
    ├── Devices/JV1010.hs       [JV-1010 drum map, continuo bank addressing]
    └── Utils.hs                [pattern/time utilities]
```

One subsystem has its own document and is only sketched here:

- **Octatripentatonics** (`Scale`, `ProgressionContext`, `Strata`,
  `OctatripentatonicT`) — harmony carried in three densities at once,
  walked through a curated space of interlocking pentatonic sets.
  See [OCTATRIPENTATONICS.md](OCTATRIPENTATONICS.md).

### Walking bass (`WalkingBass`, `LineHarmony`)

Linearises harmony into a quarter-note bass line, adapted from Gary
Willis's *Fingerboard Harmony for Bass*.
`walkLine` is a pure function of `(voiceFn, progression)` in three
passes:

1. **Beat 1s** — the notated root (or `fund`) of every bar, placed by a
   greedy nearest-to-previous chain with a soft direction-persistence
   bias, a half-weight loop-closure pull on the final bar, and a
   root–fifth alternation option inside runs of repeated chords. Bar 0
   anchors at the register centre.
2. **Beat 3s** — the chord tone minimising smoothness plus a consonance
   table (P5 ≺ root ≺ 3rds ≺ 7ths ≺ colour tones), so strong beats stay
   grounded anchors.
3. **Beats 2/4** — weighted connectors from the key-area palette
   ('Analysis.KeyArea.barPalettes' — per-bar mode sets from the same
   whole-progression analysis that derives the chordscale S/M layers; plus
   chromatic neighbours of the target): sandwich motion preferred,
   leading tones and root/P5 approaches rewarded on beat 4, copy and
   repeat gates. Progression-level consonance (`progConsonance`) scales
   strictness; derived entropy (`progressionEntropy`) drives the repeat
   probability.

`walkLineP` is the octatripentatonic variant: the connector pool becomes
the bar's strata ∪ mode ∪ neighbour-triad overlap (closed — no chromatic
outsiders) with three-tier preference.

`walkLineJ` is the jazz variant, and the reason it exists is that a
corpus chord symbol describes a working voicing rather than a bass
player's reading of it: 13th chords carry no 5th and no 11th, altered
qualities put a #5 or b5 where the 5th would be, and notated colours
(b9, #9, #11, b13) are tension to pass through, not tones to land on.
`Rules.Import.Jazz.bassVocabFor` derives that reading per bar — the
triadic **target** tones a line aims at, the defining seventh, the
favourable passing extensions, the avoid tones, and *the* fifth
(restored where the symbol implies one the voicing omits). The vocabulary
then corrects every fifth-driven term, tiers the beat-3 pool (target ≺
seventh ≺ passing ≺ palette) and the beat-2 preference, and keeps avoid
tones off strong beats while leaving them reachable as weak-beat colour.
The same triad/extension logic feeds key detection, which since the
chordscale unification is `Analysis.KeyArea`: a cyclic Viterbi over 24
key areas (12 major + 12 composite minor — one tonic realised per bar as
its natural / harmonic / melodic form) with probe-calibrated switch
penalties and boundary bonuses at dominant-approach and tonic-arrival.
Extensions still sharpen the reading — an altered dominant votes a minor
tonic, a #11 over a major seventh rules out the subdominant — but as
emission evidence inside a whole-progression optimisation rather than a
per-bar vote window. One detector feeds both the walk's palettes and the
chordscale layers (full theory: [CHORDSCALE.md](CHORDSCALE.md)); under a
reordering chord selection the walk re-analyses the performed sequence,
deliberately.

`LineHarmony` selects the variant per context — strata chroma for
`FStrata`, bass vocabulary for `FJazz`, plain tone sets otherwise — with
the chosen side-channel folded into the cache key so lines never collide.
It wraps whichever variant in the cached Tidal interface and resolves the
**performed** bar sequence from
the chord-selection pattern, so warped/repeated bars are walked in the
order the audience hears them (non-periodic selections fall back to
stored order). The register is a 21-semitone window (MIDI 28–48); its
absolute octave at the speaker depends on the synth patch via
`tidalNoteOffset`.

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
data Layer = T | S | M | TS | TM | SM | TSM | PT

data ProgressionContext = ProgressionContext
  { triadLayer   :: Progression
  , strataLayer  :: Progression
  , modeLayer    :: Progression
  , pcProvenance :: Maybe (Seq (Tristrata, StrataLabel))
  , pcFamily     :: Family
  }
```

`arrange` takes the layer as an argument, so the same pattern can read
any view of one generated result. `T`/`S`/`M` project the stored
layers; `TS`/`TM`/`SM`/`TSM` synthesize pointwise pitch-class unions
per bar, rooted on the lowest constituent layer (T before S before M —
the foundation owns a merged bass); `PT` synthesizes the pivot tones
every layer shares. The strata-first `genP` family populates the layers
with curated strata/mode chromas; the polytonal `genE` family (below)
fills them with independent partner triad chains; `gen` and `genJ`
derive theirs by chordscale key-area analysis — S the bar's best-fit
anhemitonic pentatonic, M the detected key as the mode on the bar's
root ([CHORDSCALE.md](CHORDSCALE.md)). Only raw hand-built contexts
(`fromChords`/`fromProgression` without `chordscale`) still duplicate
the triad across all three layers.

### 5.3a The genE family (polytonal generation)

A distinct generator family (`genE`/`genE'`/`genE''`, family-aware
`genFrom`, `Harmonic.Framework.Builder.PolyGen`, family tag `FPoly`).
Full theory and the viability study behind the design:
`documents/POLYTONAL.md` and `archive/analysis/poly_viability.md`.

The **foundation walk** (T layer) is byte-identical to `gen` — the same
chain builder, and the only place R constraints (key, roots, rise/fall
direction, drift, pedal, inversion spacing) apply: the foundation owns
the bass. The **partner pass** then walks two partner triad chains over
the finished foundation. Each partner bar continues from its OWN
previous bar's transition list (every partner bar is a real graph edge
of its own layer's history), filtered to the overlap rules against that
bar's foundation triad: share exactly 2 pitch classes with it, union of
all three exactly 5. Those rules admit two per-bar geometries — the
traversal chooses freely between them: **common-dyad** (all three
triads share one dyad; every layer pair sounds 4 tones) and
**base-anchored** (partners take different foundation dyads; T-pairs
sound 4 tones, S+M the pentad). Partners honour the harmonic space
(key / allowed roots / overtones, via the same R predicate with no bass
target) but never the direction specs or strata machinery.

Jointly valid (S, M) pairs are ranked by summed own-list rank and drawn
with one entropy-scaled gamma over the pair pool; supply relaxes from
the corpus lists through a space-constrained pure enumeration over all
220 absolute 3-PC sets to an unconstrained enumeration floor, so
partner selection is total — under a crushing context the foundation
degrades exactly as `gen` does (absorbing repetition), never the
partners. S/M identity is assigned once at the end (lower whole-layer
dissonance total = S); a partial regen (`genFrom`) preserves the
source's labelling and seeds the partner chains from the kept bars.

The corpus graph is 3-set only; the **shadow projection**
(`walkTriadCadence` — every state projected to its most-consonant
rooted embedded triad for graph fetch keys, R filters, and drift
comparisons; identity for all ≤3-interval states) remains for the
places >3-note material still enters a walk: regen cues over hand-built
`lead'` progressions, and the genJ classical steer. Corpus-shaped
means `corpusFunctionality`'s 55-form table (Harmony.hs), transcribed
from the live graph: the database was ingested under legacy naming
(e.g. `[0,3,8]` → `maj_1stInv`), which the modernised zero-form namers
deliberately diverge from, so fetch keys cannot be derived from them.
The same table fixed a latent bug in `gen` itself: `constructCadence`
previously read inversions back under modern names, so every
graph-selected inversion silently dropped the walk to fallback for one
step.

Attempt scoring reads the triad layer for the three single-progression
families, and `cadenceFavFromMap` projects any >3-note bar through the same
shadow before keying the corpus map; voice-leading scoring evaluates the
triad skeleton (uniform 3-note comparisons, by design).

genE ranks differently, because it *is* three progressions: each layer is
scored in its own right and blended (0.5 foundation, 0.25 each partner),
and a divergence axis measures how far the layers stand apart — partner
geometry and root spread, averaged over bars. Divergence is the purpose of
the family, so it carries real weight rather than breaking ties; the
calibration, and the requirement that raising K raise both means, are
recorded on `polyDivergenceWeight`.

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
stepwise motion (`rise`/`fall`, with `<…>` step lists; whitespace before
the bracket is tolerated). All three contexts share one parser core, so
the grammar cannot diverge between them. The roots string additionally
accepts the magic values `key` (mirror the key filter) and `tones` (the
key-filtered overtone set). A token no branch recognises is ignored for
generation but named in a `⚠` warning printed once per generation — a
typo never silently reshapes the rule set. Overtone strings expand each
named fundamental to its playable set (root, fifth, third).

---

## 6. Database Schema

**Connection**: HTTP Query API at `http://localhost:7474` (`POST /db/neo4j/query/v2`), credentials `neo4j/password`
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

**Corpus**: Yale Classical Archives Corpus, 488 composers. Ingestion
extracts the fundamental of each slice and ranks its most consonant
triad interpretations with per-slice normalised weights. Transitions
are counted over slice TRIPLES — the cadence `a -> b` followed by
`b -> c` with the middle reading shared — so every consistent
interpretation path informs the model and alternative readings of one
moment are never mistaken for movement. `Analysis/Markov.hs` normalises
the counts into per-source transition probabilities before the graph
write.

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

The voice-leading engine (2026-08-20 pass) is seam-aware and
strictly-scoped:

- **Mixed cardinalities voice-lead for real.** A transition between bars
  of different sizes is costed by optimal monotone padding
  (`alignVoices`): the smaller sorted voicing is expanded by duplicating
  tones per the minimal-distance non-crossing alignment in which every
  voice of both chords participates, then the full cost function runs on
  the aligned pair. (Previously a flat 999 sentinel made such edges
  invisible to the DP — seam registers were arbitrary and the cyclic
  wrap objective silently vanished on mixed material.)
- **A held chord is strictly cheapest.** The cost total is floored at 1
  for any actual motion; bonuses (contrary, stepwise) discriminate among
  moving alternatives but can no longer beat stillness.
- **Chroma routing.** genP-provenance S/M layers, and the chordscale-
  derived S/M layers of gen/genJ contexts, are always voiced by
  `strataModeFlow` (lattice semantics: a fixed pitch lattice grounded
  on bar 0, each slot inflected onto the current bar's set by the
  minimum-cost bijection, so a held pattern index pedals its pitch and
  key changes arrive as accidentals, never as a jump to the new root);
  combination selectors over those contexts take the same
  route. Derived-layer detection requires a distinct mode layer whose
  every bar is chroma-sized (≥5 PCs) — so a substitution-downgraded genE
  context and a bar-substituted derived context both fall back honestly.
  genE contexts themselves never chroma-route: every selector —
  partners, pairs, the pentad, the pivot tones — honours the user's
  `VoiceFunction` through the real voice-leading DP.
  The general `flow`/`grid` DP handles all
  harmony-sized material — bars of ≤5 voices, uniform or mixed; ≥6-PC
  bars (hand-built scale sets) safety-route to the chroma engine
  (a 16-bar 7-PC DP costs ~107 s as bytecode and ~2.4 s compiled —
  slow either way, and answering the wrong musical question).
- **Attempt scoring measures the heard surface**: sorted absolute PCs at
  full cardinality (the previous extraction was unsorted and
  mod-12-wrapped, so root motion C→A measured 9 semitones; anchors were
  recalibrated empirically — see `Scoring/Progression.hs`).
- **Solves are shared across callers.** `solveFlow`/`solveRoot` are
  memoised on their input, so a 15-instrument Orchestra score solves each
  distinct progression once rather than 15 times. The memo sits on the
  solver rather than inside `arrange` because `lineHarmony ... grid`
  reaches the DP through `beat1PCs`, which an `arrange`-level cache would
  never see.
- **Compiled live sessions.** `live/bin/ghci` loads the library as `-O2`
  object code rather than bytecode. Measured on 5-PC bars: a 4-bar solve
  goes 0.317 s to 0.0065 s, 8-bar 0.958 s to 0.0212 s, 16-bar 2.390 s to
  0.0528 s (45–49×; `-O0` alone gives 7.7–10×). The object directory must
  be pinned. With no `-odir`/`-hidir` GHCi writes `.o` and `.hi` beside
  each source file, and every later session links those instead of
  compiling — after an edit, an ABI mismatch. The wrapper pins
  `.stack-work/live-odir`, private to it, so plain `stack ghci`,
  `stack build` and CI are untouched.

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
is the design ideal: filters do not rank, scorers do not exclude. Two
known, deliberate leaks cross it — `applyDriftFilter` excludes using a
dissonance score (an E function acting as a filter), and the composer
blend drops zero-weight transitions (a scorer acting as a filter,
because a move a composer never made is absent, not merely unfavoured).
Both are documented rather than hidden; anything new should hold the
line.

---

## Appendix: Glossary

| Term | Meaning |
|---|---|
| **Cadence** | A relative harmonic transition: function, root movement, zero-form intervals |
| **CadenceState** | A cadence plus its absolute root and enharmonic spelling |
| **Chord** | An absolute sonority: root plus intervals |
| **Confidence** | Per-edge weight stored on `:NEXT`, derived from corpus transition counts |
| **Drift** | Constraint on dissonance direction across a progression (`consonant`/`dissonant`) |
| **Entropy** | Dial (≥ 0) targeting rank `entropy·10` in the scored pool (gamma shape `entropy·10 + 0.5`, capped at pool size); how far down the ranking traversal reaches |
| **Form node** | A timed point carrying kinetics, dynamics and a progression |
| **Kinetics** | Continuous 0–1 intensity signal driving range-gated arrangement |
| **Key area** | A contiguous span governed by one key: 12 major + 12 composite minor candidates in the chordscale analysis |
| **Key form** | The per-bar realisation of a composite minor key: natural (relative-major set), harmonic, or melodic |
| **Chordscale** | Whole-progression key-area analysis deriving gen/genJ S (pentatonic) and M (mode) layers (`Analysis.KeyArea`) |
| **Layer (T/S/M/TS/TM/SM/TSM/PT)** | A view of one progression context: stored layer, pairwise/threefold union, or the shared pivot tones |
| **Partner chain** | A genE S/M layer: an independent corpus walk sharing tones with the foundation per bar |
| **Pivot tones (PT)** | The pitch classes common to all three layers of a bar (a dyad on common-dyad bars, the hub tone on base-anchored bars) |
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
| Compiled-session wrapper | `live/bin/ghci` |
| Session launcher | `live/bin/livecode` |
| SuperCollider startup | `live/superdirt_startup.scd` |
| Editor configuration | `live/pulsar/` (reference copies) |
| Interactive guide | `live/USER_GUIDE.tidal` |
| Neo4j compose config | `docker-compose.yml` |
| Package definition | `package.yaml` |
| Tests | `test/` (mirrors `src/` structure) |
