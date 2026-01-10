# The Harmonic Algorithm - AI Agent Instructions

## Project Overview
`theHarmonicAlgorithm` is a Haskell + R toolchain that mines the Yale Classical Archives Corpus (YCACL) and stores cadence transitions in Neo4j for later TidalCycles performance. The system follows a four-layer architecture:

| Layer | Name | Role | Key Modules |
|-------|------|------|-------------|
| **A** | Memory | Ingestion – R export, CSV parsing, Neo4j writes | `export_ycacl.R`, `Ingestion/*`, `Graph.hs` |
| **B** | Brain | Types – pitch-class algebra, harmony naming, voice leading | `Pitch.hs`, `Harmony.hs`, `Overtone.hs`, `VoiceLeading.hs` |
| **C** | Hands | IO – database queries, builder logic, probabilistic selection | `Query.hs`, `Builder.hs`, `Probabilistic.hs` |
| **D** | Voice | Interface – TidalCycles bridge, pattern lookup, arrangement | `Interface.hs`, `Arranger.hs`, `BootTidal.hs` |

## Layer Constraints

### Layer A (Memory) — Ingestion
- Database stores **relative movements** (pitch-agnostic). The `movement` and `chord` fields are text representations that can be transposed at runtime.
- Fundamentals are trusted inputs from the exporter; every ingestion slice reads `sliceFundamental` instead of recomputing bass.
- Multi-triad duplication replaces fractional weighting: most consonant appears 3×, then 2×, then 1× in cadence stream.

### Layer B (Brain) — Types
- **Strict types**: `PitchClass` newtype with ℤ₁₂ algebra (Num, Monoid instances).
- **Constructive generation** via `possibleTriads`: triads built from overtone sets rather than enumerated.
- **Legacy nameFunc fidelity**: `nameFuncTriad` and `nameFuncChord` ported verbatim from legacy with minimal fixes.
- `zeroForm` subtracts the FIRST element (not minimum), matching legacy behavior.

### Layer C (Hands) — IO
- Follows **R→E→T pipeline** order (Rules filter → Evaluation rank → Traversal walk).
- **Gamma sampling** for weighted selection; distribution shape controls exploration vs exploitation.
- **Homing logic** activates at 75% threshold to guide progressions toward resolution.
- Composer weights support three formats: space-separated equal, colon-weighted, comma-separated.

### Layer D (Voice) — Interface
- `lookupChord` uses **modulo wrap** for infinite pattern cycling (index 4 on 4-chord prog → 0).
- `arrange` preserves **launcher paradigm** compatibility for TidalCycles integration.
- Visual `Show` instance displays **4-column grid** with bar labels for Progression type.

## Architecture Snapshot

### Layer A — Memory (Ingestion)

1. **R Exporter** (`theHarmonicAlgorithm/scripts/export_ycacl.R`)
	- Scans YCACL CSVs, normalizes composer names (currently Debussy & Stravinsky enabled).
	- Parses every note token, keeping accidentals and octave context to distinguish tokens like `B-1` vs `B--1`.
	- Orders pitch classes low→high, dedupes doublings, and emits a `fundamental` pitch-class column (0–11). Ultra-low pedals that simply double the same pitch class within two octaves are promoted to their higher counterparts.

2. **Haskell Ingestion** (`theHarmonicAlgorithm/src/Harmonic/Ingestion/*`)
	- `Harmonic.Ingestion.CSV` loads the exporter artifact into nested maps of `ChordSlice` records (pitches + fundamental).
	- `Harmonic.Ingestion.Transform` derives cadences by generating every triad rooted on the slice fundamental, ranking by Hindemith dissonance, and duplicating the top three choices 3/2/1 times. Adjacent slices are cross-multiplied so the Markov corpus reflects alternate harmonic interpretations without fractional weights.

3. **Pipeline Orchestration** (`theHarmonicAlgorithm/app/Main.hs`)
	- Logs per-composer coverage, cadences, and transition counts.
	- Invokes an APOC batched delete (`apoc.periodic.iterate`) to clear `:Cadence`/`:NEXT` data before every reload, then rebuilds schema and writes transitions.

4. **Storage** (`theHarmonicAlgorithm/docker-compose.yml`, `src/Harmonic/Database/Graph.hs`)
	- Single Neo4j container with APOC enabled; cadence nodes carry `{show, movement, chord, dissonance}` while `r.weights` stores per-composer totals as JSON strings (still valid after duplicating triads because counts remain integers).
	- **CRITICAL:** Database stores relative movements (pitch-agnostic)—`movement` and `chord` are text, transposable at runtime.

### Layer B — Brain (Types)

**Core Modules** (`theHarmonicAlgorithm/src/Harmonic/Core/*`)
- `Harmonic.Core.Pitch` — `PitchClass` newtype with ℤ₁₂ algebra (Num, Monoid instances), constructors `pc`/`mkPitchClass`, `transpose`.
- `Harmonic.Core.Harmony` — Chord/triad naming with **separate** functions:
	- `nameFuncTriad` — verbatim port from legacy (for exactly 3 pitches, with inversion detection)
	- `nameFuncChord` — verbatim port from legacy (for extended harmonies, no reduction)
	- `toTriad`/`flatTriad`/`sharpTriad` — reduce >3 pitches via `mostConsonant`, use `nameFuncTriad`
	- `toChord`/`flatChord`/`sharpChord` — preserve all pitches, use `nameFuncChord`
	- `normalForm`, `primeForm`, `zeroForm`, `toMovement` — set-theory operations
- `Harmonic.Core.Overtone` — `Overtones` newtype, `fromPitchClasses`, Hindemith dissonance ranking, **constructive generation** via `possibleTriads`.
- `Harmonic.Core.VoiceLeading` — Cyclic DP voice leading (`solveRoot`, `solveFlow`), parallel fifths/octaves penalty, leap penalty.
- `Harmonic.Core.Progression` — `Progression` type with Semigroup/Monoid, rotation, excerpt, transpose, expand, fuse combinators. Visual **4-column grid** Show instance.
- `Harmonic.Core.Probabilistic` — Gamma distribution sampling for weighted selection; `gammaIndex`, `gammaSelect`, `gammaSequence`, `weightedSelect`.
- `Harmonic.Core.Filter` — Legacy filter notation parser:
	- `parseOvertones` — tuning/overtone filter: `"E A D G"`, `"C"`, `"G E' A' A#'"`
	- `parseKey` — key signature filter: `"#"`, `"##"`, `"1b"`, `"2b"`, `"C"`, `"F#"`
	- `parseFunds` — root notes filter: `"E F# G"`, `"1#"`, `"##"`
	- Wildcards: `"*"`, `"all"`, `"chr"` (all match everything)
- `Harmonic.Core.MusicData` — Legacy monolith (still used by ingestion pipeline).

### Layer C — Hands (IO)

**Database & Builder Modules** (`theHarmonicAlgorithm/src/Harmonic/Database/*`, `src/Harmonic/Tidal/*`)

Implements the **CSF (Creative Systems Framework)** R→E→T pipeline:
- **R (Rules):** `HarmonicContext` constrains overtone sets, key filters, and root motion filters
- **E (Evaluation):** Gamma-weighted selection ranks candidates by consonance and context fit
- **T (Traversal):** Markov walks through Neo4j cadence graph produce progressions

Modules:
- `Harmonic.Database.Query` — Neo4j read interface:
	- `parseComposerWeights` — parses "bach debussy" (equal) or "bach:30 debussy:70" (weighted)
	- `fetchTransitions` — retrieves NEXT edges with composer weights
	- `resolveWeights` — applies composer blend to raw weights
	- `applyComposerBlend` — normalizes composer contributions
- `Harmonic.Tidal.Builder` — Generation engine with **unified interface**:
	- `HarmonicContext` — R constraints (overtones, key, roots) — three-part filtering
	- Filter notation from legacy Overtone.hs: `"*"` = wildcard, `"1#"` = G major, `"2b"` = Bb major, `"##"` = D major, note names like `"c"`, `"eb"`
	- `GeneratorConfig` — homing threshold (75%), strength, minimum candidates
	- **Three unified generation functions** (identical signatures, variable output):
		- `genSilent` — no diagnostics (production use)
		- `genStandard` — standard diagnostics (exploration use)
		- `genVerbose` — verbose diagnostics with transform/advance traces (debugging use)
	- **With custom config** (`'` suffix variants):
		- `genSilent'`, `genStandard'`, `genVerbose'` — same as above but with custom `GeneratorConfig`
	- **Internal functions** (for advanced use):
		- `generate'`, `generate''` — return tuple `(Progression, GenerationDiagnostics)` for manual extraction
		- `genWith'`, `genWith''` — with custom config
	- **Diagnostics utility**:
		- `printDiagnostics` — reprint diagnostics at any time

### Layer D — Voice (Interface)

**TidalCycles Bridge** (`theHarmonicAlgorithm/src/Harmonic/Tidal/*`)
- `Harmonic.Tidal.Interface` — TidalCycles bridge:
	- `lookupProgression` — fetches stored progression by ID
	- `lookupChord` — index-based lookup with **modulo wrap** (index 4 on 4-chord prog → 0)
	- `voiceBy` — extracts voice (flow, root, bass) from chord
	- `VoiceType` — `Flow | Root | Bass` enum for voice extraction
	- `arrange` — applies voicing strategy to progression, preserves **launcher paradigm**

## Unified Generation Interface (Layer C)

The generation engine provides a **unified interface** with three functions sharing identical type signatures:

```haskell
genSilent   :: CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression
genStandard :: CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression
genVerbose  :: CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression
```

All three return `IO Progression` (not tuples); diagnostics are printed as side effects based on verbosity level.

### Verbosity Levels

- **0 (Silent)**: No diagnostic output; fastest execution. Use for production music-making.
- **1 (Standard)**: Per-step candidate pools, selections, and rendered chord names. Use for exploration and tuning.
- **2 (Verbose)**: Standard diagnostics plus transform and advance traces (DB intervals, transposition, PC arithmetic, enharmonic spelling). Approximately 20-30% slower. Use only for debugging.

### Usage Pattern

**Identical signatures enable seamless switching:**

```haskell
-- Start with exploration
prog <- genStandard start 8 "*" 0.5 ctx

-- Switch to production (one word change)
prog <- genSilent start 8 "*" 0.5 ctx

-- Switch to debugging (one word change)
prog <- genVerbose start 4 "*" 0.5 ctx
```

### Custom Configuration Variants

All three functions have `'` suffix variants accepting custom `GeneratorConfig`:

```haskell
genSilent'   :: GeneratorConfig -> CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression
genStandard' :: GeneratorConfig -> CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression
genVerbose'  :: GeneratorConfig -> CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression
```

Example:
```haskell
let cfg = defaultConfig { cfgHomingThreshold = 0.8 }
prog <- genStandard' cfg start 8 "*" 0.5 ctx
```

### Internal Tuple Functions

For advanced use cases requiring manual diagnostics extraction:

```haskell
generate'   :: ... -> IO (Progression, GenerationDiagnostics)
generate''  :: ... -> IO (Progression, GenerationDiagnostics)
genWith'    :: ... -> IO (Progression, GenerationDiagnostics)
genWith''   :: ... -> IO (Progression, GenerationDiagnostics)
printDiagnostics :: Int -> GenerationDiagnostics -> IO ()
```

- `generate'` and `genWith'` return standard-level diagnostics
- `generate''` and `genWith''` return maximum diagnostics (with full traces)
- `printDiagnostics` reprints diagnostics at any verbosity level

### Diagnostics Output Format

**Standard diagnostics (verbosity 1)** include:
- Per-step prior/posterior cadence states and roots
- Graph and fallback candidate pool counts
- Top 3 candidates from each pool with confidence scores
- Selected source (graph or fallback) and gamma-weighted index
- Selected movement and rendered chord name

**Verbose diagnostics (verbosity 2)** add:
- **Transform traces**: DB intervals, transposition, normalization, zero-form, detected root, computed vs. stored name
- **Advance traces**: Prior/posterior root PCs, movement interval, PC arithmetic (mod 12), enharmonic spelling decision, final root note

### Documentation

- **UNIFIED_INTERFACE.md** — Complete reference for all generation functions
- **UNIFIED_INTERFACE_DESIGN.md** — Design philosophy and rationale
- **USER_GUIDE.md** — Workflow guide with practical examples
- **Haddock comments** in `src/Harmonic/Core/Builder.hs` — Full API documentation

### Test Suite (`theHarmonicAlgorithm/test/Harmonic/*`)

**Layer B Tests** (`test/Harmonic/Core/`):
- `PitchSpec` — ℤ₁₂ wraparound, Monoid laws, transpose.
- `HarmonySpec` — Golden tests for triad/chord naming (maj, min, dim, aug, sus2, sus4, sus2/4, 6ths, 7ths), inversions, normalForm, primeForm, toMovement with boundary-crossing edge cases.
- `OvertoneSpec` — Dissonance ranking invariants.
- `VoiceLeadingSpec` — Cost metrics, parallel fifths/octaves penalty, leap penalty, boundary-crossing voice leading.
- `ProgressionSpec` — Semigroup/Monoid laws, rotation, excerpt, transpose, expand, fuse.
- `FilterSpec` — Legacy filter notation parsing (overtones, keys, roots, wildcards).

**Layer C Tests**:
- `ProbabilisticSpec` — Gamma sampling statistics, distribution shape, weighted selection.
- `QuerySpec` (`test/Harmonic/Database/`) — Composer weight parsing (equal, weighted, comma formats).
- `BuilderSpec` — HarmonicContext wildcards, GeneratorConfig validation.

**Layer D Tests**:
- `InterfaceSpec` (`test/Harmonic/Tidal/`) — TidalCycles pattern extraction, modulo wrap, VoiceType enum.

- **Run with:** `stack test` (301 examples, 0 failures).

## Workflow Highlights

- **Build**: `cd theHarmonicAlgorithm && stack build`
- **Test**: `cd theHarmonicAlgorithm && stack test`
- **Export YCACL**: `Rscript scripts/export_ycacl.R ../musicdata/YCACL ../musicdata/YCAC-metadata.csv/YCAC-metadata.csv data/ycacl_sequences.csv`
- **Run ingestion**: `stack run` (auto-clears Neo4j cadence subgraph, repopulates via duplicated triad cadences).
- **Verify DB**: `docker exec theHarmonicAlgorithm-neo4j cypher-shell -u neo4j -p password "MATCH (c:Cadence) RETURN count(c)"`

## Conventions & Decisions

### Enharmonic Spelling (`selectEnharm`)

The system implements **persistence-based context-aware enharmonic spelling** via the `selectEnharm` function in `Harmonic.Core.Harmony`. Four-tier priority:

1. **Same Pitch Class** (highest priority)
   - If prior and posterior pitch classes are identical, use prior's actual spelling
   - Example: D# → D# keeps Sharp, Eb → Eb keeps Flat (regardless of preferences)

2. **C is Flexible** (pitch class 0)
   - C has no preference, can adopt adjacent pitches' preferences
   - Prior is C + posterior is definite → adopt posterior's preference
   - Posterior is C + prior is definite → adopt prior's actual spelling
   - Both C → persist current spelling

3. **Both Definite (different pitch classes)** — Consensus-Based Switching
   - **Prior actual matches posterior preference** → persist (consensus on what was chosen)
   - **Both have SAME preference AND it differs from prior actual** → switch (consensus to switch)
   - **Preferences disagree** → persist prior's actual (no consensus)

4. **Examples:**
   - D (SharpPref, actual Flat) + Eb (FlatPref) → disagree → persist Flat
   - D (SharpPref, actual Flat) + E (SharpPref) → both prefer Sharp → switch to Sharp!
   - Db (FlatPref, actual Flat) + Db (FlatPref) → same PC → persist Flat
   - C (ambiguous, actual Sharp) + D (SharpPref) → C is flexible → adopt posterior Sharp

**Implementation:** Called in `Builder.hs:advanceStateTraced` with `selectEnharm spelling currentRootPC newRootPC`, where `spelling` is the actual prior spelling being used. The function checks both pitch class preferences and matches them against the actual spelling to determine switches.

### Core Conventions

- Only triads with ≥3 distinct pitch classes participate; slices exceeding 7 voices are filtered at export time.
- Fundamentals are now trusted inputs from the exporter, so every ingestion slice should read `sliceFundamental` instead of recomputing the bass.
- Multi-triad duplication replaces fractional weighting: the most consonant candidate appears three times in the cadence stream, then two, then one—so Markov counts remain integer while still privileging clearer interpretations.
- Neo4j writes are additive across composers; MERGE keeps existing cadences, and `truncateCadenceGraph` must run before each ingestion to guarantee determinism.
- **Layer B naming functions are SEPARATE**: `nameFuncTriad` for triads, `nameFuncChord` for extended harmonies—ported verbatim from legacy with minimal fixes (e.g., sus2 detection for root position `[0,2,7]`).
- **zeroForm subtracts the FIRST element** (not minimum), matching legacy behavior.
- **Layer C composer weights** support three formats: space-separated equal ("bach debussy"), colon-weighted ("bach:30 debussy:70"), and comma-separated.
- **Layer D lookupChord uses modulo wrap**: index 4 on a 4-chord progression returns index 0, enabling infinite cycling through patterns.
- **Layer C HarmonicContext** uses three-part filtering (overtones, key, roots) with legacy Overtone.hs notation: `"*"` = wildcard, `"1#"` = G major, `"2b"` = Bb major, `"##"` = D major, note names like `"c"`, `"eb"`.
- **Layer C filter notation** fully supports the original README format: `"E A D G"` for bass tuning overtones, `"G E' A' A#'"` for combined overtones + individual pitches, key signatures like `"#"`, `"bb"`, `"1#"`, `"2b"`.
- **Layer C/D String wrappers for TidalCycles**: Use `hContext` (not `HarmonicContext`), `gen`/`genWith` (not `generate`/`generateWith`), `overtones` (not `parseOvertones`), `key` (not `parseKey`), `funds` (not `parseFunds`), `wildcard` (not `isWildcard`) in the Tidal REPL to avoid `Stringy Text` conflicts.
- **Layer D fuse vs interleave**: `fuse` concatenates a list of progressions (`[Progression] -> Progression`), matching legacy behavior. `fuse2` is binary convenience (`Progression -> Progression -> Progression`). `interleave` alternates chords between two progressions.

## Debug Tips

- **Exporter**: If `fundamental` is NA, the parser skipped the row—look for malformed note tokens in YCACL (search for stray characters).
- **Ingestion**: If cadences explode unexpectedly, ensure `possibleTriads''` is fed the deduped pitch set; repeated values can inflate overtone lists.
- **Neo4j**: Port binding errors mean another compose stack is already running; `docker compose ps` inside `theHarmonicAlgorithm/` shows the active container.
- **Tests**: Run `stack test` to validate Layer B, C, and D modules. Use `stack ghci` with qualified imports (`import qualified Harmonic.Core.Harmony as H`) to avoid ambiguity with legacy `MusicData`.
- **Layer C Query**: Use `parseComposerWeights "bach:30 debussy:70"` in GHCi to test weight parsing; result should show normalized Map with values summing to 1.0.
- **Layer C Builder**: Test `defaultContext` and `defaultConfig` in GHCi; context should show `"*"` wildcards, config should show threshold=0.75.
- **Layer C Generation**: Use `genStandard start 8 "*" 0.5 ctx` to explore generation flow; switches to `genVerbose` for full traces when debugging chord naming or voice leading.
- **Layer C Diagnostics**: Call `printDiagnostics 1 diag` or `printDiagnostics 2 diag` to reprint diagnostics at different verbosity levels after extraction.
- **Layer D Interface**: Test `lookupChord prog 4` where prog has 4 chords; should return chord at index 0 (modulo wrap).

## Unified Interface Conventions

- **Primary functions** (high-frequency): `genSilent`, `genStandard`, `genVerbose` — use these for all generation
- **Custom config** (`'` suffix): `genSilent'`, `genStandard'`, `genVerbose'` — when tuning homing, strength, or candidate thresholds
- **Internal functions**: `generate'`, `generate''`, `genWith'`, `genWith''` — only for manual diagnostics processing
- **Diagnostics printing**: `printDiagnostics` — reprint diagnostics after extraction or batch processing
- **String wrappers for TidalCycles**: In live code, use `gen`/`genWith` (not `generate`/`generateWith`) to avoid conflicts with TidalCycles' own `generate`

Keep this document updated whenever the data flow, composer scope, module structure, generation interface, or triad-weighting strategy changes.
