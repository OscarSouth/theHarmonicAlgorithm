## The Harmonic Algorithm

`theHarmonicAlgorithm` is an environment for generating harmonic progressions from the Yale Classical Archives Corpus (YCACL), storing cadence transitions inside Neo4j, and reusing that graph as the live-coding backend (via TidalCycles/SuperCollider).

### Prerequisites

- [Stack](https://docs.haskellstack.org/) with a working GHC toolchain (project currently uses GHC 9.6.7).
- R with the tidyverse installed (for the YCACL exporter).
- Docker (or another Neo4j runtime). The repo ships with `docker-compose.yml` that exposes Bolt on `7687` with `NEO4J_AUTH=neo4j/password`.

### Quick start

```bash
stack build          # compile
stack test           # run test suite (301 examples)
docker compose up -d neo4j
stack run            # populate Neo4j with cadence graph
```

### Architecture Overview

The system is organized into four layers:

| Layer | Name | Role | Key Modules |
|-------|------|------|-------------|
| **A** | Memory | Ingestion – R export, CSV parsing, Neo4j writes | `export_ycacl.R`, `Ingestion/*`, `Graph.hs` |
| **B** | Brain | Types – pitch-class algebra, harmony naming, voice leading | `Pitch.hs`, `Harmony.hs`, `Overtone.hs`, `VoiceLeading.hs` |
| **C** | Hands | IO – database queries, builder logic, probabilistic selection | `Query.hs`, `Builder.hs`, `Probabilistic.hs` |
| **D** | Voice | Interface – TidalCycles bridge, pattern lookup, arrangement | `Interface.hs`, `Arranger.hs`, `BootTidal.hs` |

#### Layer Constraints & Zero-Form Invariant

- **CRITICAL:** All Cadence objects store intervals in **zero-form** `[P 0, ...]` (relative, pitch-agnostic). This enables transposition: `posterior chord = rootPC + cadenceIntervals`.
- **Layer A (Memory):** Database stores zero-form cadences normalized during ingestion. All three codepaths (DB query, toCadence, fallback generation) enforce zero-form normalization.
- **Layer B (Brain):** Strict types (`PitchClass` newtype with ℤ₁₂ algebra), **constructive generation** via `possibleTriads` (no enumeration), legacy `nameFunc` fidelity preserved. `zeroFormPC` ensures semantic consistency across all cadence sources. **Enharmonic spelling** determined by `selectEnharm` with context-aware rules (see Conventions section below).
- **Layer C (Hands):** Follows **R→E→T pipeline** order (Rules filter → Evaluation rank → Traversal walk). Three-part `HarmonicContext` filtering (overtones, key, roots). Gamma sampling for weighted selection. Homing logic activates at 75% threshold. Fallback generation produces zero-form cadences matching database format. Enharmonic spelling carries through state transitions via `advanceStateTraced`.
- **Layer D (Voice):** `lookupChord` uses **modulo wrap** for infinite pattern cycling. `arrange` preserves launcher paradigm compatibility. Visual `Show` instance displays 4-column grid with bar labels. Enharmonic spelling determined by CadenceState and propagated through voicing.

### Layer B Core Modules

The `src/Harmonic/Core/` directory contains modular, well-typed music theory primitives:

| Module | Purpose |
|--------|---------|
| `Pitch.hs` | `PitchClass` newtype with ℤ₁₂ algebra (Num, Monoid), constructors `pc`/`mkPitchClass`, `transpose` |
| `Harmony.hs` | Chord/triad naming with **separate** `nameFuncTriad` (3 pitches) and `nameFuncChord` (extended); `normalForm`, `primeForm`, `zeroForm`, `toMovement` |
| `Overtone.hs` | `Overtones` newtype, `fromPitchClasses`, Hindemith dissonance ranking |
| `VoiceLeading.hs` | Cyclic DP voice leading, `solveRoot`/`solveFlow`, parallel fifths penalty |
| `Progression.hs` | `Progression` type with Semigroup/Monoid, rotation, excerpt, transpose, expand, fuse |
| `MusicData.hs` | Legacy monolith (still used by ingestion pipeline) |

### Layer C Hands Modules

Layer C implements the **CSF (Creative Systems Framework)** pipeline: **R** (Rules) → **E** (Evaluation) → **T** (Traversal).

| Module | Purpose |
|--------|---------|
| `Probabilistic.hs` | Gamma distribution sampling for weighted selection; `gammaIndex`, `gammaSelect`, `gammaSequence` |
| `Builder.hs` | Main generation engine; `HarmonicContext` (R constraints), `GeneratorConfig`, `generate` function |
| `Query.hs` | Neo4j read interface; `parseComposerWeights`, `fetchTransitions`, `resolveWeights` |
| `Interface.hs` | TidalCycles bridge; `lookupProgression`, `voiceBy`, modulo wrap, `arrange` |

### Layer D Voice Modules

Layer D provides the TidalCycles integration and visual output:

| Module | Purpose |
|--------|---------|
| `Interface.hs` | Pattern lookup with **modulo wrap** (index 4 on 4-chord prog → 0), `lookupProgression`, `voiceBy` |
| `Arranger.hs` | Voicing strategies, `arrange` preserves launcher paradigm |
| `BootTidal.hs` | TidalCycles boot script with progression helpers |

**Composer Weight Parsing:**
```haskell
parseComposerWeights "bach debussy"      -- Equal weights: 0.5/0.5
parseComposerWeights "bach:30 debussy:70" -- Weighted: 0.3/0.7
```

**Harmonic Context (R constraints):**

The filter notation from the original Harmonic Algorithm CLI is fully supported:

| Filter | Purpose | Examples |
|--------|---------|----------|
| **Overtones** | Limit to overtone series of fundamentals | `"E A D G"` (bass tuning), `"C"`, `"*"` |
| **Prime (')** | Add individual pitch classes | `"E'"`, `"G E' A' A#'"` (G overtones + E, A, A#) |
| **Key (sharps)** | Key signature filtering | `"#"` (G), `"##"` (D), `"1#"`, `"2#"` |
| **Key (flats)** | Key signature filtering | `"b"` (F), `"bb"` (Bb), `"1b"`, `"2b"` |
| **Named key** | Key by name | `"C"`, `"G"`, `"F#"`, `"Bb"` |
| **Roots** | Limit bass notes | `"E F# G"`, `"1b"`, `"##"` |
| **Wildcard** | Match all | `"*"`, `"all"`, `"chr"` |

```haskell
defaultContext                           -- "*" wildcards (no filtering)
harmonicContext "*" "1#" "1#"            -- G major key and root motion
harmonicContext "E A D G" "*" "*"        -- Bass tuning overtones
harmonicContext "G E' A' A#'" "*" "E G"  -- G overtones + blues notes, E/G roots
```

**Test coverage:** `stack test` runs 309 examples across all four layers, verifying:
- Layer B: ℤ₁₂ arithmetic, zero-form normalization, triad/chord naming, voice leading costs
- Layer C: Composer weight parsing, context filtering, fallback cadence generation (zero-form)
- Layer D: TidalCycles interface, modulo wrap, enharmonic rendering
All cadence sources (DB, toCadence, fallback) are verified to produce semantically consistent zero-form intervals.

### Data preparation

1. Ensure the YCACL CSV corpus and metadata exist under `musicdata/` (see `musicdata/YCACL` and `musicdata/YCAC-metadata.csv`).
2. From the project root run:

	```bash
	Rscript scripts/export_ycacl.R ../musicdata/YCACL ../musicdata/YCAC-metadata.csv/YCAC-metadata.csv data/ycacl_sequences.csv
	```

	The exporter normalizes composer names, filters to triads/quartal voicings of 3–7 voices, and writes a tall CSV (`composer,piece,order,pitches,fundamental`). The `fundamental` column stores the lowest pitch-class integer (0–11) inferred from each slice. When an ultra-low pedal tone merely doubles the same pitch class in higher registers (e.g., `A--1` under `A-1`/`A1`), the exporter promotes the higher voice so that the stored fundamental reflects the harmonic root rather than the duplicated pedal. The parser keeps the original high→low ordering quirks in mind, so runs like `B-1 B--1` retain the distinction between Bb/1 and Bb/−1 while we evaluate potential doublings.

### Populating Neo4j

1. Start the bundled database: `docker compose up neo4j` (or reuse an existing instance by editing `Harmonic.Config`).
2. Run the ingestion executable:

	```bash
	stack run
	```

	The `Main` module logs every composer, cadence count, and transition count before writing. Each run now triggers a memory-safe APOC purge (`apoc.periodic.iterate "MATCH (n:Cadence) RETURN n" "DETACH DELETE n" {batchSize:5000,parallel:true}`) so cadences and `NEXT` edges from previous sessions are cleared in batches before reloading the graph. Once the delete completes, the pipeline re-creates the schema constraint and writes the fresh transition set.

3. Validate the graph with `cypher-shell`:

	```bash
	docker exec theHarmonicAlgorithm-neo4j \
	  cypher-shell -u neo4j -p password "MATCH (c:Cadence) RETURN count(c)"
	docker exec theHarmonicAlgorithm-neo4j \
	  cypher-shell -u neo4j -p password "MATCH ()-[r:NEXT]->() RETURN count(r)"
	```

### Composer filtering

`composerInclude` in [app/Main.hs](app/Main.hs) lists every normalized YCACL composer. Only Bach, Debussy, and Stravinsky are uncommented by default; remove `--` prefixes to bring more composers into the training set. Use `composerExclude` for quick overrides when you want to keep the master list intact.

### Code map

**Layer A — Memory** (Ingestion)
- [scripts/export_ycacl.R](scripts/export_ycacl.R) — R exporter, constrained to include list, 3–7 voices
- [src/Harmonic/Ingestion/CSV.hs](src/Harmonic/Ingestion/CSV.hs) — loads the YCACL artifact
- [src/Harmonic/Ingestion/Transform.hs](src/Harmonic/Ingestion/Transform.hs) — converts pitch-class lists into cadences
- [src/Harmonic/Analysis/Markov.hs](src/Harmonic/Analysis/Markov.hs) — transition counts and probabilities
- [src/Harmonic/Database/Graph.hs](src/Harmonic/Database/Graph.hs) — Neo4j writer (stores relative movements, pitch-agnostic)

**Layer B — Brain** (`src/Harmonic/Core/`)
- [Pitch.hs](src/Harmonic/Core/Pitch.hs) — `PitchClass` newtype with ℤ₁₂ arithmetic
- [Harmony.hs](src/Harmonic/Core/Harmony.hs) — chord/triad naming (separate `nameFuncTriad` / `nameFuncChord`), set-theory ops
- [Overtone.hs](src/Harmonic/Core/Overtone.hs) — Hindemith dissonance ranking, constructive generation via `possibleTriads`
- [VoiceLeading.hs](src/Harmonic/Core/VoiceLeading.hs) — voice-leading cost, optimal voicing
- [Progression.hs](src/Harmonic/Core/Progression.hs) — `Progression` combinator library
- [Filter.hs](src/Harmonic/Core/Filter.hs) — legacy filter notation parser (overtones, keys, roots)
- [MusicData.hs](src/Harmonic/Core/MusicData.hs) — legacy monolith (still used by ingestion)
- [Probabilistic.hs](src/Harmonic/Core/Probabilistic.hs) — gamma distribution sampling for weighted selection

**Layer C — Hands** (`src/Harmonic/Database/`, `src/Harmonic/Tidal/`)
- [Query.hs](src/Harmonic/Database/Query.hs) — Neo4j read interface, composer weight parsing, transition resolution
- [Builder.hs](src/Harmonic/Tidal/Builder.hs) — generation engine, `HarmonicContext` (R constraints), R→E→T pipeline, `generate` function

**Layer D — Voice** (`src/Harmonic/Tidal/`)
- [Interface.hs](src/Harmonic/Tidal/Interface.hs) — TidalCycles bridge, `lookupProgression`, `voiceBy`, modulo wrap, `arrange`

**Layer B Tests** (`test/Harmonic/Core/`)
- [PitchSpec.hs](test/Harmonic/Core/PitchSpec.hs) — ℤ₁₂ wraparound, Monoid laws
- [HarmonySpec.hs](test/Harmonic/Core/HarmonySpec.hs) — triad/chord naming golden tests
- [OvertoneSpec.hs](test/Harmonic/Core/OvertoneSpec.hs) — dissonance ranking
- [VoiceLeadingSpec.hs](test/Harmonic/Core/VoiceLeadingSpec.hs) — cost metrics, parallel fifths
- [ProgressionSpec.hs](test/Harmonic/Core/ProgressionSpec.hs) — Semigroup/Monoid laws, combinators
- [FilterSpec.hs](test/Harmonic/Core/FilterSpec.hs) — legacy filter notation parsing

**Layer C/D Tests** (`test/Harmonic/`)
- [ProbabilisticSpec.hs](test/Harmonic/Core/ProbabilisticSpec.hs) — gamma sampling statistics
- [QuerySpec.hs](test/Harmonic/Database/QuerySpec.hs) — composer weight parsing
- [BuilderSpec.hs](test/Harmonic/Core/BuilderSpec.hs) — generation engine config
- [InterfaceSpec.hs](test/Harmonic/Tidal/InterfaceSpec.hs) — TidalCycles pattern tests, modulo wrap

**Live coding**
- [live/](live/) — TidalCycles boot scripts

## Conventions & Decisions

### Enharmonic Spelling (`selectEnharm`)

The system uses **persistence-based context-aware enharmonic spelling** to choose between equivalent pitches (C#/Db, D#/Eb, etc.). The `selectEnharm` function implements four-tier rules:

1. **Same Pitch Class** (highest priority)
   - If prior and posterior have the **same pitch class**, use prior's actual spelling
   - Example: D# → D# keeps Sharp, Eb → Eb keeps Flat

2. **C is Flexible**
   - C (pitch class 0) has no preference, adopts adjacent pitch preferences
   - Prior is C + posterior is definite → adopt posterior's preference
   - Posterior is C + prior is definite → adopt prior's actual spelling

3. **Both Definite (different pitch classes) - Consensus-Based**
   - If prior's actual spelling matches posterior's preference → persist (consensus)
   - If both have **same preference** AND it differs from prior's actual → switch (consensus to switch)
   - Otherwise → persist prior's actual spelling (disagreement)

**Example Trace:**
```
Db (FlatPref, actual Flat) → D (SharpPref) → disagree → persist Flat
D (SharpPref, actual Flat) → E (SharpPref) → both prefer Sharp → switch to Sharp!
E (SharpPref, actual Sharp) → Eb (FlatPref) → disagree → persist Sharp
Eb (FlatPref, actual Sharp) → Db (FlatPref) → both prefer Flat → switch to Flat
Db (FlatPref, actual Flat) → Db → same PC → persist Flat
```

This approach prevents spelling churn while enabling natural key transitions guided by pitch preferences and consensus.

### Other Conventions

- Only triads with ≥3 distinct pitch classes participate; slices exceeding 7 voices are filtered at export time.
- Fundamentals are trusted inputs from the exporter; ingestion reads `sliceFundamental` instead of recomputing bass.
- Multi-triad duplication replaces fractional weighting: most consonant appears 3×, then 2×, then 1× in cadence stream.
- Neo4j writes are additive across composers; MERGE keeps existing cadences, and truncation runs before each ingestion to guarantee determinism.
- **Layer B naming functions are SEPARATE**: `nameFuncTriad` for triads, `nameFuncChord` for extended harmonies—ported verbatim from legacy.
- **zeroForm subtracts the FIRST element** (not minimum), matching legacy behavior.
- **Layer C composer weights** support three formats: space-separated equal ("bach debussy"), colon-weighted ("bach:30 debussy:70"), and comma-separated.
- **Layer D lookupChord uses modulo wrap**: index 4 on a 4-chord progression returns index 0, enabling infinite cycling.
- **Layer C HarmonicContext** uses three-part filtering (overtones, key, roots) with legacy notation: `"*"` = wildcard, `"1#"` = G major, `"2b"` = Bb major, note names like `"c"`, `"eb"`.
- **String wrappers for TidalCycles**: Use `hContext` (not `HarmonicContext`), `gen`/`genWith` (not `generate`), `overtones`, `key`, `funds`, `wildcard` in the Tidal REPL.

### Typical workflow

1. `docker compose up -d neo4j`
2. `Rscript scripts/export_ycacl.R …` (re-run when the corpus changes)
3. `stack run` (creates/updates graph, logs per-composer progress)
4. Inspect Neo4j or export derived playlists/patterns
5. Fire up SuperCollider + Tidal to improvise with the stored cadences.

### Notes on database resets

Running `stack run` now wipes only the `Cadence`/`NEXT` subgraph via APOC's batched delete before repopulating it, so you get a deterministic rebuild without exhausting heap space. If you truly want to drop every Neo4j object (indexes, other custom nodes, etc.), stop the container and delete the mounted volume (`docker compose down -v neo4j`) as before.

### Database Schema

The Neo4j graph stores cadence transitions using the following structure:

#### Cadence Node Properties

| Property     | Type    | Description |
|--------------|---------|-------------|
| `show`       | String  | Unique key: `"( <movement> -> <functionality> )"` |
| `movement`   | String  | Root motion (e.g., `"desc 2"`, `"pedal"`) — pitch-agnostic |
| `chord`      | String  | Target pitch classes (e.g., `"[P 0,P 9,P 11]"`) |
| `dissonance` | Integer | Hindemith ranking hint |

A uniqueness constraint exists on `Cadence.show`.

#### NEXT Relationships

Cadence transitions are represented by `[:NEXT]` edges with a `weights` property containing per-composer counts as a JSON string.

```cypher
-- Inspect node structure
MATCH (c:Cadence) RETURN keys(c) AS props LIMIT 1;
-- => ["show", "movement", "dissonance", "chord"]

-- Sample cadences
MATCH (c:Cadence) RETURN c.show, c.movement, c.chord, c.dissonance LIMIT 5;

-- Sample transitions
MATCH (from:Cadence)-[r:NEXT]->(to:Cadence)
RETURN from.show, r.weights, to.show LIMIT 5;
```

#### Round-Tripping

Data is serialized via `deconstructCadence` and deserialized via `constructCadence` (see `MusicData.hs`). The `movement` and `chord` fields are stored as text to enable pitch-agnostic storage; transposition happens at runtime.

### Live Coding

For TidalCycles workflow instructions, see [USER_GUIDE.md](USER_GUIDE.md).
