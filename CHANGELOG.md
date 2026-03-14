# Changelog

## V3 (2026-03-13)

### Architecture
- **R->E->T Framework**: Rules->Evaluation->Traversal pipeline with four layers:
  - Layer A (Memory): CSV parsing, Neo4j writes, constraints
  - Layer B (Brain): Pitch-class algebra, harmony naming, voice leading
  - Layer C (Hands): Database queries, probabilistic selection, Markov analysis
  - Layer D (Voice): TidalCycles bridge, arrangement, groove interface
- **Builder module split**: Monolithic Builder.hs (~1900 lines) split into Types, Core, Diagnostics, Portmanteau sub-modules with facade re-export

### Generation Engine
- **Unified generation interface**: `genSilent`, `genStandard`, `genVerbose` with identical type signatures differing only in diagnostic output
- **Composer blend specification**: `"bach:30 debussy:70"` weighted blend, single composer, or `"*"` wildcard aggregation over YCACL corpus (600+ composers)
- **HarmonicContext filtering**: Three-part R-constraint system (overtones, key, roots) applied before database evaluation
- **Multiplicative fallback scoring**: `badness = chordDiss * motionDiss * (gammaDraw + 1)` with gamma perturbation for organic randomness
- **Consonance fallback**: Constructive generation from overtone palette when graph candidates are insufficient
- **Diagnostic trace system**: TransformTrace and AdvanceTrace for step-by-step debugging of chord rendering and root motion

### Voice Leading
- **Cyclic DP optimization**: `solveRoot` (root position) and `solveFlow` (free inversion) paradigms with wrap-around cost
- **Parallel motion penalties**: Parallel fifths/octaves detection with configurable penalty
- **Large leap penalties**: Movement > 4 semitones penalized proportionally

### TidalCycles Interface
- **Bridge**: Pattern lookup with modulo wrap (`lookupChord`), voice extraction (`rootNotes`, `bassNotes`, `flow`)
- **Arranger**: Progression combinators (`rotate`, `excerpt`, `fuse`, `transpose`, `expand`, `insert`)
- **Groove**: Performance interface with `fund` voice strategy for harmonic root extraction
- **Instruments**: Instrument definitions and utilities
- **Voicing strategies**: `root`, `flow`, `lite`, `literal` via `voiceBy`

### Performance
- **Shared RNG**: Single `GenIO` threaded through generation run (eliminated ~4600 `/dev/urandom` reads per 8-chord progression)
- **O(1) chain building**: Reverse-cons accumulator with explicit current state (was O(n^2) list append)
- **Pre-parsed context**: `ParsedContext` with `IntSet` for O(1) membership tests (was reparsing text ~690 times/step)
- **Vector-ified DP solver**: `Data.Vector` for O(1) indexing in voice leading solver (was O(n) list indexing)

### Correctness
- **`mostConsonant` fix**: Inline implementation now matches canonical Dissonance module (added perfect fifth bonus and degenerate set penalty)
- **`readNoteName` fix**: Unknown note names now throw error instead of silently defaulting to C
- **Candidate pool alignment**: `genSilent` and `genStandard`/`genVerbose` now use identical candidate pools
- **CSV parse error handling**: Parse failures now throw instead of silently returning empty data

### Cleanup
- Removed dead homing mechanism scaffold (`gcHomingThreshold`, `gcHomingStrength`, `fetchHomingCandidates`, `applyHomingBias`)
- `GeneratorConfig` reduced to single field: `gcPoolSize`
- Documented Markov.hs as ingestion-only (not used in runtime generation path)
