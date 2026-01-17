# theHarmonicAlgorithm

**Generate harmonic progressions trained on the Yale Classical Archives Corpus**

A Haskell library that combines music theory, graph databases, and probabilistic selection to create musically coherent chord progressions for live coding with TidalCycles.

---

## What It Does

theHarmonicAlgorithm generates harmonic progressions by learning from Bach chorales in the Yale Classical Archives Corpus (YCACL). It stores cadence transitions in Neo4j, applies harmonic constraints, and selects chords probabilistically.

**Key Features:**
- **Training on classical repertoire**: Learn harmonic patterns from 60 Bach chorales
- **Flexible constraints**: Filter by overtones, key signatures, and root motion
- **Probabilistic selection**: Gamma-distribution sampling balances exploration and exploitation
- **TidalCycles integration**: Pattern-based lookup with infinite cycling for live coding
- **Voice leading optimization**: Dynamic programming for smooth chord progressions
- **Comprehensive test suite**: 379 examples validating music theory and generation

**Who It's For:**
- Live coders using TidalCycles and SuperCollider
- Composers exploring algorithmic harmony
- Music researchers interested in computational creativity

---

## Quick Start (5 Minutes)

### Prerequisites

- **Haskell Stack**: [Install Stack](https://docs.haskellstack.org/) with GHC 9.6.7
- **Docker**: For running Neo4j database
- **TidalCycles** (optional): For live coding integration

### Installation

```bash
# Step 1: Clone and build
git clone https://github.com/OscarSouth/theHarmonicAlgorithm
cd theHarmonicAlgorithm
stack build

# Step 2: Start Neo4j database
docker compose up -d neo4j

# Step 3: Populate database with Bach chorales
stack run
# Expected: "Cadence count: ~5000, Transition count: ~15000"

# Step 4: Run tests to verify everything works
stack test
# Expected: "379 examples, 0 failures"
```

### First Progression

```bash
stack ghci
```

```haskell
:set -XOverloadedStrings
import Harmonic.Lib

-- Create context and starting point
ctx <- harmonicContext "*" "*" "*"  -- No filtering (wildcard)
let start = initCadenceState 0 "C" [0,4,7] FlatSpelling  -- C major

-- Generate 4-chord progression
prog <- genSilent start 4 "*" 0.5 ctx
print prog
```

**Expected output**:
```
C maj → F maj → G maj → C maj
```

---

## Architecture Overview

theHarmonicAlgorithm implements the **Creative Systems Framework (R→E→T)**:

```
   RULES (R)              EVALUATION (E)          TRAVERSAL (T)
Define valid chords ───▶ Score chord quality ───▶ Select probabilistically
```

- **Rules (R)**: Defines valid harmonic possibilities (overtones, key signatures, root motion)
- **Evaluation (E)**: Scores quality using dissonance and voice leading costs
- **Traversal (T)**: Selects next chord using gamma-distribution sampling

**Four-Layer Architecture:**

```
Layer D: VOICE       ─ TidalCycles interface, pattern lookup
         │
Layer C: HANDS       ─ Evaluation (scoring) + Traversal (selection)
         │
Layer B: BRAIN       ─ Music theory types (Pitch, Harmony, Progression)
         │
Layer A: MEMORY      ─ Data ingestion (CSV parsing, Neo4j writes)
```

**For detailed architecture**, see [ARCHITECTURE.md](ARCHITECTURE.md).

---

## Usage Examples

### Generate Progressions

```haskell
import Harmonic.Lib

-- Create context and starting state
ctx <- harmonicContext "*" "*" "*"
let start = initCadenceState 0 "C" [0,4,7] FlatSpelling

-- Different verbosity levels
prog1 <- genSilent start 8 "*" 0.5 ctx    -- No diagnostics
prog2 <- genStandard start 8 "*" 0.5 ctx  -- Standard logging
prog3 <- genVerbose start 8 "*" 0.5 ctx   -- Full trace

-- Control entropy (exploration vs exploitation)
lowEntropy  <- genSilent start 8 "*" 0.2 ctx  -- Conservative (high-weight cadences)
highEntropy <- genSilent start 8 "*" 0.8 ctx  -- Exploratory (deeper sampling)
```

### Apply Constraints

```haskell
-- Key filtering (G major - 1 sharp)
let gMajorCtx = harmonicContext "*" "1#" "*"
prog <- genSilent start 8 "*" 0.5 gMajorCtx

-- Overtone filtering (bass guitar tuning E-A-D-G)
let bassCtx = harmonicContext "E A D G" "*" "*"
prog <- genSilent start 8 "*" 0.5 bassCtx

-- Combined: D major key and roots
let dMajorCtx = harmonicContext "*" "##" "##"
prog <- genSilent start 8 "*" 0.5 dMajorCtx

-- Prime notation (exact pitch-classes without overtones)
let bluesCtx = harmonicContext "G E' A' A#'" "*" "E G"
prog <- genSilent start 8 "*" 0.5 bluesCtx
```

**Filter Notation:**
- **Overtones**: `"E A D G"` (fundamentals), `"C"` (single overtone series), `"*"` (wildcard)
- **Prime**: `"C'"` (exact pitch-class C, no overtones), `"E'"` (exact E)
- **Key signatures**: `"1#"` (G major), `"2b"` (Bb major), `"##"` (D major)
- **Roots**: `"E F# G"` (specific roots), `"1#"` (G major scale roots)

### TidalCycles Integration

```haskell
-- Generate a progression
ctx <- harmonicContext "*" "*" "*"
let start = initCadenceState 0 "C" [0,4,7] FlatSpelling
prog <- genSilent start 16 "*" 0.5 ctx

-- Use in TidalCycles patterns
d1 $ note (harmony prog "<0 1 2 3>") # s "superpiano"

-- Pattern-based lookup with modulo wrap
-- Index 16 on a 16-chord progression wraps to 0 (infinite cycling)
d1 $ note (harmony prog (run 16)) # s "superpiano"

-- Extract different voices
d1 $ note (voiceBy Roots prog (run 8)) # s "bass"
d1 $ note (voiceBy Harmony prog "<0 2 4 6>") # s "superpiano"
```

**Full TidalCycles guide**: [live/USER_GUIDE.tidal](live/USER_GUIDE.tidal)

---

## Explicit Progression Construction

In addition to generating progressions from the database, you can explicitly construct progressions for composition and arrangement workflows:

```haskell
import Harmonic.Lib

-- Method 1: Direct pitch-class lists
myProg = fromChordsFlat [
    [0, 4, 7],    -- C major
    [5, 9, 0],    -- F major
    [7, 11, 2],   -- G major
    [0, 4, 7]     -- C major
  ]

-- Method 2: Note names for readability
myProg2 = fromChordsSharp [
    notesToPCs [C, E, G],     -- C major
    notesToPCs [D, F', A],    -- D major
    notesToPCs [G, B, D]      -- G major
  ]

-- Use in TidalCycles
d1 $ note (arrange flow myProg 0 (-9,9) "0 1 2 3") # s "superpiano"
```

### Switch Mechanism (Harmony as Scale Source)

The switch mechanism allows harmony to serve as the scale source for melody, enabling flexible melodic construction:

```haskell
-- Create harmony progression
harmonyProg = fromChordsFlat [[0,4,7], [5,9,0], [7,11,2]]

-- Option 1: Explicit scales (traditional)
scales = [[0,2,4,7,9], [0,2,4,5,9], [0,2,4,7,9]]
melodyState1 = fromChordsFlat scales

-- Option 2: Use harmony as scales (switch!)
melodyState2 = harmonyProg  -- Same progression, used for melody

-- Option 3: Use harmony with overlap (passing tones)
melodyState3 = progOverlapF 1 harmonyProg

-- Usage in patterns:
d1 $ note (arrange flow harmonyProg 0 (-9,9) "0 1 2") # s "superpiano"     -- Harmony
d2 $ note (arrange flow melodyState1 0 (-9,9) "[0 2 4]") # s "supersaw"    -- Melody (explicit scales)
d2 $ note (arrange flow melodyState2 0 (-9,9) "[0 1 2]") # s "supersaw"    -- Melody (harmony as scale)
d2 $ note (arrange flow melodyState3 0 (-9,9) "[0 1 2 3]") # s "supersaw"  -- Melody (with passing tones)
```

### Form Transformation

Define musical sections and transform them by changing the form structure:

```haskell
-- Define sections
a = [notesToPCs [C, E, G], notesToPCs [F, A, C]]
b = [notesToPCs [G, B, D], notesToPCs [D, F', A]]

-- Change form by changing the assembly
form1 = concat [a, a, b, a]  -- AABA (original)
form2 = concat [a, b, a, b]  -- ABAB (alternate)
form3 = concat [a, a, b, b]  -- AABB (variation)

state1 = fromChordsSharp form1
state2 = fromChordsSharp form2
state3 = fromChordsSharp form3

-- Same melody, different forms
melody = "[0 2 4 0 1 2 3 1]"
d1 $ note (arrange flow state1 0 (-9,9) melody) # s "superpiano"  -- AABA structure
d1 $ note (arrange flow state2 0 (-9,9) melody) # s "superpiano"  -- ABAB structure
```

**Examples:**
- **[live/examples/blue_in_green.tidal](live/examples/blue_in_green.tidal)** - Jazz progression with scale/melody separation and switch mechanism
- **[live/examples/rosslyn_castle.tidal](live/examples/rosslyn_castle.tidal)** - AABA form with transformation support and note name syntax

---

## Documentation

- **[ARCHITECTURE.md](ARCHITECTURE.md)** - Comprehensive technical reference (R→E→T framework, module structure, core concepts)
- **[live/USER_GUIDE.tidal](live/USER_GUIDE.tidal)** - Interactive TidalCycles tutorial with examples
- **[CLAUDE.md](CLAUDE.md)** - Development guidelines for contributors (vertical slices, testing, legacy comparison)
- **[IMPROVEMENTS.md](IMPROVEMENTS.md)** - Tracked opportunities for optimization and enhancement

---

## Key Concepts

### Zero-Form Invariant

All cadences are stored as **relative intervals** starting at pitch-class 0, making the graph transposition-invariant:

```
C major → F major  =  Up 5 semitones + [0,4,7]
G major → C major  =  Up 5 semitones + [0,4,7]
                      ↑ Same relative movement!
```

**Why?**
- Dataset not biased toward common keys
- Smaller transition matrix (12× fewer states)
- Emphasizes cadence movement not chord identity

### Layer Boundaries

Architecture enforces clean dependency flow:
- Layer B (Types) **cannot** import from C or D
- Layer C (Evaluation/Traversal) may import from B but **not** D
- Layer D (Interface) may import from B and C

**Prevents** circular dependencies and ensures unidirectional data flow.

### Composer Specification

Currently **not implemented** - use `"*"` for all operations to aggregate all composer intent from the deterministic graph.

Planned for future release: per-composer filtering (e.g., `"bach"`, `"debussy"`) and weighted blending (e.g., `"bach:70 debussy:30"`).

---

## Development

### Build Commands

```bash
stack build      # Compile library and executable
stack test       # Run test suite (379 examples)
stack ghci       # Interactive REPL for testing
stack run        # Populate Neo4j with YCACL corpus
stack haddock    # Generate API documentation
```

### Project Structure

```
src/Harmonic/
├── Framework/      - Builder and orchestration (R→E→T pipeline)
├── Rules/          - Types and constraints (validity)
├── Evaluation/     - Scoring (dissonance, voice leading, database)
├── Traversal/      - Selection (probabilistic sampling)
└── Interface/      - TidalCycles integration

test/               - HSpec + QuickCheck test suite
live/               - TidalCycles boot scripts and examples
musicdata/          - Yale Classical Archives Corpus (Bach chorales)
scripts/            - R scripts for corpus preprocessing
```

### Contributing

See [CLAUDE.md](CLAUDE.md) for detailed development guidelines:
- Vertical slice methodology (minimum deliverable units)
- Mandatory verification (tests + REPL)
- Legacy comparison workflow
- Layer boundary enforcement

**Before contributing**:
1. Ensure Neo4j is running (`docker compose up -d neo4j`)
2. Run full test suite (`stack test`)
3. Verify behavior matches legacy implementation where applicable

---

## Data Preparation (Optional)

The repository includes preprocessed YCACL data. To regenerate from source:

```bash
# Requires R with tidyverse
Rscript scripts/export_ycacl.R \
  musicdata/YCACL \
  musicdata/YCAC-metadata.csv \
  data/ycacl_sequences.csv
```

The exporter:
- Filters to triads/quartal voicings (3-7 voices)
- Normalizes composer names
- Extracts fundamentals (lowest pitch-class)
- Writes tall CSV format (`composer,piece,order,pitches,fundamental`)

---

## License

MIT License - see [LICENSE](LICENSE) for details.

---

## Acknowledgments

- **Geraint A. Wiggins** - Creative Systems Framework (R→E→T)
- **Alex McLean** - TidalCycles live coding environment
- **UCI Machine Learning Repository** - Yale Classical Archives Corpus (Bach chorales dataset)
- **Paul Hindemith** - Interval dissonance theory (The Craft of Musical Composition, 1937)

---

## Troubleshooting

**Neo4j connection fails:**
```bash
# Check Neo4j is running
curl -s http://localhost:7474
# Expected: HTML page

# Verify credentials in src/Harmonic/Config.hs
-- default: bolt://localhost:7687 with neo4j/password
```

**Build errors:**
```bash
# Clean and rebuild
stack clean
stack build

# Update package index
stack update
```

**Tests fail:**
```bash
# Ensure Neo4j is running (some tests require database)
docker compose up -d neo4j

# Run specific test suite
stack test --test-arguments="--match Pitch"
```

**Generation returns empty progressions:**
```bash
# Verify database is populated
docker exec -it theHarmonicAlgorithm-neo4j \
  cypher-shell -u neo4j -p password \
  "MATCH (c:Cadence) RETURN count(c)"

# Expected: ~5000 cadences
# If 0, run: stack run
```

---

**Version**: 3.0.0
**Last Updated**: 2026-01-14
**Repository**: https://github.com/OscarSouth/theHarmonicAlgorithm
