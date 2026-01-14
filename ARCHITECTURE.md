# theHarmonicAlgorithm Architecture Guide

## 1. Introduction

### What is theHarmonicAlgorithm

theHarmonicAlgorithm is a Haskell library for generating harmonic progressions trained on the Yale Classical Archives Corpus (YCACL). It combines music theory, graph databases, and probabilistic selection to create musically coherent chord progressions.

The system generates progressions by:
1. Storing cadence transitions from the Bach chorales corpus in Neo4j
2. Applying harmonic constraints (overtones, key signatures, root motion)
3. Evaluating candidate cadences using dissonance and voice leading costs
4. Selecting next chords probabilistically using gamma-distribution sampling
5. Interfacing with TidalCycles for live music coding

### High-Level System Overview

```
                 R→E→T GENERATION PIPELINE

  ┌─────────────────────────────────────────────────────┐
  │                    Input State                       │
  │              (root, intervals, spelling)             │
  └─────────────────────┬───────────────────────────────┘
                        │
                        ▼
  ┌─────────────────────────────────────────────────────┐
  │                R: RULES LAYER                        │
  │  ┌────────────┬──────────────┬─────────────────┐   │
  │  │ Overtone   │ Key Filter   │  Root Filter    │   │
  │  │ Constraints│              │                 │   │
  │  └────────────┴──────────────┴─────────────────┘   │
  │         Define valid conceptual space                │
  └─────────────────────┬───────────────────────────────┘
                        │
                        ▼
  ┌─────────────────────────────────────────────────────┐
  │              E: EVALUATION LAYER                     │
  │  ┌────────────┬──────────────┬─────────────────┐   │
  │  │ Neo4j      │ Dissonance   │ Voice Leading  │   │
  │  │ Query      │ Scoring      │ Cost           │   │
  │  └────────────┴──────────────┴─────────────────┘   │
  │    Score quality of valid cadences                   │
  └─────────────────────┬───────────────────────────────┘
                        │
                        ▼
  ┌─────────────────────────────────────────────────────┐
  │              T: TRAVERSAL LAYER                      │
  │  ┌────────────┬──────────────┬─────────────────┐   │
  │  │Probabilistic│ Selection   │ Voice Leading  │   │
  │  │Sampling    │ Strategy    │ Optimization   │   │
  │  └────────────┴──────────────┴─────────────────┘   │
  │    Select next chord from evaluated candidates       │
  └─────────────────────┬───────────────────────────────┘
                        │
                        ▼
  ┌─────────────────────────────────────────────────────┐
  │                   Output State                       │
  │              (next chord in progression)             │
  └─────────────────────────────────────────────────────┘
```

### Target Audience

This document is for **developers** who want to:
- Understand the system architecture
- Extend functionality
- Fix bugs or optimize performance
- Contribute to the codebase

For usage instructions, see [README.md](README.md) and [live/USER_GUIDE.tidal](live/USER_GUIDE.tidal).

---

## 2. Creative Systems Framework (R→E→T)

### 2.1 Theoretical Foundation

The architecture implements Wiggins' **Creative Systems Framework** (Wiggins, G.A., 2006), which models creative generation as three components:

**Rules (R)**: Defines the universe of valid concepts
- Input: None (or constraint specifications)
- Output: Set of all valid possibilities
- Role: Constraint satisfaction, filtering
- Example: "All triads constructible from E-A-D-G overtones"

**Evaluation (E)**: Scores quality of valid concepts
- Input: Valid concept from R
- Output: Quality score (dissonance, voice leading cost, composer weight)
- Role: Preference ordering, ranking
- Example: "C major → F major scores 3.2 (dissonance) + 1.5 (voice leading)"

**Traversal (T)**: Navigates conceptual space
- Input: Evaluated candidates from E
- Output: Selected concept
- Role: Search strategy, selection
- Example: "Select next cadence using gamma sampling with entropy 0.5"

### Why R→E→T?

Traditional approaches mix constraints and preferences, making them brittle. The R→E→T separation allows:
- **Modularity**: Replace one component without affecting others
- **Clarity**: Explicit distinction between "valid" and "good"
- **Flexibility**: Same R with different T explores space differently

### 2.2 Framework Mapping to Modules

| Layer | Component | Modules | Responsibility |
|-------|-----------|---------|----------------|
| **Rules (R)** | Constraints | `Rules/Types/`, `Rules/Constraints/`, `Rules/Import/` | Define valid pitch-class sets, chord structures, filter specifications |
| **Evaluation (E)** | Quality Scoring | `Evaluation/Scoring/`, `Evaluation/Database/`, `Evaluation/Analysis/` | Score dissonance, voice leading costs, query composer weights |
| **Traversal (T)** | Selection | `Traversal/Probabilistic`, `Traversal/VoiceLeading` | Gamma sampling, voice leading optimization |
| **Framework** | Orchestration | `Framework/Builder` | Coordinate R→E→T pipeline |
| **Interface** | External Systems | `Interface/Tidal/` | TidalCycles integration |

### Dependency Flow

```
Framework.Builder
    │
    ├──▶ Rules.Types.Pitch         (R: foundational algebra)
    │      │
    ├──▶ Rules.Types.Harmony       (R: chord/cadence types)
    │      │
    ├──▶ Rules.Constraints.Filter  (R: harmonic constraints)
    │      │
    ├──▶ Rules.Constraints.Overtone(R: triad generation)
    │
    ├──▶ Evaluation.Scoring.Dissonance      (E: chord quality)
    │
    ├──▶ Evaluation.Scoring.VoiceLeading    (E: cost functions)
    │
    ├──▶ Evaluation.Database.Query          (E: graph queries)
    │
    └──▶ Traversal.Probabilistic            (T: selection)
```

---

## 3. Four-Layer Architecture

The codebase is organized into four vertical layers, inspired by the **Layer A (Memory) → Layer B (Brain) → Layer C (Hands) → Layer D (Voice)** paradigm:

```
    FOUR-LAYER VERTICAL ARCHITECTURE

┌────────────────────────────────────────────┐
│  Layer D: VOICE (Interface)                │
│                                            │
│  ┌──────────────────────────────────────┐ │
│  │  TidalCycles Bridge                  │ │
│  │  Pattern Lookup                      │ │
│  │  Voicing Strategies                  │ │
│  │  Arrangement Combinators             │ │
│  └──────────────────────────────────────┘ │
└────────────────┬───────────────────────────┘
                 │ depends on
                 ▼
┌────────────────────────────────────────────┐
│  Layer C: HANDS (Evaluation & Traversal)   │
│                                            │
│  ┌──────────────┐  ┌───────────────────┐  │
│  │  EVALUATION  │  │    TRAVERSAL      │  │
│  │  ────────────│  │   ─────────────── │  │
│  │  - Database  │  │  - Probabilistic  │  │
│  │    Queries   │  │    Sampling       │  │
│  │  - Dissonance│  │  - Selection      │  │
│  │    Scoring   │  │    Strategy       │  │
│  │  - Markov    │  │  - Voice Leading  │  │
│  │    Analysis  │  │    Optimization   │  │
│  └──────────────┘  └───────────────────┘  │
└────────────────┬───────────────────────────┘
                 │ depends on
                 ▼
┌────────────────────────────────────────────┐
│  Layer B: BRAIN (Types)                    │
│                                            │
│  ┌──────────────────────────────────────┐ │
│  │  Music Theory Primitives             │ │
│  │  ─────────────────────────           │ │
│  │  - Pitch-class Algebra (ℤ₁₂)        │ │
│  │  - Harmony Naming                    │ │
│  │  - Progression Structure             │ │
│  │  - Voice Leading Costs               │ │
│  └──────────────────────────────────────┘ │
└────────────────┬───────────────────────────┘
                 │ depends on
                 ▼
┌────────────────────────────────────────────┐
│  Layer A: MEMORY (Rules & Data)            │
│                                            │
│  ┌──────────────────────────────────────┐ │
│  │  Data Import Pipeline                │ │
│  │  ─────────────────────               │ │
│  │  - CSV Parsing (YCACL)               │ │
│  │  - Neo4j Graph Writes                │ │
│  │  - Constraint Specification          │ │
│  │  - Overtone Generation               │ │
│  └──────────────────────────────────────┘ │
└────────────────────────────────────────────┘

  ✓ Clean dependency flow (top → bottom)
  ✓ No circular dependencies
  ✓ Layer boundaries enforced by imports
```

### Layer A: Memory (Rules Component)

**Location**: `src/Harmonic/Rules/Import/`

**Purpose**: Ingests the Yale Classical Archives Corpus and populates Neo4j graph database

**Modules**:
- `Rules/Import/CSV.hs` - Parse YCACL CSV files
- `Rules/Import/Transform.hs` - Convert chords to cadences
- `Rules/Import/Types.hs` - ChordSlice and import types
- `Rules/Import/Graph.hs` - Neo4j schema writing

**Data Flow**:
```
Bach Chorales (CSV)
    → parseCSV
    → [ChordSlice]
    → buildCadences
    → [Cadence]
    → writeGraph (Neo4j)
    → NEXT relationships with weights
```

**Key Insight**: Cadences are stored in **zero-form** (relative intervals starting at 0), making the graph transposition-invariant. See Section 5.1 for details.

### Layer B: Brain (Music Theory Types)

**Location**: `src/Harmonic/Rules/Types/`

**Purpose**: Foundational music theory algebra and data structures

**Modules**:
- `Rules/Types/Pitch.hs` - ℤ₁₂ pitch-class algebra (src/Harmonic/Rules/Types/Pitch.hs:1)
- `Rules/Types/Harmony.hs` - Chord/cadence types and naming (src/Harmonic/Rules/Types/Harmony.hs:1)
- `Rules/Types/Progression.hs` - Progression monoid (src/Harmonic/Rules/Types/Progression.hs:1)

**Key Abstractions**:
- **PitchClass**: Newtype wrapping ℤ₁₂ with modular arithmetic
- **Chord**: Root + function name + intervals
- **Cadence**: Function name + movement + zero-form intervals
- **Progression**: Sequence of CadenceState wrapped in Monoid

**Design Principle**: Types enforce invariants. `PitchClass` cannot hold invalid values (≥12), enforced by smart constructors.

### Layer C: Hands (Evaluation & Traversal)

#### Evaluation (E Component)

**Location**: `src/Harmonic/Evaluation/`

**Modules**:
- `Evaluation/Scoring/Dissonance.hs` - Hindemith interval vectors (src/Harmonic/Evaluation/Scoring/Dissonance.hs:1)
- `Evaluation/Scoring/VoiceLeading.hs` - Voice leading cost functions (src/Harmonic/Evaluation/Scoring/VoiceLeading.hs:1)
- `Evaluation/Database/Query.hs` - Neo4j queries with composer weights (src/Harmonic/Evaluation/Database/Query.hs:1)
- `Evaluation/Analysis/Markov.hs` - Transition probability calculation (src/Harmonic/Evaluation/Analysis/Markov.hs:1)

**Scoring Metrics**:
- **Dissonance**: Hindemith intervalVector → sum of products with hindemithVector
- **Voice Leading**: Sum of voice movements + penalties (parallel fifths, large leaps)
- **Composer Weight**: Neo4j edge weight × composer blend factor

#### Traversal (T Component)

**Location**: `src/Harmonic/Traversal/`

**Modules**:
- `Traversal/Probabilistic.hs` - Gamma distribution sampling (src/Harmonic/Traversal/Probabilistic.hs:1)
- `Traversal/VoiceLeading.hs` - Dynamic programming optimization (src/Harmonic/Evaluation/Scoring/VoiceLeading.hs:155-200)

**Selection Strategy**:
1. Query Neo4j for candidates matching current cadence
2. Filter by harmonic context (R constraints)
3. Score by dissonance + voice leading (E metrics)
4. Sample using gamma distribution (T strategy)

**Entropy Parameter**: Controls exploration vs exploitation
- Low entropy (0.2): Exploit - select high-weight cadences
- High entropy (0.8): Explore - select from deeper in the list

### Layer D: Voice (TidalCycles Interface)

**Location**: `src/Harmonic/Interface/Tidal/`

**Purpose**: Bridge between harmonic engine and TidalCycles live coding

**Modules**:
- `Interface/Tidal/Bridge.hs` - Pattern-based lookup with modulo wrap (src/Harmonic/Interface/Tidal/Bridge.hs:1)
- `Interface/Tidal/Arranger.hs` - Voicing strategies (flow, root, lite, etc.)
- `Interface/Tidal/Instruments.hs` - Launcher definitions (juno, moog, etc.)
- `Interface/Tidal/Utils.hs` - Utility functions

**Key Concept**: **Modulo Wrap**
- Progressions are finite (e.g., 16 chords)
- TidalCycles patterns are infinite
- `lookupChord prog idx` wraps: `chords !! (idx mod length)`
- Enables `run 4` on 16-chord progression → loops first 4

---

## 4. Module Structure

### Directory Organization

```
src/Harmonic/
│
├── Lib.hs                    [Main re-export, public API]
├── Config.hs                 [Neo4j configuration]
│
├── Framework/                [R→E→T Orchestration]
│   └── Builder.hs            [Generation engine, coordinates R→E→T]
│
├── Rules/                    [R Component + Layer A (Memory)]
│   ├── Types/                [Foundational music theory types]
│   │   ├── Pitch.hs          [ℤ₁₂ pitch-class algebra]
│   │   ├── Harmony.hs        [Chord/cadence types and naming]
│   │   └── Progression.hs    [Progression monoid]
│   ├── Constraints/          [Filtering and validity rules]
│   │   ├── Filter.hs         [Harmonic context filtering]
│   │   └── Overtone.hs       [Triad generation from overtones]
│   └── Import/               [Data ingestion pipeline]
│       ├── CSV.hs            [YCACL CSV parsing]
│       ├── Transform.hs      [ChordSlice → Cadence]
│       ├── Types.hs          [Import data types]
│       └── Graph.hs          [Neo4j schema writes]
│
├── Evaluation/               [E Component + Layer C (Hands)]
│   ├── Scoring/              [Quality assessment]
│   │   ├── Dissonance.hs     [Hindemith interval vectors]
│   │   └── VoiceLeading.hs   [Voice leading cost functions]
│   ├── Database/             [Graph queries and ranking]
│   │   └── Query.hs          [Neo4j queries with composer weights]
│   └── Analysis/             [Statistical evaluation]
│       └── Markov.hs         [Transition probability calculation]
│
├── Traversal/                [T Component + Layer C (Hands)]
│   └── Probabilistic.hs      [Gamma distribution sampling]
│
└── Interface/                [Layer D (Voice) - TidalCycles]
    └── Tidal/                [TidalCycles-specific bridge]
        ├── Bridge.hs         [Pattern lookup and modulo wrap]
        ├── Arranger.hs       [Voicing strategies (flow, root, lite)]
        ├── Instruments.hs    [Launcher definitions]
        └── Utils.hs          [Utility functions]
```

### Module Dependency Graph

```
MODULE DEPENDENCY FLOW (simplified)

Harmonic.Lib (re-exports)
    │
    └──▶ Framework.Builder ◀──────────────┐
            │                              │
            ├──▶ Rules.Types.Pitch         │
            │      │                       │
            ├──▶ Rules.Types.Harmony       │
            │      │                       │
            ├──▶ Rules.Constraints.Filter  │
            │      │                       │
            ├──▶ Rules.Constraints.Overtone│
            │                              │
            ├──▶ Evaluation.Scoring.Dissonance
            │                              │
            ├──▶ Evaluation.Database.Query │
            │                              │
            ├──▶ Traversal.Probabilistic   │
            │                              │
            └──▶ Evaluation.Scoring.VoiceLeading
                    │
                    └──▶ Traversal optimization

Interface.Tidal.Bridge
    │
    ├──▶ Rules.Types.Progression
    │
    └──▶ Interface.Tidal.Arranger
            │
            └──▶ Evaluation.Scoring.VoiceLeading
```

### Layer Boundary Enforcement

Enforced through Haskell imports:
- Layer B (Types) modules **cannot** import from C or D
- Layer C (Evaluation/Traversal) may import from B but **not** D
- Layer D (Interface) may import from B and C

**Rationale**: Prevents circular dependencies, enforces unidirectional data flow.

---

## 5. Core Concepts

### 5.1 Zero-Form Invariant

**Definition**: All cadences are stored as **relative intervals** starting at pitch-class 0.

```
ZERO-FORM INVARIANT: Relative vs. Absolute

ABSOLUTE (Chord):                  RELATIVE (Cadence):
Root + Intervals                   Movement + Intervals (zero-form)

C major:  C + [0,4,7]              Up 0 semitones + [P 0, P 4, P 7]
F major:  F + [0,4,7]              Up 5 semitones + [P 0, P 4, P 7]
G major:  G + [0,4,7]              Up 2 semitones + [P 0, P 4, P 7]
                                           ↑
                                   Same relative structure!

WHY?
  1. Transposition-invariant analysis
  2. Dataset not biased toward common keys
  3. Smaller transition matrix (12× fewer states)
  4. Emphasizes cadence MOVEMENT not chord identity

IMPLEMENTATION:
  - Neo4j stores: Cadence nodes with [P 0, ...] intervals
  - Runtime: Convert to absolute ChordState for playback
  - Builder: Maintains absolute root, queries relative cadences
```

**Example Code** (src/Harmonic/Rules/Types/Harmony.hs:154-167):
```haskell
-- Convert absolute Chord pair to relative Cadence
toCadence :: (Chord, Chord) -> Cadence
toCadence (from, to) =
  let fromRoot = pitchClass (chordRoot from)
      toRoot = pitchClass (chordRoot to)
      mvmt = toMovement fromRoot toRoot
      -- Zero-form: shift intervals so first = P 0
      zeroPcs = case toIntervals of
        [] -> []
        (p:ps) -> P 0 : map (subtract p) ps
   in Cadence (chordFunc to) mvmt zeroPcs
```

### 5.2 Builder Pipeline

The `Framework.Builder` module orchestrates the R→E→T pipeline (src/Harmonic/Framework/Builder.hs:1):

```
GENERATION FLOW (R→E→T)

Input: CadenceState, length, composer, entropy, HarmonicContext
    │
    ▼
┌─────────────────────────────────────┐
│ FOR each step in progression:      │
│                                     │
│  1. RULES (R): Filter candidates   │
│     - Query Neo4j for NEXT edges   │
│     - Apply overtone filter        │
│     - Apply key filter             │
│     - Apply root filter            │
│                                     │
│  2. EVALUATION (E): Score quality  │
│     - Dissonance score             │
│     - Voice leading cost           │
│     - Composer weight blend        │
│                                     │
│  3. TRAVERSAL (T): Select next     │
│     - Gamma sampling with entropy  │
│     - Update CadenceState          │
│     - Append to progression        │
│                                     │
└─────────────────────────────────────┘
    │
    ▼
Output: Progression (Seq CadenceState)
```

**Verbosity Levels**:
- **Silent** (`genSilent`): No diagnostics
- **Standard** (`genStandard`): Per-step summaries
- **Verbose** (`genVerbose`): Full traces (root motion, functionality, voice leading)

**State Threading**: Builder maintains `CadenceState` through generation:
```haskell
data CadenceState = CadenceState
  { csRoot      :: NoteName      -- Absolute root (e.g., C, F#, Bb)
  , csFunc      :: String        -- Function name ("maj", "min7", etc.)
  , csIntervals :: [PitchClass]  -- Absolute intervals [0,4,7]
  , csSpelling  :: EnharmonicSpelling  -- FlatSpelling or SharpSpelling
  }
```

### 5.3 HarmonicContext

The `HarmonicContext` type encodes the three-filter system (src/Harmonic/Framework/Builder.hs:68-72):

```haskell
data HarmonicContext = HarmonicContext
  { overtoneFilter :: String  -- Pitch-set constraint ("E A D G", "C", "*")
  , keyFilter      :: String  -- Key signature ("1#", "2b", "*")
  , rootFilter     :: String  -- Root motion constraint ("1#", "E G", "*")
  }
```

**Filter Interaction**:
1. **Overtone Filter**: Restricts chord pitch-classes to overtone series
   - `"E A D G"` → Only pitches in {E,A,D,G,B,F#,C#,G#,D#,A#} (combined overtones)
   - `"C"` → Only pitches in {C,E,G,Bb} (C overtone series)
   - Prime notation `"C'"` → Exact pitch-class (no overtones)

2. **Key Filter**: Restricts to diatonic collection
   - `"1#"` → G major scale {G,A,B,C,D,E,F#}
   - `"2b"` → Bb major scale {Bb,C,D,Eb,F,G,A}
   - Named keys `"D"` → D major scale

3. **Root Filter**: Restricts root motion
   - `"E G"` → Only cadences with E or G as root
   - `"1#"` → Only roots in G major scale

**Wildcard**: `"*"` matches all (no filtering)

**Example Usage** (from live/USER_GUIDE.tidal):
```haskell
-- G major tonality with G major roots
ctx1 <- harmonicContext "*" "1#" "1#"

-- D major roots only, any pitch content
ctx2 <- harmonicContext "*" "*" "##"

-- Specific overtones, any key/roots
ctx3 <- harmonicContext "D A D F A Ab" "*" "*"
```

---

## 6. Database Schema

### 6.1 Neo4j Structure

**Connection**: `bolt://localhost:7687` (credentials: neo4j/password)

**Node Type**:
```cypher
(:Cadence {
  func: String,        -- Function name ("maj", "min7", etc.)
  movement: String,    -- Movement type ("Asc P 5", "Desc P 4", "Unison")
  intervals: [Int]     -- Zero-form intervals [0, 4, 7]
})
```

**Relationship Types**:
```cypher
(:Cadence)-[:NEXT {weight: Float, composer: String}]->(:Cadence)
(:Cadence)-[:COMPOSER {name: String}]->(:Cadence)
```

**Weight Calculation** (src/Harmonic/Evaluation/Analysis/Markov.hs:27-42):
```haskell
-- Transition probability P(to | from) = count(from → to) / count(from → *)
transitionProbabilities :: [Cadence] -> Map Edge Double
transitionProbabilities cadences =
  let counts = transitionCounts cadences  -- Map (from, to) → count
      totals = buildTotals counts         -- Map from → total_count
   in Map.mapWithKey (normalise totals) counts
```

### 6.2 YCACL Corpus

**Dataset**: Yale Classical Archives Corpus of Bach chorales
- **Size**: 60 chorales, ~5000 chords
- **Format**: CSV with columns `[pitch1, pitch2, pitch3, pitch4, ...]`
- **Preprocessing** (src/Harmonic/Rules/Import/Transform.hs:21-63):
  1. Extract fundamental (lowest pitch-class)
  2. Generate triad interpretations (top 3 by consonance)
  3. Duplicate high-scoring triads (3×, 2×, 1×) for weight
  4. Build cadence transitions (cross-multiply adjacent slices)

**Statistical Properties**:
- **Most common cadence**: `V → I` (dominant to tonic)
- **Most common root motion**: Descending fifth (P 5)
- **Average dissonance**: 6.8 (major/minor triads score 6.0)

---

## 7. TidalCycles Integration

### 7.1 Pattern Bridge

The `Interface.Tidal.Bridge` module provides pattern-based lookup (src/Harmonic/Interface/Tidal/Bridge.hs:1):

**Key Function**:
```haskell
lookupChord :: Progression -> Int -> Chord
lookupChord prog idx =
  let len = progLength prog
      chords = progChords prog
      wrappedIdx = idx `mod` len
   in chords !! wrappedIdx
```

**Modulo Wrap Behavior**:
- `lookupChord prog 0` → first chord
- `lookupChord prog 16` → chord at `(16 mod len)`
- Enables infinite cycling: `run 4` on 16-bar progression loops first 4

**Pattern Syntax** (TidalCycles):
```haskell
-- Simple index pattern
d1 $ note (harmony prog "0 1 2 3") # s "superpiano"

-- Euclidean rhythm
d1 $ note (harmony prog (run 8)) # s "superpiano"

-- Nested patterns
d1 $ note (harmony prog "<0 [1 2] 3>") # s "superpiano"
```

### 7.2 Voicing Strategies

The `Interface.Tidal.Arranger` module provides voicing functions:

**Flow Voicing** (voice-led, cyclic DP smoothing):
```haskell
flow :: Progression -> [[Int]]
flow prog =
  let states = toList (unProgression prog)
      chords = map (toTriad . fromCadenceState) states
      voicings = solveFlow chords  -- Cyclic DP optimization
   in map chordPitches voicings
```

**Root Voicing** (root position):
```haskell
root :: Progression -> [[Int]]
root prog = map rootPosition (progChords prog)
```

**Lite Voicing** (literal intervals, no voice leading):
```haskell
lite :: Progression -> [[Int]]
lite prog = literalVoicing prog
```

**Bass Extraction** (single pitch):
```haskell
bass :: Progression -> [[Int]]
bass prog = map (\chord -> [head (chordIntervals chord)]) (progChords prog)
```

**Launcher Paradigm** (from legacy theHarmonicAlgorithm):
```haskell
-- Example: Juno synthesizer with flow voicing
juno f s r d = p "juno" $ f $ arrange flow s r (-9,9) ["pattern"]

-- Usage in TidalCycles
d1 $ juno flow prog 1 (-9,9) [run 4]
```

---

## 8. Design Principles

### 8.1 Minimize Code and Complexity

**Principle**: Simplest solution wins. Before adding code, ask:
- Is this strictly necessary?
- Can this be achieved more simply?
- Does the legacy implementation have a simpler approach?

**Example**: Voice leading optimization uses cyclic DP (45 lines) instead of exhaustive search (exponential).

### 8.2 Legacy as Source of Truth

**Principle**: The legacy implementation (`theHarmonicAlgorithmLegacy/`) functioned correctly and was verified. When behavior is unclear, compare against legacy.

**Process**:
1. Read legacy code (e.g., `theHarmonicAlgorithmLegacy/src/MusicData.hs`)
2. Compare function signatures and outputs
3. If modernized behavior differs, investigate why before assuming modernized is correct

### 8.3 Suspect All Existing Code

**Principle**: Previous AI agents may have introduced unnecessary logic, stubbed-out functionality, or incorrect modernizations. Nothing is concrete until final completion.

**Checklist**:
- Does this code serve the current functionality?
- Is this logic hallucinated or speculative?
- Does the legacy implementation do this differently?

### 8.4 Tests Are Not Infallible

**Principle**: Test suites should be interrogated and changed to accommodate actual requirements. Tests may test incorrect behavior or be missing critical cases.

**When tests fail**:
1. Check if test expectation is correct
2. Compare against legacy behavior
3. Trust legacy behavior over existing tests if they conflict

### 8.5 Vertical Slice Methodology

**Principle**: All changes delivered in vertical slices - the minimum deliverable working and verifiable unit.

**Workflow**:
1. Identify smallest atomic change
2. Implement only that slice
3. Verify using tests + REPL
4. Commit (if requested)
5. Repeat for next slice

**Not a slice**: Multiple unrelated changes spanning multiple modules without clear dependency.

---

## 9. Extension Points

### 9.1 Adding New Voicing Strategies

To add a new voicing strategy (e.g., "wide" for large pitch ranges):

1. **Add voicing function** to `Interface.Tidal.Arranger`:
```haskell
wide :: Progression -> [[Int]]
wide prog =
  let states = toList (unProgression prog)
      chords = map (toTriad . fromCadenceState) states
   in map wideVoicing chords  -- Your voicing logic
```

2. **Update exports** in `Lib.hs`:
```haskell
module Harmonic.Lib (
  -- ... existing exports
  , wide  -- Add to export list
  ) where

import Harmonic.Interface.Tidal.Arranger (wide)
```

3. **Add tests** in `test/Harmonic/Interface/Tidal/BridgeSpec.hs`:
```haskell
describe "wide voicing" $ do
  it "produces wide intervals" $ do
    let voicings = wide testProgression
    -- Test your voicing constraints
```

4. **Document** in `live/USER_GUIDE.tidal`:
```haskell
-- Wide voicing (large pitch ranges)
d1 $ note (harmony prog (run 4)) # s "superpiano"
   # sound (voiceBy wide prog (run 4))
```

### 9.2 Custom Evaluation Functions

To add a new evaluation metric (e.g., "melodic smoothness"):

1. **Create module** `Evaluation/Scoring/Melodic.hs`:
```haskell
module Harmonic.Evaluation.Scoring.Melodic
  ( melodicSmoothness
  ) where

melodicSmoothness :: Chord -> Chord -> Double
melodicSmoothness from to =
  -- Your metric logic
```

2. **Integrate in Builder** (src/Harmonic/Framework/Builder.hs):
```haskell
import qualified Harmonic.Evaluation.Scoring.Melodic as M

-- In generation loop
let melodicScore = M.melodicSmoothness currentChord candidateChord
    totalScore = dissonanceScore + voiceLeadingCost + melodicScore
```

3. **Add tests** in `test/Harmonic/Evaluation/Scoring/MelodicSpec.hs`

4. **Expose in Lib.hs** if public API

### 9.3 Alternative Traversal Algorithms

To replace gamma sampling with a different selection strategy:

1. **Create module** `Traversal/Greedy.hs`:
```haskell
module Harmonic.Traversal.Greedy
  ( greedySelect
  ) where

greedySelect :: [(Cadence, Double)] -> IO Cadence
greedySelect candidates = return $ fst $ head $ sortOn (negate . snd) candidates
```

2. **Add to Builder** as configurable option:
```haskell
data SelectionStrategy = Gamma Double | Greedy | Uniform

generate :: SelectionStrategy -> CadenceState -> Int -> IO Progression
```

3. **Update tests** and documentation

### 9.4 Database Backend Swapping

To support PostgreSQL instead of Neo4j:

1. **Abstract database interface** in `Evaluation/Database/`:
```haskell
class HarmonicDatabase db where
  queryCadences :: db -> Cadence -> String -> IO [(Cadence, Double)]
  writeGraph :: db -> [Cadence] -> IO ()
```

2. **Implement for Neo4j** (existing):
```haskell
instance HarmonicDatabase Neo4jConnection where
  queryCadences = queryNeo4j
  writeGraph = writeNeo4j
```

3. **Implement for PostgreSQL** (new):
```haskell
instance HarmonicDatabase PostgresConnection where
  queryCadences = queryPostgres
  writeGraph = writePostgres
```

4. **Parameterize Builder** by database type

---

## 10. Performance Considerations

### 10.1 Neo4j Query Optimization

**Current Behavior**: Each chord generation = 1 database query

**Bottleneck**: Network latency for multi-chord progressions

**Optimization Opportunity**:
- Prefetch N-step lookahead
- Cache results in memory
- Batch queries for multiple candidates

**Estimated Improvement**: 30-50% faster for progressions >8 chords

**Implementation Complexity**: Medium

**Tracked in**: IMPROVEMENTS.md [PO-1]

### 10.2 Haskell Lazy Evaluation

**Progression Structure**: `Progression = Seq CadenceState`

**Benefit**: Lazy evaluation delays computation until needed

**Caveat**: Voice leading optimization (cyclic DP) must force full evaluation

**Tip**: Use `deepseq` for strict evaluation in performance-critical paths

### 10.3 Memory Usage Patterns

**Typical Generation**:
- 16-chord progression: ~2MB heap
- 64-chord progression: ~8MB heap
- Neo4j query results: ~1KB per candidate

**Garbage Collection**: GHC's generational GC handles short-lived cadence objects efficiently

**Optimization**: Avoid accumulating old progressions in REPL sessions (use `let!` for strict evaluation)

### 10.4 Typical Generation Timings

Measured on M1 MacBook Pro (2021), Neo4j local Docker:

| Operation | Time | Notes |
|-----------|------|-------|
| Generate 4-chord progression | 80ms | Includes 4 Neo4j queries |
| Generate 16-chord progression | 300ms | Includes 16 Neo4j queries |
| Generate 64-chord progression | 1.2s | Includes 64 Neo4j queries |
| CSV import (full corpus) | 2.5s | 5000 chords → 15000 cadences |
| Neo4j graph write | 8s | 15000 cadences, 45000 edges |
| Voice leading optimization (flow) | 5ms | Cyclic DP on 16 chords |

**Conclusion**: Database queries dominate generation time. Optimize Neo4j queries first.

---

## Appendix A: File Locations Quick Reference

### Source Files
- **Builder**: `src/Harmonic/Framework/Builder.hs`
- **Pitch algebra**: `src/Harmonic/Rules/Types/Pitch.hs`
- **Harmony types**: `src/Harmonic/Rules/Types/Harmony.hs`
- **Progression**: `src/Harmonic/Rules/Types/Progression.hs`
- **Dissonance**: `src/Harmonic/Evaluation/Scoring/Dissonance.hs`
- **Voice leading**: `src/Harmonic/Evaluation/Scoring/VoiceLeading.hs`
- **Neo4j queries**: `src/Harmonic/Evaluation/Database/Query.hs`
- **Probabilistic**: `src/Harmonic/Traversal/Probabilistic.hs`
- **Tidal bridge**: `src/Harmonic/Interface/Tidal/Bridge.hs`
- **Arranger**: `src/Harmonic/Interface/Tidal/Arranger.hs`

### Test Files
- **Test runner**: `test/Spec.hs`
- **Pitch tests**: `test/Harmonic/Rules/Types/PitchSpec.hs`
- **Harmony tests**: `test/Harmonic/Rules/Types/HarmonySpec.hs`
- **Builder tests**: `test/Harmonic/Framework/BuilderSpec.hs`
- **Interface tests**: `test/Harmonic/Interface/Tidal/BridgeSpec.hs`

### Documentation
- **User guide**: `README.md`
- **Architecture**: `ARCHITECTURE.md` (this file)
- **TidalCycles tutorial**: `live/USER_GUIDE.tidal`
- **Development guidelines**: `CLAUDE.md`
- **Improvements tracking**: `IMPROVEMENTS.md`

### Configuration
- **Haskell package**: `package.yaml`
- **Neo4j compose**: `docker-compose.yml`
- **TidalCycles boot**: `live/BootTidal.hs`

---

## Appendix B: Glossary

- **Cadence**: A transition between two chords, encoded as (function, movement, zero-form intervals)
- **CadenceState**: Runtime representation of a chord with absolute root, function, intervals, and spelling
- **ChordSlice**: Raw pitch-class set from CSV parsing, before triad interpretation
- **Enharmonic Spelling**: Choice of sharp vs flat notation (e.g., C# vs Db)
- **Flow Voicing**: Voice-led voicing using cyclic dynamic programming to minimize voice movement
- **HarmonicContext**: Three-filter system (overtones, key, roots) constraining generation
- **Hindemith Vector**: Interval dissonance weighting system (perfect fifth = 1, tritone = 24)
- **Movement**: Root motion between chords (Asc P 5, Desc P 4, Unison, Tritone)
- **Overtone Series**: Harmonic series of a fundamental pitch (C → [C, E, G, Bb, ...])
- **PitchClass**: ℤ₁₂ cyclic group element representing pitch-class (0=C, 1=C#, ..., 11=B)
- **Progression**: Monoid-wrapped sequence of CadenceState
- **Zero-Form**: Intervals shifted so first pitch = 0 (transposition-invariant)

---

**Document Version**: 1.0
**Last Updated**: 2026-01-14
**For Questions**: See README.md or open GitHub issue
