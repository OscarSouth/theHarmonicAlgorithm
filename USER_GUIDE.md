# User Guide: TidalCycles Workflow

This guide covers the live-coding workflow in the TidalCycles interpreter environment. For installation, architecture, and theoretical background, see [README.md](README.md).

## Generation Pipeline Overview

The generation process follows a **six-stage pipeline** that creates musically coherent progressions:

### Six-Stage Pipeline

**[1] PRIOR STATE** → Cadence + Root (e.g., C major)  
**[2] CANDIDATE POOL** → Graph + Fallback ranked by dissonance (30 candidates per step)  
**[3] SELECTED FROM DB** → Movement + Zero-form intervals (pitch-agnostic)  
**[4] ADVANCE** → Root motion computation (prior PC + movement = posterior PC)  
**[5] POSTERIOR STATE** → New root + cadence  
**[6] RENDER** → Transpose intervals by root + name chord  

**Verification:** DB stored name == computed name (guaranteed by zero-form invariant)

### Zero-Form Invariant

All Cadence objects store intervals in **zero-form** `[P 0, ...]` (relative, pitch-agnostic):
- `[P 0, P 3, P 7]` = minor triad (any root, any inversion)
- Transposition: `posterior chord = rootPC + cadenceIntervals`
- Database, Chord pairs, and fallback generation all produce zero-form cadences
- Naming consistency: DB functionality always matches computed functionality

## Prerequisites

Before you begin:
1. Neo4j database running with populated cadence graph (`docker compose up -d neo4j && stack run`)
2. SuperCollider running with SuperDirt
3. TidalCycles editor plugin configured (VS Code, Pulsar, or nvim)

## Quick Start

```haskell
-- 1. Set up harmonic context (what key/mode/root restrictions?)
let ctx = defaultContext  -- No filtering, full chromatic

-- 2. Create starting state
let start = initCadenceState 0 "C" [0,4,7] FlatSpelling

-- 3. Generate a 16-bar progression (use 'gen' for String-friendly API)
progA <- gen start 16 "debussy stravinsky" 1.0 ctx

-- 4. Play it
d1 $ note (harmony progA "<0 1 2 3>") # s "superpiano"
```

## Session Setup

The `live/state.tidal` template provides a standard session structure. Evaluate it at session start:

```haskell
:load live/state.tidal
```

This sets up:
- Harmonic context (`ctx`)
- Generator config (`cfg`)
- Common scales
- Instrument launchers

## Harmonic Context (Filtering)

The `HarmonicContext` constrains which cadences are valid candidates during generation:

```haskell
data HarmonicContext = HarmonicContext
  { hcOvertones :: Text  -- Pitch class filter
  , hcKey       :: Text  -- Key signature filter  
  , hcRoots     :: Text  -- Root motion filter
  }
```

### Filter Notation

| Filter | Syntax | Examples |
|--------|--------|----------|
| **Overtones** | Space-separated pitches | `"E A D G"`, `"C"`, `"G E' A'"` |
| **Key** | Key signature | `"#"` (G), `"bb"` (Bb), `"2#"` (D) |
| **Roots** | Allowed bass notes | `"E F# G"`, `"1b"`, `"*"` |
| **Wildcard** | Match all | `"*"`, `"all"`, `"chr"` |

### Common Contexts

Use the `hContext` function (String-friendly) instead of the `HarmonicContext` constructor (Text-based) in TidalCycles:

```haskell
-- No restrictions (chromatic)
defaultContext  -- "*" "*" "*"

-- E minor pentatonic, F major key
hContext "E G A B D" "1b" "*"

-- Jazz ii-V-I in C
hContext "C D E G" "*" "D G C"

-- Dorian over D (D Dorian = D E F G A B C)
hContext "D E F G A B C" "1b" "D"

-- Bass tuning overtones (E A D G)
hContext "E A D G" "*" "*"

-- Blues: G overtones + blue notes
hContext "G E' A' A#'" "*" "E G"
```

### String vs Text API

TidalCycles uses `OverloadedStrings` with its own `Stringy` typeclass. To avoid type conflicts, use the String-friendly wrappers:

| Text Function | String Wrapper | Use in TidalCycles |
|---------------|----------------|--------------------|
| `parseOvertones` | `overtones` | `overtones "E A D G"` |
| `parseKey` | `key` | `key "#"` |
| `parseFunds` | `funds` | `funds "E F# G"` |
| `isWildcard` | `wildcard` | `wildcard "*"` |
| `harmonicContext` | `hContext` | `hContext "*" "*" "*"` |
| `generate` | `gen` | `gen start 8 "debussy" 1.0 ctx` |
| `generateWith` | `genWith` | `genWith config start 8 "debussy" 1.0 ctx` |

## Generator Configuration

Control the generation algorithm behavior:

```haskell
data GeneratorConfig = GeneratorConfig
  { configHomingThreshold :: Double  -- When to start homing (default: 0.75)
  , configHomingStrength  :: Double  -- How strongly to pull (default: 0.5)
  , configMinCandidates   :: Int     -- Minimum transitions per step (default: 3)
  }

-- Customize
let cfg = defaultConfig { 
  configHomingThreshold = 0.75,  -- Start homing at 75%
  configHomingStrength = 0.8,    -- Strong pull toward target
  configMinCandidates = 5        -- More variety
}
```

## Generating Progressions

### Unified Generation Interface — Three Functions, One Signature

The harmonic generator provides **three functions with identical type signatures** that differ only in their diagnostic output. This enables seamless switching between verbosity levels:

```haskell
genSilent   :: CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression
genStandard :: CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression
genVerbose  :: CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression
```

All three:
- Accept identical parameters in identical order
- Return `IO Progression` directly (not tuples)
- Print diagnostics as side effects (when applicable)
- Can be switched with a single-word change

This design makes it trivial to move between modes without changing the rest of your code:

```haskell
-- Development: explore behavior
prog <- genStandard start 8 "*" 0.5 ctx

-- Production: remove output (one word change)
prog <- genSilent start 8 "*" 0.5 ctx

-- Debugging: add traces (one word change)
prog <- genVerbose start 4 "*" 0.5 ctx
```

#### Silent Mode (Verbosity 0) — Production

**Best for:** Live performance, batch generation, when you only care about the progression.

```haskell
prog <- genSilent start 16 "*" 0.5 ctx
print prog  -- Just shows the progression, no diagnostic output
```

**Characteristics:**
- Zero diagnostic output
- Fastest execution (no I/O overhead)
- Returns progression directly

**Use when:**
- Generating music for live performance
- Running batch operations
- Working in production environments where output is unnecessary

#### Standard Mode (Verbosity 1) — Exploration

**Best for:** Understanding generation behavior, tuning context filters, and learning how the system works.

```haskell
prog <- genStandard start 8 "*" 0.5 ctx
```

**Console output for each step includes:**
- Prior and posterior cadence states with roots
- Graph candidate pool size and top 3 candidates (with confidence scores)
- Fallback candidate pool size and top 3 candidates
- Total pool size and gamma-weighted selection index
- Selected source (graph or fallback) and selected movement
- Rendered chord name
- Final progression visualization (4-column grid)

**Example output:**
```
═══════════════════════════════════════════════════════════════════
GENERATION DIAGNOSTICS
═══════════════════════════════════════════════════════════════════
Starting: ( pedal -> maj ) @ C
Entropy: 0.5
Generated: 8/8 chords

STEP 1:
  Prior: ( pedal -> maj ) @ C
  Candidates: graph=30, fallback=0, pool=30
    Top graph: [("ped->min",0.95),("ped->aug",0.87),...]
  Selected: graph @ index 5
  Movement: down_2nd
  Posterior: Bb
  Chord: Bbm

...
═══════════════════════════════════════════════════════════════════
FINAL PROGRESSION:
═══════════════════════════════════════════════════════════════════
Bar 1: | Cm   | Bbm  |
Bar 2: | Fm   | Bbm  |
```

**Use when:**
- Exploring generation behavior
- Understanding why certain candidates were selected
- Tuning context filters (overtones, keys, roots)
- Spotting pattern issues
- Learning how the system works

#### Verbose Mode (Verbosity 2) — Debugging

**Best for:** Debugging chord naming discrepancies, understanding voice leading computations, and investigating transformation pipeline issues.

```haskell
prog <- genVerbose start 2 "*" 0.5 ctx
```

**Console output includes everything from Standard mode, plus:**

**Transform trace** for each step:
- DB zero-form intervals before transposition (what's stored)
- Transposed pitch classes (DB intervals + root PC arithmetic)
- Normalized pitch classes (after fundamental adjustment)
- Final zero-form representation
- Detected root from pitch classes
- Computed chord name (derived from pitches)
- Comparison with DB stored chord name

**Advance trace** for each step:
- Prior and posterior root pitch classes
- Movement interval extracted from DB
- PC arithmetic: `(prior_PC + movement_interval) mod 12`
- Enharmonic spelling decision (flat/sharp)
- Final posterior root note name

**Example traces:**
```
[TRANSFORM TRACE]
  DB intervals: [0,4,7]
  Transposed: [0,4,7]
  Normalized: [0,4,7]
  Zero-form: [0,4,7]
  Detected root: Bb
  Computed name: Bbm
  DB stored name: min (normalized "Bbm" → "min")
[ADVANCE TRACE]
  0 + 10 = 10 (mod 12)
  C → Bb
```

**Performance:** Approximately 20-30% slower than Standard mode due to extra tracing computation.

**Use when:**
- Debugging chord naming discrepancies (computed vs. DB stored name)
- Understanding voice leading calculations
- Investigating transform pipeline issues
- Checking enharmonic spelling decisions
- Tuning generator configuration

### Custom Configuration with `'` Suffix Variants

All three functions have `'` suffix variants that accept custom `GeneratorConfig`:

```haskell
genSilent'   :: GeneratorConfig -> CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression
genStandard' :: GeneratorConfig -> CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression
genVerbose'  :: GeneratorConfig -> CadenceState -> Int -> String -> Double -> HarmonicContext -> IO Progression
```

Customize generation behavior:

```haskell
let cfg = defaultConfig 
      { cfgHomingThreshold = 0.8     -- Start homing at 80% (default: 75%)
      , cfgCompositionStrength = 0.8 -- Strong pull to resolution (default: 0.5)
      , cfgMinCandidates = 5         -- More variety (default: 3)
      }

-- Silent with custom config
prog1 <- genSilent' cfg start 16 "*" 0.5 ctx

-- Standard with custom config  
prog2 <- genStandard' cfg start 16 "*" 0.5 ctx

-- Verbose with custom config
prog3 <- genVerbose' cfg start 8 "*" 0.5 ctx
```

**Configuration options:**
- `cfgHomingThreshold` — When to start guiding toward resolution (0.0-1.0, default: 0.75)
- `cfgCompositionStrength` — How strongly to pull toward resolution (0.0-1.0, default: 0.5)
- `cfgMinCandidates` — Minimum candidates to keep per step (default: 3)

### Internal Tuple Functions (Advanced)

For advanced use cases that require manual diagnostics extraction or processing:

```haskell
generate'   :: CadenceState -> Int -> String -> Double -> HarmonicContext -> IO (Progression, GenerationDiagnostics)
generate''  :: CadenceState -> Int -> String -> Double -> HarmonicContext -> IO (Progression, GenerationDiagnostics)
genWith'    :: GeneratorConfig -> CadenceState -> Int -> String -> Double -> HarmonicContext -> IO (Progression, GenerationDiagnostics)
genWith''   :: GeneratorConfig -> CadenceState -> Int -> String -> Double -> HarmonicContext -> IO (Progression, GenerationDiagnostics)

printDiagnostics :: Int -> GenerationDiagnostics -> IO ()
```

These functions return tuples instead of side-effect diagnostics:
- `generate'` and `genWith'` return standard-level diagnostics
- `generate''` and `genWith''` return maximum diagnostics (with full traces)
- `printDiagnostics` reprints diagnostics at any verbosity level

Example:
```haskell
-- Generate once, extract tuple
(prog, diag) <- generate' start 8 "*" 0.5 ctx

-- Reprint at different verbosity levels
printDiagnostics 1 diag  -- standard output
printDiagnostics 2 diag  -- verbose output with traces
```

### Basic Progression Display

```haskell
-- Generate 16-bar progression
prog <- genSilent start 16 "debussy stravinsky" 1.0 ctx

-- Display progression (4-column grid)
print prog
-- Bar 1: | Dm   | F    | Am   | G    |
-- Bar 2: | Em   | Bdim | C    | Am   |
-- ...

-- Get progression length
progLength prog  -- Returns Int

-- Lookup specific chord by index
lookupChord prog 0   -- First chord
lookupChord prog 15  -- 16th chord
lookupChord prog 16  -- Wraps to 0 (modulo wrap)
```

## Playing Progressions

### Basic Playback

```haskell
-- Simple chord sequence
d1 $ note (harmony prog "<0 1 2 3>") # s "superpiano"

-- Arpeggiated
d1 $ note (harmony prog "0*4 1*4 2*4 3*4") # s "superpiano"

-- With pattern transformations
d1 $ note (harmony prog $ fast 2 "<0 1 2 3>") # s "superpiano"
```

### Voice Extraction

Use `voiceBy` to extract specific voices from the progression:

```haskell
-- All notes (full harmony)
voiceBy Harmony prog "<0 1 2 3>"

-- Root notes only (bass line)
voiceBy Roots prog "0"

-- Lowest note of voicing
voiceBy Bass prog "0"
```

## Enharmonic Spelling

The system uses **persistence-based enharmonic spelling with C flexibility**:

### Rule Set

1. **Same Pitch Class** (highest priority)
   - If prior and posterior have the same pitch class, use prior's actual spelling
   - The root hasn't changed, so keep its spelling consistent
   - Example: D# → D# keeps Sharp, Eb → Eb keeps Flat

2. **C (pitch class 0) is flexible**: Allows easy switching based on adjacent pitches
   - When C is prior and posterior is definite: adopt posterior's preference
   - When C is posterior and prior is definite: adopt prior's actual spelling
   - When both are C: persist current spelling

3. **Other pitches have definite preferences** (persist by default):
   - **Sharp-preferring:** F#, G, A, B, D, E, C# (6, 7, 9, 11, 2, 4, 1)
   - **Flat-preferring:** Db, Eb, F, Ab, Bb (1, 3, 5, 8, 10)

4. **Transition rule when both are definite (different pitch classes)**: Consensus-based switching
   - **DEFAULT**: Keep the prior's actual spelling
   - **SWITCH only when**: Both prior and posterior have the SAME preference AND it differs from current
   - **PERSIST if prior actual matches posterior preference** (consensus on what was chosen)
   - This prevents churn while allowing multi-pitch consensus changes

### Initial State

When creating a starting state, use `defaultEnharm` (applied in `generate` function):
- **C** → `FlatSpelling` (defaults to flat for initial state)
- **All other pitches** → their natural preference

### Example

```haskell
-- Starting on C with FlatSpelling (defaultEnharm)
let start = initCadenceState 0 "C" [0,4,7] FlatSpelling

-- Advance C (ambiguous, real Flat) → Db (FlatPref)
-- Result: C is flexible, Db prefers flat → FlatSpelling (persist flat)

-- Advance C (ambiguous, real Sharp) → Db (FlatPref)
-- Result: C is flexible, Db prefers flat → FlatSpelling (switch from sharp to flat!)

-- Advance Db (FlatPref, actual Flat) → D (SharpPref)
-- Result: They disagree → persist Flat (no switch, no consensus)

-- Advance D (SharpPref, actual Flat) → Eb (FlatPref)
-- Result: They disagree → persist Flat (no switch, no consensus)

-- Advance D (SharpPref, actual Flat) → D (SharpPref) [same PC!]
-- Result: Same pitch class → persist prior's actual spelling (Flat)

-- Advance D# → D# (same PC, sharp)
-- Result: Same pitch class → persist Sharp (root unchanged)

-- Advance Eb (FlatPref, actual Flat) → C (ambiguous)
-- Result: C is flexible, Eb prefers flat → FlatSpelling (persist flat through C)

-- Advance C (ambiguous, actual Flat) → Db (FlatPref)
-- Result: C is flexible, Db prefers flat → FlatSpelling (consensus stays)
```

### Flexibility with Purpose

The rules enable natural behavior:
- **Definite pitches persist**: A flat pitch stays flat unless multiple pitches agree to switch
- **C is flexible**: Can adopt adjacent preferences, enabling smooth key transitions
- **Consensus enables switches**: Only when multiple pitches agree on a preference do we switch away from current spelling
- **No churn from disagreement**: When pitches disagree, we respect the prior's choice

### Voicing Paradigms (3 Strategies)

The system provides 3 voicing paradigms using cyclic dynamic programming for globally optimal voice leading:

| Paradigm | Bass | Voice Leading | Use Case |
|----------|------|---------------|----------|
| `root` | Root always | Smooth, compact (cyclic DP) | Bass lines, traditional harmony |
| `flow` | Any (inversions allowed) | Smoothest motion (cyclic DP) | Jazz voicings, smooth progressions |
| `lite` | Any | None (literal) | Raw intervals, direct control |

```haskell
-- Apply different voicing paradigms to a progression
root prog  -- Root always in bass, globally optimized voice leading
flow prog  -- Any inversion allowed for smoothest voice leading
lite prog  -- Literal intervals, no voice leading
```

#### Root
Root is always the lowest note. Uses cyclic dynamic programming to find the globally optimal voicings that minimize voice movement across the entire progression (including wrap-around from last to first chord). All voices constrained to [0, 36] range (roughly C2 to C5).

#### Flow
Any inversion allowed—the algorithm picks whichever voicing creates the smoothest voice leading. Uses cyclic DP for global optimization. This often results in inversions that minimize jumps between adjacent chords.

#### Lite
Returns intervals exactly as stored with no voice leading applied. Use this when you want direct control or the raw data.

### The `arrange` Function (Launcher Paradigm)

For more complex arrangements, use `arrange` with voicing paradigms:

```haskell
-- arrange :: VoiceFunction -> Progression -> Pattern Time -> (Int, Int) -> [Pattern Int] -> Pattern ValueMap

-- Full harmony with flow voicing in mid range
d1 $ arrange flow prog 4 (-12, 12) ["0 1 2 3"] # s "superpiano"

-- Wide voicings for orchestral spread
d2 $ arrange wide prog 4 (-24, 24) ["0 1 2 3"] # s "superstrings"

-- Root notes in bass range  
d3 $ arrange rootNotes prog 4 (-24, -12) ["0"] # s "superbass"

-- Bass voice (lowest note of voicing)
d4 $ arrange bassNotes prog 4 (-24, 0) ["0"] # s "superbass"
```

#### Voice Functions

| Function | Purpose |
|----------|----------|
| `root` | Cyclic DP voice leading, root always in bass |
| `flow` | Cyclic DP voice leading, any inversion |
| `lite` | Literal (no voice leading) |
| `rootNotes` | Root note only |
| `bassNotes` | Lowest pitch class |

### Pattern-Based Lookup

For maximum flexibility, use pattern-based chord lookup:

```haskell
-- Lookup specific chord by index (0-based)
lookupChord prog 0   -- First chord
lookupChord prog 15  -- 16th chord
lookupChord prog 16  -- Wraps to first chord (modulo)

-- Convert index pattern to chord pattern
lookupProgression prog "<0 1 2 3>"

-- Get progression length
progLength prog  -- Returns Int
```

**Modulo wrap**: Index 16 on a 16-chord progression returns chord 0. This enables infinite cycling with patterns like `run 32` on shorter progressions.

## Instrument Launchers

Define custom launchers for your instrument setup:

```haskell
-- MIDI instrument launcher template
let p01 f s r d = d01 $ f $ arrange flow s r (-9,9) ["0 1 2 3"] # ch 1 |* vel d

-- Usage: p01 id progA 4 0.8
-- f = transformation (id, fast 2, slow 2, etc.)
-- s = progression
-- r = cycle length
-- d = dynamics/velocity

-- SuperDirt launcher
let sd1 f s r = d1 $ f $ arrange flow s r (-12,12) ["0 1 2 3"] # s "superpiano"
```

## Common Patterns

### Rhythmic Variations

```haskell
-- Whole notes
d1 $ arrange flow prog 4 (-12,12) ["0"] # s "superpiano"

-- Half notes  
d1 $ arrange flow prog 4 (-12,12) ["0 ~"] # s "superpiano"

-- Quarter notes
d1 $ arrange flow prog 4 (-12,12) ["0 ~ ~ ~"] # s "superpiano"

-- Arpeggios
d1 $ arrange flow prog 4 (-12,12) ["0 1 2 3 4 5 6 7"] # s "superpiano"
```

### Layered Arrangement

```haskell
-- Bass + chords + melody
d1 $ arrange bass prog 4 (-24,-12) ["0"] # s "superbass"
d2 $ arrange flow prog 4 (-6,6) ["0 1 2 3"] # s "superpiano" 
d3 $ arrange root prog 4 (12,24) ["0 ~ ~ ~"] # s "supersaw"
```

### Progressive Builds

```haskell
-- Start sparse, add density
d1 $ arrange flow prog 4 (-12,12) ["0"] # s "superpiano"
-- ... later
d1 $ arrange flow prog 4 (-12,12) ["0 1"] # s "superpiano"
-- ... later  
d1 $ arrange flow prog 4 (-12,12) ["0 1 2 3"] # s "superpiano"
```

## Scales Reference

Common scales (pitch class lists for `toScale`):

```haskell
let cMaj = [0,2,4,5,7,9,11]
let cMin = [0,2,3,5,7,8,10]
let cPent = [0,2,4,7,9]
let chromatic = [0,1,2,3,4,5,6,7,8,9,10,11]

-- Modes
let dorian = [0,2,3,5,7,9,10]
let phrygian = [0,1,3,5,7,8,10]
let lydian = [0,2,4,6,7,9,11]
let mixolydian = [0,2,4,5,7,9,10]
```

## Workflow Summary

1. **Set context**: Define harmonic constraints (`ctx`)
2. **Configure**: Adjust generator settings (`cfg`) 
3. **Generate**: Create progression (`prog <- generate ctx cfg 16`)
4. **Preview**: Print to see chord names (`print prog`)
5. **Perform**: Use `arrange`, `harmony`, or `voiceBy` to play
6. **Layer**: Add bass, melody, and rhythm parts
7. **Iterate**: Generate new progressions, combine with `<>` (Semigroup)

## Troubleshooting

**No candidates found**: Loosen context filters (use more wildcards)

**Progression sounds wrong**: Check key signature filter matches your harmonic intent

**Pattern doesn't loop**: Ensure your index pattern fits the progression length, or rely on modulo wrap

**Database connection error**: Verify Neo4j is running (`docker compose ps`)
