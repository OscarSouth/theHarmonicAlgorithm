# theHarmonicAlgorithm - Agent Guidelines

## Project Overview

A Haskell library for generating harmonic progressions trained on the Yale Classical Archives Corpus (YCACL). Uses Neo4j graph database for storing cadence transitions and integrates with TidalCycles for live music coding.

## Architecture

Four-layer system following the **Creative Systems Framework (R→E→T)**:
- **Layer A (Memory)**: R component - Rules/constraints, CSV parsing, Neo4j writes
- **Layer B (Brain)**: Types - pitch-class algebra, harmony naming, voice leading
- **Layer C (Hands)**: E→T components - Evaluation (database queries), Traversal (probabilistic selection)
- **Layer D (Voice)**: Interface - TidalCycles bridge, pattern lookup, arrangement

**Note**: The "R" in Layer A refers to the R→E→T (Rules→Evaluation→Traversal) pipeline, not the R programming language.

Key directories:
- `src/Harmonic/Core/` - Music theory primitives (Pitch, Harmony, VoiceLeading, etc.)
- `src/Harmonic/Database/` - Neo4j integration (Query.hs, Graph.hs)
- `src/Harmonic/Tidal/` - TidalCycles interface
- `test/` - HSpec + QuickCheck test suite
- `live/` - TidalCycles scripts and boot configuration
- `theHarmonicAlgorithmLegacy/` - Legacy implementation (source of truth for behavior)

## Codebase State & Guiding Principles

**CRITICAL**: The codebase is in an intermediate refactoring state. Apply these principles:

### 1. Legacy as Source of Truth
The legacy implementation (`theHarmonicAlgorithmLegacy/`) functioned correctly and was thoroughly verified. When behavior is unclear or results seem wrong, **compare against legacy behavior**.

### 2. Minimize Code and Complexity
The overarching goal is to minimize code and complexity while facilitating the core TidalCycles interfaces. Before adding code, ask:
- Is this strictly necessary for the current functionality?
- Can this be achieved more simply?
- Does the legacy implementation have a simpler approach?

### 3. Suspect All Existing Code
Previous AI agents may have introduced:
- Unnecessary/hallucinated logic
- Stubbed-out required functionality
- Incorrectly modernized implementations

**Every detail of structure, design, and implementation should be examined.** Nothing is concrete until final completion.

### 4. Tests Are Not Infallible
Test suites should be **interrogated and changed** to accommodate actual requirements. Tests may:
- Test incorrect behavior
- Be missing critical cases
- Have been added speculatively by previous agents

When behavior differs from legacy, **trust legacy behavior over existing tests**.

### 5. Incomplete Features
Some features are not yet implemented. Currently:
- Composer specification (e.g., "bach", "debussy") is **not implemented** - use `"*"` for all operations
- Use `"*"` to aggregate all composer intent from the deterministic graph

## Pre-Work Checklist

**Before starting ANY work, execute these steps:**

```bash
# 1. Ensure Neo4j is running
docker compose up -d neo4j

# 2. Verify Neo4j is accessible
curl -s http://localhost:7474 > /dev/null && echo "Neo4j OK" || echo "Neo4j FAILED - do not proceed"

# 3. Verify project builds
stack build

# 4. Run test suite to establish baseline
stack test
```

Only proceed if all checks pass.

## Memory Systems (MCP Servers)

This project uses **two memory systems** to preserve knowledge across sessions. Both should be actively used throughout development.

### System 1: mem0 (Conversational Memory)

**Purpose**: Stores project context, user preferences, and workflow patterns as natural language memories.

**When to use**:
- At the **start of each session**: Query to recall project context
- When discovering **new patterns or quirks** in the codebase
- When the user expresses a **preference or constraint**
- At the **end of a session**: Store key learnings

**Query existing memories**:
```
Tool: mcp__mem0__search-memories
- query: "Neo4j database" or "vertical slices" or "composer specification"
- userId: "mem0-mcp-user"
```

**Add new memories**:
```
Tool: mcp__mem0__add-memory
- content: "User prefers X approach for Y feature"
- userId: "mem0-mcp-user"
```

**Current memories include**:
- Neo4j connection requirements and startup
- Legacy codebase as source of truth
- Vertical slice workflow preferences
- Composer specification status (not implemented)
- Architecture layers and boundaries
- Zero-form invariant
- Code minimization principles

### System 2: memory (Knowledge Graph)

**Purpose**: Stores structured entities and relations representing the codebase architecture.

**When to use**:
- To understand **module dependencies** before making changes
- When exploring how components **relate to each other**
- To identify **which modules to check** when debugging
- When **adding new modules** (create entities and relations)

**Read the full graph**:
```
Tool: mcp__memory__read_graph
```

**Query specific nodes**:
```
Tool: mcp__memory__search_nodes
- query: "Builder" or "TidalCycles" or "Layer B"
```

**Add new entities**:
```
Tool: mcp__memory__create_entities
- entities: [{"name": "NewModule", "entityType": "CoreModule", "observations": ["description"]}]
```

**Add new relations**:
```
Tool: mcp__memory__create_relations
- relations: [{"from": "ModuleA", "to": "ModuleB", "relationType": "depends-on"}]
```

**Current graph structure**:
- **Entities**: Pitch, Harmony, VoiceLeading, Builder, Interface, Arranger, Neo4jDatabase, LegacyCodebase
- **Relations**: Shows dependency chains (e.g., Builder → Harmony → Pitch)

### Memory Workflow

**At session start**:
1. Query mem0 for relevant project context: `search-memories` with your current task keywords
2. Read knowledge graph to understand affected modules: `read_graph` or `search_nodes`
3. Use retrieved knowledge to inform your approach

**During development**:
- When discovering new patterns → add to mem0
- When creating new modules → add entities and relations to knowledge graph
- When user states preferences → add to mem0

**At session end** (or after major milestones):
- Store key learnings in mem0
- Update knowledge graph if architecture changed

### Validation

**Test memory systems are working**:
```bash
# mem0 test
Tool: mcp__mem0__search-memories with query "Neo4j"
# Should return: Neo4j connection info and requirements

# Knowledge graph test
Tool: mcp__memory__open_nodes with names ["Builder", "Interface"]
# Should return: Entity details with observations and relations
```

## Development Workflow: Vertical Slices

**CRITICAL**: All changes must be delivered in vertical slices - the minimum deliverable working and verifiable unit.

### Slice Workflow

1. **Identify** the smallest atomic change that can be independently verified
2. **Implement** only that slice
3. **Verify** using BOTH verification methods below
4. **Commit** (if requested) only after verification passes
5. **Repeat** for the next slice

### What Constitutes a Slice

- A single function implementation
- A single type definition with its instances
- A single test case addition
- A single bug fix
- A single refactor of one function

**NOT a slice**: Multiple unrelated changes, or changes that span multiple modules without clear dependency.

## Mandatory Verification

**EVERY slice MUST be verified before moving to the next slice.**

### Verification Method A: Test Suite

Run the full test suite after every change:

```bash
stack test
```

Expected: All tests pass. Any failures must be fixed before proceeding.

### Verification Method B: GHCi REPL

For any code that affects runtime behavior, verify interactively:

```bash
cd /Users/oscarsouth/.stack/global-project && stack ghci << 'EOF'
:set -XOverloadedStrings
import Harmonic.Lib

-- Example verification for Pitch module:
import Harmonic.Core.Pitch
transpose 5 (pc 7)  -- Should yield P 0 (wraps mod 12)

-- Example verification for Harmony module:
import Harmonic.Core.Harmony
nameFuncTriad [0, 4, 7]  -- Should yield "maj"

-- Example verification for generation:
import Harmonic.Core.Builder
ctx <- harmonicContext "*" "*" "*"
-- Further generation tests as needed

:quit
EOF
```

### Verification Method C: Compare Against Legacy

When behavior is unclear or tests fail unexpectedly, compare against the legacy implementation:

```bash
# The legacy codebase is in theHarmonicAlgorithmLegacy/
# Check how the same operation behaves in the legacy system
```

Key legacy files for reference:
- `theHarmonicAlgorithmLegacy/src/MusicData.hs` - Original music data types and naming
- `theHarmonicAlgorithmLegacy/src/Lib.hs` - Original interface and generation
- `theHarmonicAlgorithmLegacy/src/Arranger.hs` - Original voicing/arrangement
- `theHarmonicAlgorithmLegacy/src/GraphDB.hs` - Original Neo4j integration
- `theHarmonicAlgorithmLegacy/src/Overtone.hs` - Original overtone filtering
- `theHarmonicAlgorithmLegacy/src/Markov.hs` - Original Markov transitions
- `theHarmonicAlgorithmLegacy/src/Utility.hs` - Original utility functions

Compare function signatures, behaviors, and outputs between legacy and modernized implementations.

**If modernized behavior differs from legacy, investigate why before assuming modernized is correct.**

### REPL Verification Examples by Module

#### Pitch (src/Harmonic/Core/Pitch.hs)
```haskell
import Harmonic.Core.Pitch
pc 14           -- P 2 (wraps mod 12)
transpose 7 (P 5)  -- P 0
interval (P 3) (P 7)  -- 4
```

#### Harmony (src/Harmonic/Core/Harmony.hs)
```haskell
import Harmonic.Core.Harmony
nameFuncTriad [0, 4, 7]   -- "maj"
nameFuncTriad [0, 3, 7]   -- "min"
nameFuncChord [0, 4, 7, 11]  -- "maj7"
```

#### VoiceLeading (src/Harmonic/Core/VoiceLeading.hs)
```haskell
import Harmonic.Core.VoiceLeading
-- Test voice leading cost calculations
```

#### Progression (src/Harmonic/Core/Progression.hs)
```haskell
import Harmonic.Core.Progression
-- Test progression combinators
```

#### Builder (src/Harmonic/Core/Builder.hs)
```haskell
import Harmonic.Lib

-- Create context and starting state
ctx <- harmonicContext "*" "*" "*"
let start = initCadenceState 0 "C" [0,4,7] FlatSpelling

-- Test generation (requires Neo4j)
prog <- genSilent start 4 "*" 0.5 ctx
print prog  -- Should show 4-chord progression

-- Test different verbosity levels
prog' <- genStandard start 4 "*" 0.5 ctx  -- With diagnostics
prog'' <- genVerbose start 4 "*" 0.5 ctx  -- Full traces
```

#### Interface (src/Harmonic/Tidal/Interface.hs)
```haskell
import Harmonic.Lib

-- Generate a progression first (requires Neo4j)
ctx <- harmonicContext "*" "*" "*"
let start = initCadenceState 0 "C" [0,4,7] FlatSpelling
prog <- genSilent start 4 "*" 0.5 ctx

-- Test pattern lookup with modulo wrap
lookupChord prog 0    -- First chord
lookupChord prog 4    -- Wraps to index 0 (infinite cycling)

-- Test voicing functions
rootNotes prog        -- Extract root notes
bassNotes prog        -- Extract bass notes

-- Test arrange (preserves launcher paradigm)
arrange prog
```

#### Arranger (src/Harmonic/Tidal/Arranger.hs)
```haskell
import Harmonic.Lib

-- Generate progression first
ctx <- harmonicContext "*" "*" "*"
let start = initCadenceState 0 "C" [0,4,7] FlatSpelling
prog <- genSilent start 8 "*" 0.5 ctx

-- Test progression combinators
rotate 2 prog         -- Rotate by 2 positions
excerpt 0 4 prog      -- Extract first 4 chords
fuse prog prog        -- Combine progressions

-- Test voicing strategies
voiceBy root prog     -- Root position voicing
voiceBy flow prog     -- Flow voicing (minimal movement)
voiceBy lite prog     -- Lite voicing
voiceBy literal prog  -- Literal (as-is)
```

## Neo4j Database (REQUIRED)

**Neo4j must be running for ALL verification.** Start before any work:

```bash
docker compose up -d neo4j
```

Connection: `bolt://localhost:7687` with credentials `neo4j/password`

Verify Neo4j is accessible before starting work:
```bash
curl -s http://localhost:7474 > /dev/null && echo "Neo4j running" || echo "Neo4j NOT running"
```

## Build Commands

```bash
stack build     # Compile library
stack test      # Run test suite (MANDATORY after changes)
stack ghci      # Interactive REPL for verification
stack run       # Populate Neo4j (requires docker)
```

## Code Conventions

### Zero-Form Invariant
All Cadence objects store intervals in zero-form `[P 0, ...]` (relative, pitch-agnostic). This is enforced at:
- Neo4j query results
- `toCadence` conversion
- Fallback generation

### Type Safety
- `PitchClass` is a newtype with Z12 algebra
- Use `pc` or `mkPitchClass` constructors
- Never use raw integers where `PitchClass` is expected

### Layer Boundaries
- Layer B (Brain) modules must not import from Layer C or D
- Layer C (Hands) may import from B but not D
- Layer D (Voice) may import from B and C

## Test Organization

Tests mirror source structure:
- `test/Harmonic/Core/PitchSpec.hs` - Z12 algebra properties
- `test/Harmonic/Core/HarmonySpec.hs` - Chord naming
- `test/Harmonic/Core/VoiceLeadingSpec.hs` - Voice leading costs
- `test/Harmonic/Core/BuilderSpec.hs` - Generation engine
- `test/Harmonic/Database/QuerySpec.hs` - Composer weight parsing
- `test/Harmonic/Tidal/InterfaceSpec.hs` - Pattern interface

When adding new functionality:
1. Add corresponding test in the appropriate Spec file
2. Run `stack test` to verify the test initially fails (TDD) or passes
3. Implement the feature
4. Run `stack test` to verify all tests pass

## Common Verification Scenarios

### After modifying Pitch.hs
```bash
stack test --test-arguments="--match Pitch"
```
Then REPL verify:
```haskell
import Harmonic.Core.Pitch
-- Test your changes
```

### After modifying Builder.hs
```bash
stack test --test-arguments="--match Builder"
```
Then REPL verify with actual generation (requires Neo4j).

### After modifying Interface.hs
```bash
stack test --test-arguments="--match Interface"
```
Then REPL verify pattern lookup behavior.

## Example: Working Through a Slice

**Task**: Add a new function `invertChord` to Harmony.hs

**Slice 1**: Add the function signature and implementation
```haskell
-- In src/Harmonic/Core/Harmony.hs
invertChord :: Chord -> Int -> Chord
invertChord (Chord intervals) n = Chord (rotate n intervals)
```

**Verify Slice 1**:
```bash
# Test suite
stack test

# REPL verification
cd /Users/oscarsouth/.stack/global-project && stack ghci << 'EOF'
:set -XOverloadedStrings
import Harmonic.Core.Harmony
invertChord (Chord [P 0, P 4, P 7]) 1  -- Should show first inversion
:quit
EOF
```

**Slice 2**: Add test case (only after Slice 1 verified)
```haskell
-- In test/Harmonic/Core/HarmonySpec.hs
it "inverts chord correctly" $
  invertChord (Chord [P 0, P 4, P 7]) 1 `shouldBe` Chord [P 4, P 7, P 0]
```

**Verify Slice 2**:
```bash
stack test --test-arguments="--match invert"
```

Only after BOTH slices pass verification is the feature complete.

## Checklist Before Completing Any Task

- [ ] Change implemented in smallest possible slice
- [ ] `stack test` passes
- [ ] REPL verification confirms expected behavior
- [ ] Behavior matches legacy implementation (if applicable)
- [ ] No unnecessary code or complexity added
- [ ] No unrelated changes included
- [ ] Layer boundaries respected
- [ ] Tests updated if they were incorrect
