# Implementation Plan: Pitch Removal Support with `-` Operator

## Executive Summary

Add pitch removal support to harmonic context parsing using the `-` prefix operator. This enables subtractive filtering: `"1b 2# -G'"` → (F major ∪ D major) \ {G pitch}.

**Core Principle**: Order of operations is `(union all positive tokens) \ (union all negative tokens)`.

**Context-Specific Behavior**:
- **Overtones**: `G` = G overtones, `G'` = G pitch, `-G` = remove G overtones, `-G'` = remove G pitch
- **Key**: `G` = G major scale, `G'` = G pitch, `-G` = remove G major scale, `-G'` = remove G pitch
- **Roots**: Same as key (fundamentals only)

---

## 1. CORE ALGORITHM

### 1.1 Separation Strategy

**Algorithm**:
```haskell
-- Partition tokens into include/exclude lists based on '-' prefix
partitionTokens :: Text -> ([Text], [Text])
partitionTokens input = 
  let tokens = T.words $ T.toLower input
      (negTokens, posTokens) = partition (T.isPrefixOf "-") tokens
      -- Strip '-' prefix from negative tokens
      negTokens' = map (T.drop 1) negTokens
  in (posTokens, negTokens')

-- Apply include/exclude logic
applyRemoval :: (Text -> [PitchClass]) -> Text -> [PitchClass]
applyRemoval parseFunc input
  | isWildcard input = chromaticSet
  | otherwise = 
      let (includes, excludes) = partitionTokens input
          -- Parse each partition separately
          includePcs = unique $ concatMap parseFunc includes
          excludePcs = unique $ concatMap parseFunc excludes
      in includePcs \\ excludePcs
```

**Key Points**:
- Use `Data.List (partition, (\\))` for partitioning and set difference
- Strip `-` prefix BEFORE delegating to existing parsing logic
- Empty includes list → empty result (NOT chromatic set)
- Wildcard handling happens BEFORE partitioning

### 1.2 Edge Cases

| Input | Behavior | Result |
|-------|----------|--------|
| `"*"` | Wildcard shortcut | `[0..11]` |
| `"* -C"` | Parse tokens normally | `[0..11] \\ [0] = [1..11]` |
| `"-*"` | Remove all | `[] \\ [0..11] = []` |
| `"-C -D -E"` | No includes | `[] \\ [0,2,4] = []` |
| `"C -C"` | Include then exclude | `[0,4,7,10] \\ [0,4,7,10] = []` |
| `"1b -invalid"` | Invalid exclude ignored | `[0,2,4,5,7,9,10]` |

**Decision**: Wildcard `*` is handled as shortcut BEFORE token parsing. Otherwise, empty includes list yields empty result.

---

## 2. FUNCTION-SPECIFIC CHANGES

### 2.1 parseOvertones' (lines 290-314)

**Current Implementation**:
```haskell
parseOvertones' :: Int -> Text -> [PitchClass]
parseOvertones' n input
  | isWildcard input = chromaticSet
  | otherwise = unique $ concatMap (parseGeneralToken n) tokens
  where
    tokens = T.words $ T.toLower input
```

**New Implementation**:
```haskell
parseOvertones' :: Int -> Text -> [PitchClass]
parseOvertones' n input
  | isWildcard input = chromaticSet
  | otherwise = 
      let (includes, excludes) = partitionTokens input
          includePcs = unique $ concatMap (parseGeneralToken n) includes
          excludePcs = unique $ concatMap (parseGeneralToken n) excludes
      in includePcs \\ excludePcs
  where
    -- partitionTokens defined in module scope
```

**Key Points**:
- `parseGeneralToken` already handles `G` vs `G'` distinction (lines 298-314)
- `-G` → `parseGeneralToken "g"` → overtones of G: `[7,11,2,6]`
- `-G'` → `parseGeneralToken "g'"` → single pitch: `[7]`
- No changes needed to `parseGeneralToken` itself

### 2.2 parseKey' (lines 171-174)

**Current Implementation**:
```haskell
parseKey' :: Int -> Text -> [PitchClass]
parseKey' _ input
  | isWildcard input = chromaticSet
  | otherwise = keyToPitchClasses input
```

**Challenge**: `keyToPitchClasses` operates on entire input string, not individual tokens.

**New Implementation**:
```haskell
parseKey' :: Int -> Text -> [PitchClass]
parseKey' _ input
  | isWildcard input = chromaticSet
  | otherwise = 
      let (includes, excludes) = partitionTokens input
          -- Parse includes: try as single key, else as space-separated tokens
          includePcs = if length includes == 1 && not (hasSpaces $ head includes)
                       then keyToPitchClasses (head includes)
                       else unique $ concatMap parseKeyToken includes
          excludePcs = unique $ concatMap parseKeyToken excludes
      in includePcs \\ excludePcs
  where
    -- New helper: parse individual token in key context
    parseKeyToken :: Text -> [PitchClass]
    parseKeyToken token
      | "'" `T.isSuffixOf` token = 
          -- Prime notation: single pitch only
          case noteNameToPitchClass (T.init token) of
            Just pc -> [pc]
            Nothing -> []
      | otherwise = 
          -- Try as key signature, then as note name
          case parseKeySignature token of
            Just fifths -> keyPitches fifths
            Nothing -> case noteNameToPitchClass token of
              Just pc -> [pc]  -- Single pitch (not overtones!)
              Nothing -> []
```

**Key Points**:
- `G` in key context → G major scale `[0,2,4,6,7,9,11]`
- `G'` in key context → single pitch `[7]`
- `-G` → remove G major scale
- `-G'` → remove G pitch
- Backward compatible: single token input uses `keyToPitchClasses` as before

### 2.3 parseFunds' (lines 246-255)

**Current Implementation**:
```haskell
parseFunds' :: Int -> Text -> [PitchClass]
parseFunds' _ input
  | isWildcard input = chromaticSet
  | otherwise = 
      -- Try parsing as key signature first
      case parseKeySignature (T.toLower input) of
        Just fifths -> keyPitches fifths
        Nothing -> 
          -- Otherwise parse as space-separated note names
          mapMaybe noteNameToPitchClass $ T.words $ T.toLower input
```

**Challenge**: Single-token key signature path vs multi-token note names path.

**New Implementation**:
```haskell
parseFunds' :: Int -> Text -> [PitchClass]
parseFunds' _ input
  | isWildcard input = chromaticSet
  | otherwise = 
      let (includes, excludes) = partitionTokens input
          -- Parse includes: try as single key sig, else as tokens
          includePcs = if length includes == 1 && not (hasSpaces $ head includes)
                       then case parseKeySignature (head includes) of
                              Just fifths -> keyPitches fifths
                              Nothing -> case noteNameToPitchClass (head includes) of
                                Just pc -> [pc]
                                Nothing -> []
                       else unique $ concatMap parseFundsToken includes
          excludePcs = unique $ concatMap parseFundsToken excludes
      in includePcs \\ excludePcs
  where
    -- New helper: parse individual token in funds context
    parseFundsToken :: Text -> [PitchClass]
    parseFundsToken token = 
      case parseKeySignature token of
        Just fifths -> keyPitches fifths
        Nothing -> case noteNameToPitchClass token of
          Just pc -> [pc]
          Nothing -> []
```

**Key Points**:
- Funds context does NOT support prime notation (no overtones)
- `G` → try as key signature (fails), then as note name: `[7]`
- `1b` → key signature: F major scale `[0,2,4,5,7,9,10]`
- `-G` → remove pitch 7
- `-1b` → remove F major scale

---

## 3. HELPER FUNCTIONS

### 3.1 Module-Level Helpers

Add to `/Users/oscarsouth/.stack/global-project/src/Harmonic/Core/Filter.hs` after imports:

```haskell
import Data.List (nub, sort, partition)
import qualified Data.List as List

-- |Partition tokens into include/exclude lists based on '-' prefix
-- Returns (includes, excludes) with '-' prefix stripped from excludes
partitionTokens :: Text -> ([Text], [Text])
partitionTokens input = 
  let tokens = T.words $ T.toLower input
      (negTokens, posTokens) = partition (T.isPrefixOf "-") tokens
      -- Strip '-' prefix from negative tokens
      negTokens' = map (T.drop 1) negTokens
  in (posTokens, negTokens')
```

**Location**: Lines 73-82 (after type definitions, before wildcard handling)

### 3.2 Generic Wrapper (Optional)

Consider adding a generic wrapper to reduce duplication:

```haskell
-- |Generic parser with inclusion/exclusion support
-- Takes a token parser function and input string
-- Returns pitch classes after applying (includes \\ excludes)
parseWithRemoval :: (Text -> [PitchClass]) -> Text -> [PitchClass]
parseWithRemoval tokenParser input
  | isWildcard input = chromaticSet
  | otherwise = 
      let (includes, excludes) = partitionTokens input
          includePcs = unique $ concatMap tokenParser includes
          excludePcs = unique $ concatMap tokenParser excludes
      in includePcs List.\\\\ excludePcs
```

**Decision**: Include this helper but DON'T use it in the three main functions yet. Keep explicit implementations for clarity and to handle special cases (single-token key signatures).

---

## 4. EDGE CASE HANDLING

### 4.1 Wildcard Interactions

| Input | Interpretation | Result |
|-------|----------------|--------|
| `"*"` | Wildcard (shortcut) | `[0..11]` |
| `"* -C"` | All except C | Parse tokens: `[0..11] \\ [0] = [1..11]` |
| `"* -C -G"` | All except C and G | `[0..11] \\ [0,7] = [1..6,8..11]` |
| `"-*"` | Remove all | `[] \\ [0..11] = []` |
| `"C -*"` | C overtones minus all | `[0,4,7,10] \\ [0..11] = []` |

**Implementation**: Wildcard check MUST happen before token parsing. Otherwise, treat `*` as a regular token in the includes/excludes lists.

```haskell
parseOvertones' :: Int -> Text -> [PitchClass]
parseOvertones' n input
  | isWildcard input = chromaticSet  -- SHORTCUT: return immediately
  | otherwise = ...  -- Token-based parsing
```

### 4.2 Invalid Tokens

| Input | Behavior | Result |
|-------|----------|--------|
| `"C -invalid"` | Invalid exclude ignored | `[0,4,7,10]` |
| `"invalid -C"` | Invalid include ignored | `[] \\ [0] = []` |
| `"-invalid"` | Invalid exclude ignored | `[]` |

**Implementation**: Existing token parsers return `[]` for invalid input. This naturally handles invalid tokens.

### 4.3 All-Negative Inputs

| Input | Behavior | Result |
|-------|----------|--------|
| `"-C -D -E"` | No includes | `[] \\ [0,2,4] = []` |
| `"-1b"` | No includes | `[] \\ [F major] = []` |
| `"-*"` | Remove all | `[] \\ [0..11] = []` |

**Semantic**: Empty includes list → empty result. To get "all except X", use `"* -X"`.

---

## 5. TESTING STRATEGY

### 5.1 Test File Structure

Add to `/Users/oscarsouth/.stack/global-project/test/Harmonic/Core/FilterSpec.hs`:

```haskell
describe "Pitch removal with '-' operator" $ do
  
  describe "parseOvertones' removal" $ do
    it "removes single pitch: C E -E' → C overtones only" $ do
      let result = sort $ parseOvertones' 4 "C E -E'"
      -- C overtones: [0,4,7,10], E overtones: [4,8,11,2]
      -- Union: [0,2,4,7,8,10,11]
      -- Minus E': [0,2,4,7,8,10,11] \\ [4] = [0,2,7,8,10,11]
      result `shouldBe` [0,2,7,8,10,11]
    
    it "removes overtones: C -G → C overtones minus G overtones" $ do
      let result = sort $ parseOvertones' 4 "C -G"
      -- C overtones: [0,4,7,10]
      -- G overtones: [7,11,2,6]
      -- [0,4,7,10] \\ [2,6,7,11] = [0,4,10]
      result `shouldBe` [0,4,10]
    
    it "wildcard minus pitch: * -C' → all except C" $ do
      let result = sort $ parseOvertones' 4 "* -C'"
      result `shouldBe` [1..11]
    
    it "wildcard minus overtones: * -G → all except G overtones" $ do
      let result = sort $ parseOvertones' 4 "* -G"
      -- Remove [7,11,2,6] from [0..11]
      result `shouldBe` [0,1,3,4,5,8,9,10]
  
  describe "parseKey' removal" $ do
    it "removes pitch from key: 1b -G' → F major minus G" $ do
      let result = sort $ parseKey' 4 "1b -G'"
      -- F major: [0,2,4,5,7,9,10]
      -- Minus G (7): [0,2,4,5,9,10]
      result `shouldBe` [0,2,4,5,9,10]
    
    it "removes key from union: 1b 2# -G → (F major ∪ D major) \\ G major" $ do
      let result = sort $ parseKey' 4 "1b 2# -G"
      -- F major (1b): [0,2,4,5,7,9,10]
      -- D major (2#): [1,2,4,6,7,9,11]
      -- Union: [0,1,2,4,5,6,7,9,10,11]
      -- G major: [0,2,4,6,7,9,11]
      -- Minus G major: [1,5,10]
      result `shouldBe` [1,5,10]
    
    it "all except C and F#: * -C' -F#' → chromatic minus two pitches" $ do
      let result = sort $ parseKey' 4 "* -C' -F#'"
      result `shouldBe` [1,2,3,4,5,7,8,9,10,11]
  
  describe "parseFunds' removal" $ do
    it "removes single root: C G -G → just C" $ do
      let result = sort $ parseFunds' 4 "C G -G"
      result `shouldBe` [0]
    
    it "key minus pitch: 1b -F → F major scale minus F" $ do
      let result = sort $ parseFunds' 4 "1b -F"
      -- F major: [0,2,4,5,7,9,10]
      -- Minus F (5): [0,2,4,7,9,10]
      result `shouldBe` [0,2,4,7,9,10]
    
    it "all except E A D G: * -E -A -D -G → chromatic minus bass notes" $ do
      let result = sort $ parseFunds' 4 "* -E -A -D -G"
      -- Remove [4,9,2,7] from [0..11]
      result `shouldBe` [0,1,3,5,6,8,10,11]
  
  describe "Edge cases" $ do
    it "remove wildcard: -* → empty" $ do
      parseOvertones' 4 "-*" `shouldBe` []
    
    it "all negative: -C -D -E → empty" $ do
      parseOvertones' 4 "-C -D -E" `shouldBe` []
    
    it "include then exclude same: C -C → empty" $ do
      parseOvertones' 4 "C -C" `shouldBe` []
    
    it "invalid exclude ignored: C -invalid" $ do
      let result = sort $ parseOvertones' 4 "C -invalid"
      -- C overtones: [0,4,7,10], invalid → []
      result `shouldBe` [0,4,7,10]
    
    it "invalid include: invalid -C → empty" $ do
      parseOvertones' 4 "invalid -C" `shouldBe` []
```

### 5.2 Integration Tests

Add to test suite:

```haskell
describe "Integration with harmonicContext" $ do
  it "generates with pitch removal filters" $ do
    ctx <- harmonicContext "* -C'" "1b" "*"
    let start = initCadenceState 0 "F" [0,4,7] FlatSpelling
    prog <- genSilent start 4 "*" 0.5 ctx
    -- Verify progression generated successfully
    length prog `shouldBe` 4
```

### 5.3 REPL Verification Examples

```haskell
-- Test overtones removal
import Harmonic.Core.Filter
parseOvertones "C E -E'"          -- [0,2,7,8,10,11]
parseOvertones "* -G"             -- [0,1,3,4,5,8,9,10]
parseOvertones "E A D G -G'"      -- Bass tuning minus G pitch

-- Test key removal
parseKey "1b -G'"                 -- F major minus G
parseKey "1b 2# -G"               -- (F ∪ D) \ G major
parseKey "* -C' -F#'"             -- All except C and F#

-- Test funds removal
parseFunds "C G -G"               -- Just C
parseFunds "* -E -A -D -G"        -- All except bass notes
parseFunds "1b -F"                -- F major scale minus F

-- Test with generation
ctx <- harmonicContext "* -C'" "1b" "*"
let start = initCadenceState 0 "F" [0,4,7] FlatSpelling
prog <- genSilent start 4 "*" 0.5 ctx
lookupChord prog 0
```

---

## 6. DOCUMENTATION

### 6.1 Module-Level Documentation

Update `/Users/oscarsouth/.stack/global-project/src/Harmonic/Core/Filter.hs` header (lines 11-33):

```haskell
-- == Filter Notation (from original README)
--
-- === Overtones/Pitch Set Filter
-- Limits harmonic choices to pitches within a specified set.
--
-- * Fundamental pitches (derives overtones): @"E A D G"@ (bass tuning)
-- * Individual pitches with prime: @"E'"@ @"A'"@ @"A#'"@
-- * Combined: @"G E' A' A#'"@ (G overtones + E, A, A# pitches)
-- * Wildcard: @"*"@ (all pitches)
-- * __Removal with @-@ prefix__: @"-G"@ (remove G overtones), @"-E'"@ (remove E pitch)
-- * __Combined__: @"C E -E'"@ → (C overtones ∪ E overtones) \ {E pitch}
--
-- === Key Filter  
-- Removes pitches not in the specified key.
--
-- * Key signature: @"bb"@, @"###"@, @"4b"@, @"0#"@
-- * Named key: @"C"@, @"F#m"@, @"Bb"@
-- * Individual pitch with prime: @"G'"@ (single pitch, not scale)
-- * Wildcard: @"*"@ (no key filtering)
-- * __Removal__: @"-G"@ (remove G major scale), @"-G'"@ (remove G pitch)
-- * __Example__: @"1b 2# -G'"@ → (F major ∪ D major) \ {G pitch}
--
-- === Root Notes Filter
-- Limits bass notes to specified pitch classes or key.
--
-- * Pitches: @"E F# G"@
-- * Key signature: @"1b"@, @"#"@
-- * Wildcard: @"*"@ (all roots)
-- * __Removal__: @"-G"@ (remove G pitch), @"-1b"@ (remove F major scale)
-- * __Example__: @"* -E -A -D -G"@ → All roots except bass tuning notes
--
-- === Removal Operator Semantics
--
-- The @-@ prefix inverts any valid token:
--
-- * __Order of operations__: @(union all positive tokens) \\\\ (union all negative tokens)@
-- * __Context-specific behavior__:
--   - Overtones: @"G"@ = G overtones, @"G'"@ = G pitch, @"-G"@ = remove G overtones, @"-G'"@ = remove G pitch
--   - Key: @"G"@ = G major scale, @"G'"@ = G pitch, @"-G"@ = remove G major scale, @"-G'"@ = remove G pitch
--   - Roots: Same as key (fundamentals only)
-- * __Edge cases__:
--   - @"* -C'"@ → All except C pitch
--   - @"-*"@ → Empty set (remove all)
--   - @"-C -D -E"@ → Empty set (no includes)
```

### 6.2 USER_GUIDE.tidal Updates

Add new section to `/Users/oscarsouth/.stack/global-project/live/USER_GUIDE.tidal` after Section 2.2:

```haskell
-- ───────────────────────────────────────────────────────────────────────────
-- 2.3 Pitch Removal with '-' Operator
-- ───────────────────────────────────────────────────────────────────────────
-- The '-' prefix removes pitches/sets from the filter result
-- Syntax: (union all positive tokens) \ (union all negative tokens)

-- OVERTONES PARAMETER: Removes overtones or individual pitches
let ctx1 = hContext "C E -E'" "*" "*"  -- (C overtones ∪ E overtones) \ {E pitch}
let ctx2 = hContext "* -G" "*" "*"     -- All pitches except G overtones [7,11,2,6]
let ctx3 = hContext "* -G'" "*" "*"    -- All pitches except G pitch [7]

-- KEY PARAMETER: Removes scales or individual pitches
let ctx4 = hContext "*" "1b -G'" "*"          -- F major scale except G pitch
let ctx5 = hContext "*" "1b 2# -G" "*"        -- (F major ∪ D major) \ G major scale
let ctx6 = hContext "*" "* -C' -F#'" "*"      -- All pitches except C and F#

-- ROOTS PARAMETER: Removes root notes
let ctx7 = hContext "*" "*" "C G -G"          -- Just C as root
let ctx8 = hContext "*" "*" "* -E -A -D -G"   -- All roots except bass notes

-- CONTEXT-SPECIFIC BEHAVIOR
-- In overtones: "G" = G overtones [7,11,2,6], "G'" = G pitch [7]
--               "-G" = remove overtones, "-G'" = remove pitch
--
-- In key:       "G" = G major scale, "G'" = G pitch [7]
--               "-G" = remove scale, "-G'" = remove pitch
--
-- In roots:     "G" = G pitch [7] (no overtones in roots)
--               "-G" = remove G pitch

-- EDGE CASES
let empty1 = hContext "-*" "*" "*"        -- Remove all → empty overtones
let empty2 = hContext "-C -D -E" "*" "*"  -- No includes → empty
let allButC = hContext "* -C'" "*" "*"    -- Wildcard minus C → [1..11]

-- PRACTICAL EXAMPLES
-- Bass tuning except G string
let bassNoG = hContext "E A D G -G'" "*" "*"

-- Diatonic except tonic and dominant
let noCandG = hContext "*" "C" "* -C -G"

-- F major / D major hybrid minus G pitch (common tone)
let fDnoG = hContext "*" "1b 2# -G'" "*"

-- Generate with removal filters
ctx <- harmonicContext "* -C'" "1b" "*"
let start = initCadenceState 0 "F" [0,4,7] FlatSpelling
prog <- gen start 8 "*" 0.5 ctx
d1 $ note (harmony prog "<0 1 2 3 4 5 6 7>") # s "superpiano"
```

### 6.3 Function-Level Documentation

Add Haddock comments to updated functions:

```haskell
-- |Parse an overtones filter with full notation support, including removal.
--
-- Supports:
--   * Note names: @"E A D G"@ (overtones)
--   * Prime notation: @"E'"@ (single pitch)
--   * Key signatures: @"1b"@, @"##"@
--   * Removal: @"-G"@ (remove overtones), @"-E'"@ (remove pitch)
--   * Wildcard: @"*"@ (all pitches)
--
-- Order of operations: @(union includes) \\\\ (union excludes)@
--
-- Examples:
--   * @"C E -E'"@ → C overtones ∪ E overtones, then remove E pitch
--   * @"* -G"@ → All pitches except G overtones
--   * @"-C -D"@ → Empty set (no includes)
parseOvertones' :: Int -> Text -> [PitchClass]
```

---

## 7. IMPLEMENTATION SEQUENCE (VERTICAL SLICES)

### Slice 1: Add partitionTokens helper
**File**: `/Users/oscarsouth/.stack/global-project/src/Harmonic/Core/Filter.hs`
**Lines**: 73-82 (after type definitions)
**Changes**:
- Add `import Data.List (partition)` to imports
- Add `partitionTokens :: Text -> ([Text], [Text])` function
- Add unit tests for partitionTokens

**Verification**:
```bash
stack test --test-arguments="--match partitionTokens"
```

**REPL**:
```haskell
import Harmonic.Core.Filter
partitionTokens "C G -E -F#"  -- (["c","g"], ["e","f#"])
partitionTokens "-*"           -- ([], ["*"])
partitionTokens "* -C"         -- (["*"], ["c"])
```

### Slice 2: Update parseOvertones' with removal support
**File**: `/Users/oscarsouth/.stack/global-project/src/Harmonic/Core/Filter.hs`
**Lines**: 290-314
**Changes**:
- Modify `parseOvertones'` to use `partitionTokens` and set difference
- Keep `parseGeneralToken` unchanged (already handles G vs G')

**Verification**:
```bash
stack test --test-arguments="--match parseOvertones.*removal"
```

**REPL**:
```haskell
import Harmonic.Core.Filter
parseOvertones "C E -E'"       -- C overtones ∪ E overtones \ {E pitch}
parseOvertones "* -G"          -- All except G overtones
```

### Slice 3: Add parseKeyToken helper
**File**: `/Users/oscarsouth/.stack/global-project/src/Harmonic/Core/Filter.hs`
**Lines**: After `keyToPitchClasses` (~185)
**Changes**:
- Add `parseKeyToken :: Text -> [PitchClass]` helper
- Handles prime notation, key signatures, and note names

**Verification**:
```bash
stack test --test-arguments="--match parseKeyToken"
```

**REPL**:
```haskell
import Harmonic.Core.Filter
-- (test parseKeyToken helper)
```

### Slice 4: Update parseKey' with removal support
**File**: `/Users/oscarsouth/.stack/global-project/src/Harmonic/Core/Filter.hs`
**Lines**: 171-174
**Changes**:
- Modify `parseKey'` to use `partitionTokens` and `parseKeyToken`
- Handle single-token backward compatibility

**Verification**:
```bash
stack test --test-arguments="--match parseKey.*removal"
```

**REPL**:
```haskell
import Harmonic.Core.Filter
parseKey "1b -G'"              -- F major minus G
parseKey "1b 2# -G"            -- (F ∪ D) \ G major
parseKey "* -C' -F#'"          -- All except C and F#
```

### Slice 5: Add parseFundsToken helper
**File**: `/Users/oscarsouth/.stack/global-project/src/Harmonic/Core/Filter.hs`
**Lines**: After `parseFunds'` (~256)
**Changes**:
- Add `parseFundsToken :: Text -> [PitchClass]` helper
- Handles key signatures and note names (no prime notation)

**Verification**:
```bash
stack test --test-arguments="--match parseFundsToken"
```

### Slice 6: Update parseFunds' with removal support
**File**: `/Users/oscarsouth/.stack/global-project/src/Harmonic/Core/Filter.hs`
**Lines**: 246-255
**Changes**:
- Modify `parseFunds'` to use `partitionTokens` and `parseFundsToken`
- Handle single-token backward compatibility

**Verification**:
```bash
stack test --test-arguments="--match parseFunds.*removal"
```

**REPL**:
```haskell
import Harmonic.Core.Filter
parseFunds "C G -G"            -- Just C
parseFunds "* -E -A -D -G"     -- All except bass notes
parseFunds "1b -F"             -- F major scale minus F
```

### Slice 7: Integration tests
**File**: `/Users/oscarsouth/.stack/global-project/test/Harmonic/Core/FilterSpec.hs`
**Changes**:
- Add integration tests with `harmonicContext`
- Test generation with removal filters

**Verification**:
```bash
stack test --test-arguments="--match Integration"
```

**REPL**:
```haskell
import Harmonic.Lib
ctx <- harmonicContext "* -C'" "1b" "*"
let start = initCadenceState 0 "F" [0,4,7] FlatSpelling
prog <- genSilent start 4 "*" 0.5 ctx
lookupChord prog 0
```

### Slice 8: Documentation updates
**Files**:
- `/Users/oscarsouth/.stack/global-project/src/Harmonic/Core/Filter.hs` (module header)
- `/Users/oscarsouth/.stack/global-project/live/USER_GUIDE.tidal` (new section 2.3)

**Changes**:
- Update module documentation with removal operator semantics
- Add USER_GUIDE section with examples and edge cases
- Add Haddock comments to updated functions

**Verification**: Manual review of documentation clarity

---

## 8. BACKWARD COMPATIBILITY

### 8.1 Existing Behavior Preserved

| Input | Old Behavior | New Behavior | Compatible? |
|-------|--------------|--------------|-------------|
| `"*"` | Chromatic set | Chromatic set | ✅ Yes |
| `"C G"` | Union overtones | Union overtones | ✅ Yes |
| `"1b 2#"` | Union keys | Union keys | ✅ Yes |
| `"E F# G"` | Union pitches | Union pitches | ✅ Yes |
| `"C'"` | Single pitch C | Single pitch C | ✅ Yes |
| `"G E' A'"` | G overtones + pitches | G overtones + pitches | ✅ Yes |

### 8.2 New Behavior

| Input | Behavior | Result |
|-------|----------|--------|
| `"-G"` | Remove G overtones | Context-specific removal |
| `"-G'"` | Remove G pitch | Context-specific removal |
| `"* -C"` | All except C | `[1..11]` |
| `"C -C"` | Include then exclude | `[]` |

**Conclusion**: Fully backward compatible. No existing input strings use `-` prefix.

---

## 9. PERFORMANCE CONSIDERATIONS

### 9.1 Algorithm Complexity

- **partitionTokens**: O(n) where n = number of tokens
- **Set difference**: O(n log n) due to `unique` sort
- **Overall**: O(n log n), same as existing union-only logic

### 9.2 Memory Usage

- Two intermediate lists (includes, excludes) instead of one
- Negligible impact: filter strings typically have < 10 tokens

### 9.3 Optimization Opportunities

None needed. Filter parsing is not performance-critical (happens once per `harmonicContext` call).

---

## 10. CRITICAL FILES FOR IMPLEMENTATION

### Primary Implementation Files

1. **`/Users/oscarsouth/.stack/global-project/src/Harmonic/Core/Filter.hs`**
   - Lines 73-82: Add `partitionTokens` helper
   - Lines 171-174: Update `parseKey'` with removal logic
   - Lines 246-255: Update `parseFunds'` with removal logic
   - Lines 290-314: Update `parseOvertones'` with removal logic
   - Add `parseKeyToken` and `parseFundsToken` helpers
   - Update module-level documentation (lines 11-33)

2. **`/Users/oscarsouth/.stack/global-project/test/Harmonic/Core/FilterSpec.hs`**
   - Add comprehensive test suite for removal operator
   - Add integration tests with `harmonicContext`
   - Test all edge cases and context-specific behaviors

3. **`/Users/oscarsouth/.stack/global-project/live/USER_GUIDE.tidal`**
   - Add Section 2.3: Pitch Removal with '-' Operator
   - Document context-specific behavior with examples
   - Include edge cases and practical use cases

### Reference Files

4. **`/Users/oscarsouth/.stack/global-project/theHarmonicAlgorithmLegacy/src/Overtone.hs`**
   - Reference for original parsing logic and behavior
   - Verify no legacy removal operator existed

5. **`/Users/oscarsouth/.stack/global-project/src/Harmonic/Core/Builder.hs`**
   - Lines 159, 1525, 1528, 1650-1651: Usage of parse functions
   - Verify integration points work with new removal logic

---

## SUMMARY

This implementation plan adds pitch removal support to all three filter parameters (overtones, key, roots) using a unified `-` prefix operator. The design:

1. **Reuses existing parsing logic** - No changes to token interpretation, only addition of partitioning step
2. **Maintains backward compatibility** - No existing inputs use `-` prefix
3. **Provides context-specific semantics** - `G` vs `G'` behavior preserved in each parameter
4. **Handles edge cases gracefully** - Wildcards, empty includes, invalid tokens
5. **Follows vertical slice methodology** - 8 independently verifiable implementation steps
6. **Includes comprehensive testing** - Unit tests, integration tests, REPL verification
7. **Provides clear documentation** - Module headers, USER_GUIDE, function-level Haddock

The implementation can be completed in 8 atomic slices, each independently verifiable via `stack test` and REPL verification.
