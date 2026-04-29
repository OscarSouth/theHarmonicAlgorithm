# Octatripentatonic Framework

The octatripentatonic framework is a *semidiatonic* construct built from
eleven canonical pentatonic **strata** and twelve curated **tristrata** —
groups of three strata whose pair-unions are all 7-note diatonic subsets and
whose three-way union is exactly the eight-tone set `[1,2,4,6,7,9,10,11]`
(omitting `[0,3,5,8]`, E-minor base).

This document covers both the theory and the codebase surface introduced by
the `genP` paradigm in V3.0.0.

## Theory

### Eight-tone union, four-tone omission

Every curated tristrata spans the same eight pitch classes:

```
union     = [1, 2, 4, 6, 7, 9, 10, 11]
omission  = [0, 3, 5, 8]
```

E-minor-base chroma applies throughout — the numbers above are PC indices
relative to E as 0.

### Pentatonic families

Four ancestral pentatonic shapes recognised throughout the legacy theory,
exposed as `Harmonic.Rules.Types.Scale.PentaFamily`:

| Family       | Chroma (rooted at 0)     |
|--------------|--------------------------|
| `MajorPenta` | `[0, 2, 4, 7, 9]`        |
| `Okinawan`   | `[0, 4, 5, 7, 11]`       |
| `Iwato`      | `[0, 1, 5, 6, 10]`       |
| `Kumoi`      | `[0, 2, 3, 7, 9]`        |

### Eleven canonical strata (Roman I–XI)

| Label | Prime-form chroma   | Dissonance |
|-------|---------------------|-----------:|
| I     | `[2, 4, 6, 7, 11]`  | 47         |
| II    | `[1, 2, 4, 6, 9]`   | 46         |
| III   | `[1, 2, 4, 6, 11]`  | 53         |
| IV    | `[2, 4, 6, 7, 9]`   | 52         |
| V     | `[1, 2, 4, 7, 9]`   | 68         |
| VI    | `[1, 2, 4, 7, 11]`  | 72         |
| VII   | `[2, 4, 6, 7, 10]`  | 71         |
| VIII  | `[1, 2, 6, 7, 9]`   | 74         |
| IX    | `[1, 2, 6, 7, 11]`  | 75         |
| X     | `[1, 2, 6, 7, 10]`  | 72         |
| XI    | `[1, 4, 6, 7, 10]`  | 91         |

### Twelve curated tristrata (Records 1–12)

| #  | Roman triad   | Dissonance sum |
|----|---------------|---------------:|
| 1  | I-V-X         | 187            |
| 2  | II-VI-X       | 190            |
| 3  | III-V-VII     | 192            |
| 4  | III-V-X       | 193            |
| 5  | IV-VI-X       | 196            |
| 6  | I-V-XI        | 206            |
| 7  | II-VI-XI      | 209            |
| 8  | III-V-XI      | 212            |
| 9  | V-VII-IX      | 214            |
| 10 | IV-VI-XI      | 215            |
| 11 | V-IX-XI       | 234            |
| 12 | VI-VIII-XI    | 237            |

Per-tristrata details (including the three pair-union modes) are mirrored as
plain-text fixtures under `data/octatripentatonic/tristrata/*.txt`.

### Tristrata-containment index

Each strata label appears in several tristratas at a specific position (1,
2, or 3). This index drives the adjacency set used by the `genP` traversal.

| Strata | Tristratas (`index:position`) |
|--------|-------------------------------|
| I      | 1:1, 6:1                      |
| II     | 2:1, 7:1                      |
| III    | 3:1, 4:1, 8:1                 |
| IV     | 5:1, 10:1                     |
| V      | 1:2, 3:2, 4:2, 6:2, 8:2, 9:1, 11:1 |
| VI     | 2:2, 5:2, 7:2, 10:2, 12:1     |
| VII    | 3:3, 9:2                      |
| VIII   | 12:2                          |
| IX     | 9:3, 11:2                     |
| X      | 1:3, 2:3, 4:3, 5:3            |
| XI     | 6:3, 7:3, 8:3, 10:3, 11:3, 12:3 |

### The "semidiatonic" framing

Any two strata of a tristrata union to a 7-note diatonic set classified via
the 28-mode taxonomy (`Harmonic.Rules.Types.Scale.ModeQuality`). The third
strata adds a single extra PC, taking the union from diatonic (7) to
octatripentatonic (8).

## Applied — code surface

### Types (`Harmonic.Rules.Types.Scale`)

```haskell
data PentaFamily = MajorPenta | Okinawan | Iwato | Kumoi
data Pentatonic  = Pentatonic { pentaFamily :: PentaFamily, pentaRoot :: PitchClass }

data ModeQuality = Ionian | Dorian | ... | LocBb7   -- 28 constructors
data Mode        = Mode { modeQuality :: ModeQuality, modeRoot :: PitchClass }

data StrataLabel = I | II | ... | XI                -- 11 constructors
data Tristrata   = Tristrata { ts1, ts2, ts3 :: StrataLabel }

strataChroma      :: StrataLabel -> [PitchClass]
strataDissonance  :: StrataLabel -> Int
validTristrata    :: [Tristrata]                    -- 12 canonical
tristrataIndex    :: Int -> Tristrata               -- 1..12
tristrataOf       :: StrataLabel -> [(Tristrata, Int)]
tristrataModes    :: Tristrata -> (Maybe Mode, Maybe Mode, Maybe Mode)
parseTristrataList, parseRelStrata, parseAbsStrata  -- string surface
```

### `ProgressionContext` (`Harmonic.Rules.Types.ProgressionContext`)

Three bar-aligned layers plus per-bar provenance:

```haskell
data Layer = T | S | M        -- Triad, Strata, Mode (M, not D — avoids NoteName.D clash)

data ProgressionContext = ProgressionContext
  { triadLayer   :: Progression
  , strataLayer  :: Progression
  , modeLayer    :: Progression
  , pcProvenance :: Maybe (Seq (Tristrata, StrataLabel))
  }
```

### Strata-first traversal (`Harmonic.Framework.Builder.Strata`)

Pure helpers that decide which `(strata, tristrata)` pair a bar is drawn
from given the previous bar's assignment and the `hcTristrata` allow-list:

| Helper | Role |
|--------|------|
| `adjacentInTristrata` | In-tristrata circular neighbourhood `{p-1, p, p+1}` |
| `allowedNext`         | Candidate pool for the next bar |
| `initialPlacement`    | Bar-0 seed: starting strata + lowest-dissonance tristrata |
| `modeFor`             | Bar's diatonic mode (pair-union classification) |
| `selectNext`          | Tie-break: strata-continuity → tristrata-continuity → dissonance |

### `genP` paradigm (`Harmonic.Framework.Builder`) — filter over R→E→T

`genP` is **a per-bar filter layer over the existing R→E→T pipeline**, not
a parallel generator. Each bar:

1. The strata walk picks `(s_i, t_i)` via `Strata.initialPlacement` +
   `Strata.allowedNext`/`Strata.selectNext`, optionally narrowed by
   `relStrata` / `absStrata`.
2. The `HarmonicContext`'s `_hcOvertones` is narrowed to the 5-PC
   `strataChroma s_i` (prime-notation string). `parseContextOnce` rebuilds
   `ParsedContext`; every other R rule (key, roots, pedal, drift,
   inversion-spacing) continues to apply on top.
3. The soft-boost `_gcBoostSame * _gcBoostFlip * _gcBoostTri` is attached
   to `pcSoftBoost` based on continuity against bars `i-1` and `i-2`.
4. `stepChainBody` runs the normal R→E→T step: graph transitions from
   Neo4j (when `seek "bach:…"`) + `consonanceFallbackParsed` for missing
   coverage. `badness` is multiplied by the soft-boost at
   `computeFallbackScoreWithBoost`; graph confidence is divided by the
   same boost (inverted sense). Gamma selection is unchanged.
5. Single-strata containment is guaranteed by construction — every triad
   produced belongs to the narrowed overtone set.

`genP` is seeded by a `StrataLabel`, not a `Tristrata` — the active
tristrata is free to move between any of the twelve corpus tristratas
that contain the current strata.

```haskell
genP     :: StrataLabel -> GenConfig
genP'    :: StrataLabel -> GenConfig    -- standard verbosity
genP''   :: StrataLabel -> GenConfig    -- verbose verbosity

-- 33 ergonomic aliases: {genI..genXI} × {primary, ', ''}
genVI    :: GenConfig                   -- = genP VI
```

Modifier chain:

```haskell
hcTristrata :: String -> HarmonicContext -> HarmonicContext
-- ""       = all 12 tristratas allowed (default)
-- "5"      = lock to tristrata #5 (IV-VI-X)
-- "1 2 5"  = whitelist
-- "[1,2,5]" = bracket form

relStrata :: String -> GenConfig -> GenConfig
-- Per-bar position (1..3) within the dynamically-changing active tristrata.
-- Sets _gcLenOverride to the list length.

absStrata :: String -> GenConfig -> GenConfig
-- Per-bar absolute strata label. Sets _gcLenOverride to the list length.

sameBoost, flipBoost, triBoost :: Double -> GenConfig -> GenConfig
-- Override the soft-boost multipliers (defaults 0.90 / 0.80 / 0.70).
-- Pass 1.0 to disable the bias for that dimension.
```

Live-coding recipes (offline examples — use `"none"` to skip Neo4j):

```haskell
-- Single-strata entrypoint: 6 bars seeded from VI.
pc <- seek "none" $ cue start $ len 6 $ genVI

-- Lock to a tristrata.
let ctx = hcTristrata "5" hContext
pc <- seek "none" $ cue start $ tonal ctx $ len 6 $ genVI

-- Per-bar relative strata (auto-sets length to 6).
pc <- seek "none" $ cue start $ relStrata "1 1 2 2 3 3" $ genVI

-- Per-bar absolute strata (auto-sets length to 3).
pc <- seek "none" $ cue start $ absStrata "I V X" $ genI
```

### Live-coding helpers (`Harmonic.Interface.Tidal.OctatripentatonicT`)

```haskell
renderTristrataReport :: ProgressionContext -> Maybe String
genPReport            :: IO ProgressionContext -> IO ()
```

`genPReport` executes a `genP`-style action and prints both the per-bar
`(tristrata, strata)` provenance and the triad-layer `Show` output.

### Progression-transformation rules

| Class | Combinators | Behaviour |
|-------|-------------|-----------|
| 1 (pointwise)   | `rotate`, `excerpt`, `insert`, `clone`, `extract`, `switch`, `transposeP`, `expandP`, `fromChords`, `fuse`/`fuse2`, `interleave` | Apply to all layers via `liftPC`; provenance dropped (see `ProgressionContext.hs` for Class-1-specific permutation upgrades) |
| 2 (layer-tagged) | `grid`, `flow`, `lite`, `literal`, `root`, `fund`, `subKick` | Take a `Layer` argument; voicing extracts from `layer l pc` |
| 3 (anchored)     | `progOverlap*`, `spliceProgression`, `fixMovementAt`, `lineHarmony` | Triad-anchored; reclassify strata/mode from `pcProvenance`, fall back to pointwise lift when the invariant breaks |

## Cross-references

* `CLAUDE.md` — project conventions and verification workflow.
* `ARCHITECTURE.md` — four-layer R→E→T layering.
* `data/octatripentatonic/tristrata/*.txt` — per-tristrata fixture table.
* `test/Harmonic/Framework/BuilderPSpec.hs` — invariant tests.
* `test/Harmonic/Rules/Types/ScaleSpec.hs` — vocabulary tests.

## Status (V3.0.0)

Landed:

* Vocabulary (`Scale.hs`, `ProgressionContext.hs`).
* `ProgressionContext` widening of the `gen` family.
* `genP` as a **filter over R→E→T** — per-bar `_hcOvertones` narrowing
  to the strata's chroma, soft-boost multipliers wired through
  `computeFallbackScoreWithBoost` and graph-confidence inverse.
* `hcTristrata` + `relStrata` / `absStrata` / `sameBoost` / `flipBoost` /
  `triBoost` modifiers.
* 33 live-coding aliases (`genI..genXI` × `{primary, ', ''}`).
* `Interface/Tidal/OctatripentatonicT` helpers.
* Golden fixtures under `data/octatripentatonic/tristrata/`.
* Live-script migration (467 pitched-instrument calls in 58 `.tidal`
  files prefixed with `T`).
* Per-bar diagnostics for `genP'` / `genP''`. Standard verbosity now
  prints a per-bar block showing the triad selection + active strata
  (5 PCs) + classified mode (7 PCs) + tristrata identity; verbose
  verbosity adds the soft-boost product applied that bar.

### Per-bar diagnostic shape (genP')

Each bar is a three-line block sharing four aligned columns (label,
identifier, info, chroma). The tristrata identity is merged into the
strata line's info column as `#N (ts1-ts2-ts3)`.

Chord names use slash notation (`A maj/F#` for inversions) and exact
legacy `toMode` mode-quality strings (`Ionian`, `Mixo_b6`, `Lyd_Aug_#2`,
etc.). Every bar picks a single 'EnharmonicSpelling' via
`H.inferSpelling` on the mode chroma (triad-root-first) — the **same
tooling** that the chord progression print uses for chord naming — and
threads it through chord name, strata chroma, mode root, parent-key
root, and mode chroma. This keeps the whole bar block in one coherent
accidental system: `F# Phrygian (D Major)` renders as `{F# G A B C# D E}`
rather than `{Gb Ab Bbb Cb Db Ebb Fb}`; the chord itself also spells in
that bar's context (`G min/A#` under B Harmonic Minor rather than
`G min/Bb`).

```
Strata walk: E → 8 bars (entropy 0.5)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    1:         E → A           A 6no3                [fallback] γ=8
    strata     II              #2 (II-VI-X)          {A C# D E F#}
    mode       A Mixolydian    (D Major)             {A B C# D E F# G}

    2:         A → Bb          Bb 6#5no3             [fallback] γ=10
    strata     X               #2 (II-VI-X)          {Bb Db D Gb G}
    mode       Bb Lyd_Aug_#2   (D Harmonic Major)    {Bb Db D E Gb G A}

    3:         Bb → B          B sus2/4no5           [fallback] γ=5
    strata     VI              #2 (II-VI-X)          {B C# D E G}
    mode       B Harm_Min      (B Harmonic Minor)    {B C# D E F# G A#}
  …
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

|= offline =| harmonic traversal (fallback only — no graph) -> …
   entropy 0.5 | overtones * | key * | roots *

   1   ||   A 6no3        |   Bb 6#5no3     |   B sus2/4no5   |   ...
```

The chord-grid footer is unchanged from `gen` — it still spells each
chord via its own `CadenceState.stateSpelling` (which is
`inferSpelling`-derived from just that chord's pitches). The grid and
diagnostic may occasionally disagree on a chord's accidentals when the
chord's own inference differs from the bar's modal inference — both
are musically valid under their respective contexts.

**Mode classification** (triad-anchored, history-aware):

1. Pick the partner strata for bar `i`: the most recent strata in the
   bar history with `s ≠ s_curr`; if none (run started here, or every
   prior bar shared the strata), use the bar-0 positional partner
   inside the active tristrata.
2. Union the current strata's chroma with the partner's chroma → 7-PC
   diatonic set (guaranteed by the framework when both strata sit in
   the same tristrata, which the walk ensures).
3. Classify the union exhaustively against the 28-mode taxonomy
   (`classifyModeAt`), pinning the root to the triad's harmonic root
   PC. Fall back to `Aeolian` rooted on the triad if no quality
   matches.
4. Annotate the parent key by inverting `modeDegree` over the matched
   `ModeQuality` to recover the parent root and family.

When a non-natural walk (e.g. `absStrata "I XI"`, two strata that
aren't members of the same tristrata) produces a union of ≠ 7 PCs,
the diagnostic prints `mode  invalid overlap   {<overlap PCs>}` with
no parent-key tag.

**Strata-containment guarantee.** With the new `pcStrictContainment`
flag, both fallback and graph candidates are filtered against every
absolute PC of the candidate triad — including the bass. An inverted
chord can no longer escape the strata via its bass note. Legacy `gen`
behaviour (where the bass is exempted from the overtone check to
allow `hcRoots "fall<…>"` chromatic passing tones) is unchanged; the
flag only fires inside `runStrataGen`.

Deferred follow-ups:

* **`gen` self-derivation.** Teach the legacy `gen` family to derive
  strata and mode layers from its own triad sequence, replacing the
  duplicated-layer default.
