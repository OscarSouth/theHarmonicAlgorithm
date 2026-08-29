# The Harmonic Algorithm — User Guide

This is the readable companion to [`live/USER_GUIDE.tidal`](live/USER_GUIDE.tidal). If you'd rather work through this interactively — evaluating each block and hearing the results as you go — open the `.tidal` file in your TidalCycles editor and scroll from top to bottom. The two files present the same material in the same order.

Each section carries a **▶ VIDEO** slot — a short demonstration of exactly the blocks in that section, embedded as they're produced.

### MIDI output

This system outputs MIDI on TidalCycles channels 1–16 (`d01`–`d16`). **Everything in this guide transmits on MIDI channel 1** (`d01`), except the groove sections (channel 10, drums). Route channel 1 to a polyphonic sound — piano, Rhodes, pad, polysynth — in your MIDI setup. That's your instrument for the whole walkthrough.

### The graph

Generation draws on a Neo4j graph of harmonic transitions learned from 460+ composers (the Yale Classical Archives Corpus). Start it before you begin:

```bash
docker compose up -d neo4j
```

If you can't run Docker right now, `seek "none"` ([§5](#5-the-composer-graph)) runs every example on the built-in consonance fallback instead — fully musical, just without corpus-trained style.

### Setup

- [Haskell Stack](https://docs.haskellstack.org/en/latest/install_and_upgrade/)
- [Docker](https://www.docker.com/) — the composer graph
- [TidalCycles](https://tidalcycles.org/) with SuperCollider + SuperDirt
- `live/BootTidal.hs` loaded with `Harmonic.Lib`

```bash
stack build                       # compile the library
docker compose up -d neo4j        # graph backend
stack run                         # one-time: populate the graph
```

See [`live/BootTidal.hs`](live/BootTidal.hs) for all available helpers (`ch`, `vel`, `oct`, the `d01`–`d16` MIDI streams, `hush`/`panic`, the drum patterns).

### Known exceptions

Two areas are deliberately out of scope:

- **The Algorithmic Orchestration system** (16-instrument virtual orchestra) depends on a specific hardware/MIDI rig — see [`ALGORITHMIC_ORCHESTRATION.md`](documents/ALGORITHMIC_ORCHESTRATION.md) and [`live/ORCHESTRAL_CATALOGUE.tidal`](live/ORCHESTRAL_CATALOGUE.tidal).
- **Hardware-bound helpers** — the Roland S-1 / P-6 CC maps and the JV-1010 drum map (`Harmonic.Interface.Tidal.Devices.S1`, `.P6`, `.JV1010`), the LED display feed (`Harmonic.Interface.Tidal.Display`), the Q-Link controller bridge, and the MPC kit program behind `subKick`. The rig they address is described in [`LIVE_ENVIRONMENT.md`](documents/LIVE_ENVIRONMENT.md). This guide stays on the single piano channel plus generic drums.

___

## 0. Setup & sound check

**Why** — confirm the three moving parts before anything musical: MIDI routing, the library, and the graph.

**What** —

```haskell
once $ note "0" # ch 01 # legato 0.9   -- one middle C

tempo = 90
start <- lead "C maj"
s <- seek "*" $ cue start $ len 4 $ entropy 0.5 $ gen
print s                                       -- a progression header
```

**How** — the single note confirms MIDI channel 1; the boot banner ("theHarmonicAlgorithm V3 boot complete.") confirms the library; the printed progression confirms the graph. A connection error here means Neo4j isn't running.

> **▶ VIDEO — Setup & sound check**
> _~20s: boot, the C sounds, the first generation prints._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 1. First generation

**Why** — the simplest possible invocation: a 4-chord progression from a starting chord, shaped by the corpus and an entropy dial.

**What** —

```haskell
start <- lead "C maj"

s <- seek "*" $ cue start $ len 4 $ entropy 0.5 $ gen

print s
```

`lead` ([`Arranger.hs`](src/Harmonic/Interface/Tidal/Arranger.hs)) parses a readable string: `"C maj"`, `"E min"`, `"Bb 7 (7)"` — root, quality, optional ascending movement. Use `initCadenceState 0 "C" [0,4,7]` for explicit interval control.

**How** — `lead` returns `IO CadenceState`, so bind with `<-`. The `seek` terminal runs the whole modifier chain; `"*"` aggregates the full corpus. `cue` sets the starting state, `len` the chord count, `entropy` (0.0–1.0+) dials between the familiar and the surprising.

**Try it** — change `"C maj"` to `"E min"` or `"Bb 7 (7)"`. Swap `entropy 0.5` for `0.2` (conventional, cadence-hungry) or `0.9` (adventurous, distant modulations). Regenerate a few times at each — entropy is a distribution, not a script.

> **▶ VIDEO — First generation**
> _~15s: run the four lines, the progression prints; change the seed chord and regenerate._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 2. Diagnostic output (`gen'` and `gen''`)

**Why** — to understand *how* the algorithm arrives at its choices. Swap `gen` for `gen'` and you get a bar-by-bar diagnostic showing the candidate pool, the selected chord, and the top alternatives at each step.

**What** —

```haskell
s <- seek "*" $ cue start $ len 4 $ entropy 0.5 $ gen'
```

Sample output (annotated):

```
Generation: C maj → 4 chords (entropy 0.5)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  1: C maj [starting state]

  2: C → G  [14G/660F]  asc 7 → G maj  [graph] γ=3
     Candidates: G maj | G dom7 | G min | E min | B min | B dom7
  …
```

Field by field:
- **`1, 2, 3`** — bar number
- **`C → G`** — root movement (prior → posterior pitch class)
- **`[NG/MF]`** — candidate pool: N graph candidates, M fallback candidates
- **`asc 7`** — movement class (ascending by 7 semitones)
- **`G maj`** — the chord selected at the posterior root
- **`[graph]` / `[fallback]`** — which pool the selection came from
- **`γ=N`** — the index chosen by gamma-entropy sampling (lower = safer)
- **`Candidates`** — the top alternatives at the same posterior root

`gen''` adds verbose transform and advance traces — pitch-class arithmetic at every step, DB-stored vs computed functionality, the full render pipeline.

**Overtone annotation** — when the context declares a tuning ([§4](#4-shape-the-generation-harmoniccontext)), `gen'` annotates each palette tone with its string/overtone sources: `{B: E2/G3}` means B is producible as string E overtone 2 *or* string G overtone 3. Overtones cover the playable harmonic domain — OT1 root, OT2 perfect fifth, OT3 major third. `"/"` separates alternative sources; `"+"` joins several node positions on one string.

**How** — use `gen` for fast generation, `gen'` to understand selection, `gen''` for debugging. All three have identical signatures and swap freely.

> **▶ VIDEO — Reading the diagnostics**
> _~30s: gen' at entropy 0.2 vs 0.9, arrows on pool counts and gamma indices; a tuning context lighting up the overtone annotations._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 3. Play the progression (`iK`, form, launcher)

**Why** — the shortest bridge from Haskell value to sound.

**What** —

```haskell
s <- seek "*" $ cue start $ len 4 $ entropy 0.5 $ gen

form = [ at 0 1 1 s ]

p01 f k d = d01 $ do
  let o = ch 01
  f
    $ stack [silence
      , arrange (0, 1) k (-9, 9) T flow (overlapF 0) ["~"
        , "[0 1 2 3]/4"
      ] # o |* vel 0.8 |= legato 0.95
    ] |* vel d

do
  let k = iK tempo form (rep s 1)
  mapM_ id [hush, setbpm tempo
    , p01 id k 0.9
    ]
```

Three concepts:
- **`at time kinetics dynamics progression`** ([`Form.hs`](src/Harmonic/Interface/Tidal/Form.hs)) — a form node. With one node the signals are constant. (`at` has siblings — `at'` snaps, `rh`/`rh'` take bars; [§13](#13-kinetics-form-programmed-arc).)
- **`iK tempo formNodes chordSelection`** — bundles everything a launcher needs into one `IK` context.
- **`arrange (lo,hi) k (-9,9) T voicing modifier [patterns]`** ([`Bridge.hs`](src/Harmonic/Interface/Tidal/Bridge.hs)) — maps patterns of voicing *degrees* across the progression. `(0,1)` is the kinetics gate, `(-9,9)` the register, `T` the progression layer (triads; `S`/`M` arrive in [§19](#19-the-three-layers-tsm--genp)).

`rep s 1` auto-derives one-chord-per-bar from the progression length. One cycle is one beat; bar-length patterns carry `/4`.

**Try it** — change `"[0 1 2 3]/4"` to `"[0 1 2 3 4 5 6 7]/8"` (double density) or `"[0 2 1 3]/4"` (new contour).

> **▶ VIDEO — First sound**
> _~30s: define the launcher, launch, hear the cycle; double the pattern density and relaunch._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 4. Shape the generation (`HarmonicContext`)

**Why** — out-of-the-box generation is chromatic and unconstrained. The HarmonicContext modifiers shape what the generator can pick from: key, pitch palette, bass motion, required tones, inversions, tension direction.

**What** —

```haskell
ctx = invSkip 1
    $ consonant
    $ hcPedal "C?"
    $ hcKey "0#"
    $ hContext

s <- seek "*" $ cue start $ tonal ctx $ len 8 $ entropy 0.5 $ gen'
```

All modifiers live in [`Builder/Types.hs`](src/Harmonic/Framework/Builder/Types.hs):

| Modifier | Example | Effect |
|---|---|---|
| `hcOvertones` | `hcOvertones "E A D G"` | Pitch palette — each fundamental expands to its playable overtone set {root, P5, M3} |
| `hcKey` | `hcKey "0#"` | Key signature — `0#`=C, `1#`=G, `2b`=Bb |
| `hcRoots` | `hcRoots "C E G"` | Allowed bass notes |
| `hcPedal` | `hcPedal "C G?"` | Required tones; `?` = preferred |
| `consonant` / `dissonant` | `dissonant $ ...` | Drift direction across the progression |
| `invSkip` | `invSkip 2` | Min non-inversions between inversions |

Filter strings support `"*"` wildcard, `"-Bb'"` subtraction, `"E'"` prime notation (exact pitch, no overtone expansion), `"key"` / `"tones"` mirror keywords, and `"rise"` / `"fall"` for forced stepwise bass motion.

**How** — compose modifiers right-to-left with `$`. Comment out individual lines to fall back to defaults. The whole context passes via `tonal`.

**Try it** — swap `consonant` for `dissonant`. Remove `hcPedal "C?"`. Add `hcOvertones "E A D G"` and watch `gen'` annotate every palette tone with its string/overtone sources — the pool narrows as constraints tighten.

> **▶ VIDEO — Sculpting the space**
> _~45s: layer modifiers one at a time, re-running gen' after each; the candidate pool narrows on screen and the sound tightens audibly._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 5. The composer graph

**Why** — the graph holds harmonic behaviour learned per composer. The `seek` string channels one style, a weighted blend of several, the whole corpus — or none of it.

**What** —

```haskell
s <- seek "bach"               $ cue start $ tonal ctx $ len 8 $ entropy 0.5 $ gen'
s <- seek "debussy"            $ cue start $ tonal ctx $ len 8 $ entropy 0.5 $ gen'
s <- seek "bach:25 debussy:75" $ cue start $ tonal ctx $ len 8 $ entropy 0.5 $ gen'
s <- seek "*"                  $ cue start $ tonal ctx $ len 8 $ entropy 0.5 $ gen'
s <- seek "none"               $ cue start $ tonal ctx $ len 8 $ entropy 0.5 $ gen'
```

Composer weight parsing lives in [`Query.hs`](src/Harmonic/Evaluation/Database/Query.hs). Blends print a portmanteau in the `gen'` header ("Bachdebussy"). Names are case-insensitive.

**`seek "none"` is the offline escape hatch** — it bypasses the graph entirely and generates from the built-in consonance fallback. Every example in this guide runs offline this way; what the corpus adds is style.

**Try it** — compare Bach's cadential pull with Debussy's modal inflections on the same seed and context. Then run the same chain with `"none"` and hear what the corpus was contributing.

> **▶ VIDEO — Composer dialects**
> _~45s: bach, then debussy, then a 30/70 blend on the same seed — the harmonic character shifts each time; the portmanteau in the header._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 6. Rank and select (`attempt`, `viability`)

**Why** — generate several candidate progressions and keep the best, instead of accepting the first.

**What** —

```haskell
s <- seek "*" $ attempt 3 12 $ cue start $ tonal ctx $ len 8 $ entropy 0.6 $ gen

-- with the scoreboard:
s <- seek "*" $ attempt 3 12 $ cue start $ tonal ctx $ len 8 $ entropy 0.6 $ gen''

-- stricter / structural-only:
s <- seek "*" $ viability 0.7 $ attempt 5 24 $ cue start $ len 8 $ entropy 0.6 $ gen
s <- seek "*" $ viability 0.0 $ attempt 3 12 $ cue start $ len 8 $ entropy 0.6 $ gen
```

`attempt N K` produces up to `K` attempts, stops early once `N` *viable* ones are collected, and returns the single highest-scoring progression. Scoring ([`Scoring/Progression.hs`](src/Harmonic/Evaluation/Scoring/Progression.hs)) is dominated by cadence-favourability, plus resolution, voice-leading and motion terms. `viability T` sets the floor an attempt must clear (default 0.5).

With `gen''`, a full **scoreboard** prints — one row per attempt with per-term scores, a viability marker, the truncated chord sequence, and `← PICK` on the winner. It's a lesson in what "better" means to the evaluator.

`attempt` wraps any generation chain — `gen`, `genE`, `genJ`, `genP`, `genFrom` — uniformly. `attempt 1 1` is a no-op.

**Try it** — raise entropy to 0.9 and watch the scoreboard: more attempts fail viability and the spread widens.

> **▶ VIDEO — The scoreboard**
> _~40s: attempt 3 12 with gen'' — the scoreboard prints, the PICK marker lands; entropy raised, the viability failures multiply._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 7. Voicing strategies

**Why** — the same progression can sit vertically in radically different ways. The voicing function distributes each chord's intervals across register without changing the harmony.

**What** — [`Arranger.hs`](src/Harmonic/Interface/Tidal/Arranger.hs)

| Function | Bass | Voice leading | Best for |
|---|---|---|---|
| `flow` | Any inversion | Smoothest (cyclic DP, globally optimal) | Pads, harmonic beds |
| `grid` | Root locked | Smooth upper voices | Grounded chords |
| `lite` | Any | None (raw intervals) | Direct control |
| `root` | Root PC only | N/A | Bass lines, mono |
| `fund` | Harmonic fundamental | N/A | Sub bass, kick drums |

```haskell
arrange (0, 1) k (-9, 9) T flow (overlapF 0) ["~", "[0 1 2 3]/4"] # ch 01
arrange (0, 1) k (-9, 9) T grid (overlapF 0) ["~", "[0 1 2 3]/4"] # ch 01
arrange (0, 1) k (-9, 9) T root (overlapF 0) ["~", "[0]/1"]       # ch 01 |- oct 2
```

**How** — the voicing function is the 5th argument of `arrange` (after the layer). Everything else stays identical. Stack multiple `arrange` calls with different voicings in one launcher to build a full texture.

Two behaviors worth knowing: **mixed chord sizes voice-lead for real** — a triad flowing into a 4-note chord (a `lead'` cue, hand-built material) is costed by aligning the larger chord's outer and inner voices onto the smaller, so seams hold common tones instead of jumping register; and **scale-sized bars route themselves** — any bar with 6+ pitch classes gets `strataModeFlow`'s degree semantics (the engine built for the S/M chroma layers) rather than a chord-voice-leading solve that would be both slow and the wrong musical question. Everything harmony-sized (up to 5 voices) gets the full cyclic DP.

**Try it** — compare `flow` and `grid` on a long progression. Add a second `arrange` with `root` and an octave offset for a bass line.

> **▶ VIDEO — Vertical textures**
> _~30s: flow, then grid, then flow+root stacked — the textural difference is immediate._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 8. Chord selection (`rep` and `warp`)

**Why** — you control *which* chord plays and *when* it changes.

**What** — [`Bridge.hs`](src/Harmonic/Interface/Tidal/Bridge.hs)

```haskell
k = iK tempo [at 0 1 1 s] (rep s 1)               -- one chord per bar
k = iK tempo [at 0 1 1 s] (warp "[1 2 3 4]/4")    -- explicit, 1-indexed
k = iK tempo [at 0 1 1 s] (warp "[1 1 2 1]/4")    -- AABA-like cycle
k = iK tempo [at 0 1 1 s] (warp "[1 <2 3> 1 4]/4") -- Tidal operators
k = iK tempo [at 0 1 1 s] (warp "[1(3,8) 2]/4")   -- euclidean rhythm
```

The `/N` divisor maps directly to physical bars. Any TidalCycles operator (choice, probabilities, euclidean rhythms, nesting) applies. Indices wrap modulo the progression length.

**Try it** — `"[1 2 3 4 3 2 1 4]/4"` (8 events, 4 bars), `"[1 . [2 3]]/4"` (nested), `"[1 2 3 4]/8"` (half-speed harmonic rhythm).

> **▶ VIDEO — Harmonic rhythm**
> _~30s: rep, then three warps — the harmonic rhythm reshapes each time._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 9. `arrange` vs `arrange'`

**Why** — two ways a pattern interacts with progression changes: flow across chords at its own speed, or compress into each chord slot.

**What** — [`Bridge.hs`](src/Harmonic/Interface/Tidal/Bridge.hs)

```haskell
-- arrange: pattern flows across the progression
arrange (0, 1) k (-9, 9) T flow (overlapF 1) ["~"
  , "[0 1 2 3 4 5 6 7]/2"
] # ch 01

-- arrange': the same pattern repeats in every chord slot
arrange' (0, 1) k (-9, 9) T flow (overlapF 0) ["~"
  , "[0 1 2 3 4 5 6 7]/2"
] # ch 01
```

`arrange` feels like a melody running over changing chords — each note's pitch maps to whichever chord is active at its onset. `arrange'` feels like an arpeggiator locked to each chord. Both sustain notes across harmony boundaries naturally.

`overlapF N` expands each chord's pitch set with pitches from N bars ahead — natural legato across transitions. `progOverlapB` / `progOverlap` ([§10](#10-progression-manipulation)) are the backward and bidirectional forms.

**Try it** — same progression, same pattern, both functions. Then `overlapF 2` — hear the sustain extend.

> **▶ VIDEO — Across vs within**
> _~45s: the same pattern under arrange then arrange', annotated "melody over changes" vs "arpeggiator per chord"; overlapF raised._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 10. Progression manipulation

**Why** — transform the harmony in real time with pure functions. All return new progressions; chain them freely.

**What** — [`Arranger.hs`](src/Harmonic/Interface/Tidal/Arranger.hs)

| Function | Signature | Effect |
|---|---|---|
| `rotate` | `Int -> …` | Rotate by N bars |
| `excerpt` | `Int -> Int -> …` | Range (1-indexed, inclusive) |
| `transposeP` | `Int -> …` | Transpose by N semitones |
| `reverse` | | Reverse the order |
| `fuse` / `fuse2` | | Concatenate |
| `interleave` | | Alternating chords |
| `expandP` | `Int -> …` | Repeat each chord N times |
| `insert` | `CadenceState -> Int -> …` | Replace bar N |
| `switch` | `Int -> Int -> …` | Swap bars M and N |
| `clone` | `Int -> Int -> …` | Copy bar M to N |
| `extract` | `Int -> … -> CadenceState` | Pull out one state |
| `progOverlapF/B` | `Int -> …` | Merge pitches from ahead/behind |
| `progOverlap` | `Int -> …` | Both directions |

```haskell
rotate 2 s
excerpt 1 4 s
fuse2 s (Harmonic.Interface.Tidal.Arranger.reverse s)   -- palindrome
interleave s (transposeP 5 s)
transposeP 7 $ rotate 3 $ s                              -- chains
```

**Try it** — `expandP 2 s` to slow the harmonic rhythm. `progOverlapF 1 s` for natural sustain. `insert (extract 5 sOther) 5 s` borrows a bar from another progression. `genGrid` in a seek chain skips traversal entirely and prints the raw candidate grid.

> **▶ VIDEO — Pure transformations**
> _~30s: an 8-chord progression under rotate, excerpt, transposeP, and a palindromic fuse — relaunching briefly after each._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 11. Explicit composition

**Why** — when you have specific changes in mind, skip the generator. Generated and explicit progressions are interchangeable everywhere.

**What** — [`Arranger.hs`](src/Harmonic/Interface/Tidal/Arranger.hs), [`Pitch.hs`](src/Harmonic/Rules/Types/Pitch.hs)

```haskell
-- Pitch-class lists
sExplicit = fromChords
  [ [0, 4, 7]    -- C maj
  , [5, 9, 0]    -- F maj
  , [7, 11, 2]   -- G maj
  , [9, 0, 4]    -- A min
  ]

-- Note-name syntax (prime = sharp: C' is C#; flat names Db, Eb, … exist too)
sNamed = prog (notesToPCs <$>
  [ [C, E, G]    -- C maj
  , [F, A, C]    -- F maj
  , [G, B, D]    -- G maj
  , [A, C, E]    -- A min
  ])

-- Full CadenceState construction with explicit root movement
sStates = fromCadenceStates
  [ initCadenceState 0 "C" [0, 4, 7]
  , initCadenceState 5 "F" [0, 4, 7]
  , initCadenceState 2 "G" [0, 4, 7]
  , initCadenceState 2 "A" [0, 3, 7]
  ]
```

`prog` is an alias for `fromChords`; `notesToPCs` maps `NoteName` lists to pitch classes.

**Try it** — build your own 8-bar progression, apply `transposeP 2`, then `interleave` it with a generated one — hand and machine in one form.

> **▶ VIDEO — By hand**
> _~30s: a 4-chord progression in note-name syntax, played by the standard launcher, then transposed._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 12. Regenerate in place (`genFrom`)

**Why** — keep the progression you like; regenerate only the bars you don't. Surgery, not do-over.

**What** — [`Builder.hs`](src/Harmonic/Framework/Builder.hs)

```haskell
s8  <- seek "*" $ cue start $ tonal ctx $ len 8 $ entropy 0.5 $ gen

s8' <- seek "*" $ entropy 0.5 $ genFrom s8 3 4          -- bars 3–4 only

s8'' <- seek "*" $ attempt 3 12 $ entropy 0.5 $ genFrom s8 3 4  -- ranked patch
```

`genFrom s a b` regenerates bars `a..b` (1-indexed, **wrap-aware**: on a 5-bar progression, `genFrom s 4 2` walks bars 4, 5, 1, 2 and keeps 3). The cue is auto-inferred from the bar before `a` so the new material connects; override with `cue`. The printed grid is always the **full spliced progression** — the source with the new bars in place.

Diagnostic variants mirror `gen`: `genFrom'` (standard trace), `genFrom''` (verbose; scoreboard under `attempt`). Composes with the whole modifier chain.

**Try it** — regenerate the final bars (`genFrom s8 7 8`) — the seam back to bar 1 is handled, since progressions cycle. Then a wrapping range (`genFrom s8 7 2`). A/B each patch.

> **▶ VIDEO — Surgery**
> _~40s: an 8-bar progression, bars 3–4 regenerated; the two grids diffed on screen; the ranked-patch variant._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 13. Kinetics form (programmed arc)

**Why** — wall-clock time as a compositional parameter. A form is a list of nodes, each at a specific time, with kinetics (0–1 intensity) and dynamics (level); between nodes the signals interpolate and the progression switches discretely.

**What** — [`Form.hs`](src/Harmonic/Interface/Tidal/Form.hs)

```haskell
sA <- seek "*" $ cue start $ tonal (consonant $ hcKey "0#" $ hContext) $ len 4 $ entropy 0.3 $ gen
sB <- seek "*" $ cue start $ tonal (dissonant $ hcKey "0#" $ hContext) $ len 4 $ entropy 0.85 $ gen

arcForm =
  [ at   0   0.0  0.0  sA
  , at   5   0.3  0.4  sA
  , at  20   0.5  0.6  sA
  , at  30   0.8  0.8  sB   -- progression switch at the rise
  , at  40   1.0  1.0  sB
  , at  50   0.5  0.65 sA
  , at  60   0.0  0.0  sA
  , at  70   0.0  0.0  sA
  ]

p01arc f k d = d01 $ do
  let o = ch 01
  f
    $ stack [silence
      , arrange (0, 1)   k (-9, 9) T flow (overlapF 0) ["~", "[0 1 2 3]/4"]    # o |* vel 0.75
      , arrange (0.7, 1) k (-9, 9) T flow (overlapF 0) ["~", "[0 2 4 7 4 2]/4"] # o |+ oct 1 |* vel 0.6
    ] |* vel d
```

The upper voice has kinetics range `(0.7, 1)` — it exists only during the climactic middle. The pad `(0, 1)` plays throughout. Kinetics gate `arrange` ranges; dynamics drive amplitude; progression switches let calm and adventurous material occupy different regions of one piece.

**Snap transitions & bar-based nodes** — `at` has three siblings on two orthogonal axes:

| Constructor | Time unit | Transition |
|-------------|-----------|------------|
| `at`  | seconds | smooth (ramp) |
| `at'` | seconds | snap (hold, then jump) |
| `rh`  | bars (4/4) | smooth |
| `rh'` | bars | snap |

Mix them freely in one form. **Form templates** (inlined in the .tidal, §13): `narrative` — the ~7m24s spectral arc (10 nodes, scale the times to taste); `popForm` — verse-chorus-bridge in bars.

**Try it** — stretch the arc to 5 minutes or compress to 20s. Change the upper gate to `(0, 0.3)` — it becomes the lead-in. Flip `rh' 8` to `rh 8` — the hard cut becomes a ramp.

> **▶ VIDEO — The arc**
> _~60s: launch the 60-second arc — kinetics rising on screen, the progression switching at the peak, the upper line entering at 0.7 and leaving on the way down._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 14. Live kinetics & gating (`lK`, `ki`, `slate`)

**Why** — drive kinetics from a live signal instead of a keyframed form. The arc becomes a control you perform.

**What** — [`Form.hs`](src/Harmonic/Interface/Tidal/Form.hs)

```haskell
-- any Pattern Double (0–1) works as a kinetics/dynamics source:
do
  let kin = slow 64 $ lfo tri 0 1        -- a 64-beat triangle sweep
      k   = lK kin kin s (rep s 1)
  mapM_ id [hush, setbpm tempo, p01arc id k 0.85]
```

`lK kinSig dynSig s chordSel` builds an `IK` from two live signals — an LFO, a random walk, a MIDI CC. Everything downstream (range gating, dynamics) behaves exactly as with `iK`. (The Q-Link controller bridge in BootTidal — `qlink1..4`, `xyX`/`xyY`, `exP` — supplies hardware CC signals on this interface; the LED display feed `display k` is likewise rig-specific. Both are pointers, not walkthrough scope.)

The same range-gating `arrange` applies is available on **any** pattern:

```haskell
ki (lo, hi) k pat        -- mask pat to the kinetics window
slate (lo, hi) k pats    -- stack pats, gated as one

-- kinetics-layered drums without arrange:
d10 $ stack
  [ slate (0.2, 1) k [ kick  "[1 0 0 0]/4" ]
  , slate (0.5, 1) k [ hhcl  "[0 1 0 1]/4" ]
  , slate (0.8, 1) k [ snare "[0 0 1 0]/4" ]
  ]
```

**Try it** — replace the LFO with `slow 16 $ lfo sine 0 1` or `segment 8 $ perlin`. `withForm k f` gives a custom pattern function the active progression reactively.

> **▶ VIDEO — Performed kinetics**
> _~45s: an LFO sweeping the arc hands-free; then the drum stack assembling itself as the signal climbs._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 15. Groove (`subKick`)

**Why** — kick and sub bass that follow the harmonic root. `subKick` combines a kick pattern, a sustained sub, and an MPC-style CC64 sustain mechanism, locked to the root of whichever chord is active at each onset.

**What** — [`Groove.hs`](src/Harmonic/Interface/Tidal/Groove.hs)

```haskell
subk f k d = p "subKick"
  $ f
    $ subKick d k root ( 1/4
      , "[1(3,8) . ~]"    -- sub bass on: euclidean 3-of-8 with rest
      , "[0 1 0 0]"       -- sub bass off: control
      , "1*4"              -- kick: four on the floor
    )

do
  let k = iK tempo [at 0 1 1 s] (rep s 1)
  mapM_ id [hush, setbpm tempo
    , p01 id k 0.9
    , subk id k 0.8
    ]
```

Routes to MIDI channel 10. (A complementary MPC kit program renders the full-range sub+kick; any drum sound on channel 10 works for the walkthrough.)

**Try it** — push the kick (`"[1 0 0 1]"`), or swap `root` for `fund` — the sub follows the harmonic fundamental instead of the chord root (inversion-proof low end).

> **▶ VIDEO — Rooted low end**
> _~30s: the pad alone, then subKick joining — the sub tracking the changes; root vs fund on an inverted chord._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 16. Walking lines (`walk` / `lineHarmony`)

**Why** — a walking bassline synthesised from the progression: consonant anchors on the strong beats (root on 1; P5/root/3rds on 3), weighted connectors on beats 2 and 4 preferring sandwich motion and approach tones, and a soft direction-persistence bias on the beat-1 contour. The walk follows the **performed** bar order — warp/rep selections resolve at eval time, repeated bars walk as neighbours (root–fifth alternation), approach tones aim at the bar that actually comes next; non-periodic selections fall back to stored order with a printed notice. Jazz progressions (`genJ`) walk from a per-quality **bass vocabulary** rather than the raw corpus set: the fifth a 13th chord omits is restored, an altered dominant's #5 replaces the natural 5 it does not contain, notated colours (b9, #9, #11, b13) stay off strong beats while remaining available as weak-beat tension, and the 9th and 11th act as favourable passing tones. `genP` walks its strata chroma as before; `gen` walks the plain tone sets, and `genE` walks its foundation (T) layer — the layer that owns the bass.

**What** — [`LineHarmony.hs`](src/Harmonic/Interface/Tidal/LineHarmony.hs), [`WalkingBass.hs`](src/Harmonic/Traversal/WalkingBass.hs)

```haskell
walk f k d = p "lineHarmony"
  $ f
    $ lineHarmony d k root [ "~"
      , "[1 2 3 4]/4"          -- full quarters
    ] # ch 01 # legato 0.95

do
  let k = iK tempo [at 0 1 1 s] (rep s 1)
  mapM_ id [hush, setbpm tempo
    , (d01 $ arrange (0, 1) k (-9, 9) T flow (overlapF 0) ["~", "[0,1,2]/1"] # ch 01 |+ oct 1 |* vel 0.4 |= legato 3.6)
    , walk id k 0.9
    ]
```

The patterns are 1-indexed **beat positions** (which beats of each bar the line sounds on), not voicing degrees. The list length N partitions the kinetics signal into N `kinPick` windows and only the window holding the current signal plays — a leading `"~"` silences the low-kinetics half. The `d` argument is applied once inside `lineHarmony`; do **not** also `|* vel d` outside (that squares the dynamic). Dynamics also steer the walk: bars quieter than the piece's own mean bias the register arc upward, louder bars downward, and a sudden drop (0.25+ between bars) resets the line to its lowest note — only eval-time-sampleable dynamics count (the `d` argument and form-node dynamics; live control signals and downstream `|* vel` are walk-neutral). The line is fixed to a 21-semitone double-bass register (E1–C3 at the default patch; the absolute octave depends on the synth via `tidalNoteOffset`); `voiceFn` (`root` or `fund`) anchors beat 1. Sparser feels compose like any Tidal pattern: `"[1 3]/4"` (two-feel), `"[1 2 ~ 4]/4"`, `"[1 [2 2] 3 4]/4"` (eighth-note fill).

For octatripentatonic progressions ([§19](#19-the-three-layers-tsm--genp)) the line's connector pool automatically reweights toward the 5-PC strata set — the walk speaks the same dialect as the harmony.

**Try it** — mute the pad and listen to the line alone: the strong beats carry the changes by themselves.

> **▶ VIDEO — The duet partner**
> _~45s: pads + walking line; the pads mute and the line still tells you the changes; a sparser two-feel._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 17. Voice lines & the instrument paradigm

**Why** — address stacked voices as independent patterns with their own kinetics ranges, voicing functions and registers. The foundation the virtual orchestra is built on.

**What** —

```haskell
p01satb f k d = d01 $ do
  let o = ch 01
  f
    $ stack [silence
      , arrange (0, 1) k (-9, 9) T flow (overlapF 0) ["~", "[3]/1"] # o |+ oct 1 |* vel 0.7
      , arrange (0, 1) k (-9, 9) T flow (overlapF 0) ["~", "[2]/1"] # o           |* vel 0.6
      , arrange (0, 1) k (-9, 9) T flow (overlapF 0) ["~", "[1]/1"] # o           |* vel 0.6
      , arrange (0, 1) k (-9, 9) T root (overlapF 0) ["~", "[0]/1"] # o |- oct 1 |* vel 0.8
    ] |* vel d
```

Four voices — soprano, alto, tenor, bass — each an `arrange` call with a different degree and octave offset.

### Divisi

When a voice splits into desks, each desk is quieter than the undivided line. Divisi handles the split and the equal-power (`1/√n`) compensation. Each voice carries three tiers in one `voiceLines` record — `soprano`, `soprano'`, `soprano''` — all ordinary degree patterns; unset tiers default one/two degrees up.

```haskell
vl = voiceLines {_vl = "~"
  , soprano  = "[3 2 1 0]/4"    -- desk 1
  , soprano' = "[5 4 3 2]/4"    -- desk 2
  }
-- hand-built desks, tagged:
, arrange (0,1) k (-9,9) T flow (overlapF 0) ["~", vlGet Soprano  vl] # o # divisi2
, arrange (0,1) k (-9,9) T flow (overlapF 0) ["~", vlGet Soprano' vl] # o # divisi2 |+ oct 1
```

With the orchestral instrument functions the same split is one line — `divisi 2 violin1 T (0, 1) k vl grid Soprano` — and primed voices (`Soprano'`, `Bass8vb'`) pick desks manually. The full system (16 instruments — 14 pitched, 2 unpitched percussion — articulations, section blocks, timbral blends) lives in [`ALGORITHMIC_ORCHESTRATION.md`](documents/ALGORITHMIC_ORCHESTRATION.md); it depends on a specific MIDI rig, so this guide stops at the paradigm.

> **▶ VIDEO — Stacked voices**
> _~45s: the SATB stack assembling voice by voice; a divisi split with the equal-power drop audible._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 18. Motifs (motivic development)

**Why** — name the recurring material a piece is built from, and develop it with the classic tools instead of retyping strings.

**What** — [`BootTidal.hs`](live/BootTidal.hs) (`>:<`, `mirror`, clave library). Motifs are plain patterns: a `Pattern Bool` **rhythm** and a `Pattern Int` **contour** (voicing degrees — realised against the harmony by `arrange`, so a contour tracks the chords). One statement binds all three names:

```haskell
(rhythm, contour, motif) =
  ( son32                 -- rhythm gate (a son clave)
  , "[3 2 1 0]/4"         -- contour (a descending figure)
  , rhythm >:< contour
  ) :: (Pattern Bool, Pattern Int, Pattern Int)
```

Primed tiers (`rhythm'`, `contour''`, …) fall back to BootTidal defaults. Blocks read a contour as a degree pattern, gate a voice or drum (`struct rhythm $ …`), or use the pre-combined `motif`.

| Development | Tool | Example |
|---|---|---|
| Retrograde | `retro` / `retroN` | `retro contour` |
| Inversion | `mirror axis` | `mirror 3 contour` |
| Augmentation / diminution | `slow` / `fast` | `slow 2 motif` |
| Transposition | `\|+` / `\|-` | `contour \|+ 2` |
| Rotation | `<~` / `~>` | `"<0 1>" <~ rhythm` |
| Combination | `struct` / `>:<` | `rhythm >:< contour` |

**Scope: Tidal's transforms work on the cycle, not the bar.** One cycle is one
beat, so a bar-length figure carries `/4` and spans four cycles. Tidal's `rev`
then reverses each beat on its own and leaves the phrase intact — which reads
as though it did nothing. `retro` is provided for this: it reverses the whole
bar, and `retroN n` reverses over `n` cycles for longer or shorter phrases.

```tidal
rev    contour   -- each beat reversed; phrase order unchanged
retro  contour   -- true phrase retrograde  (retro . retro = id)
retroN 8 motif   -- retrograde a two-bar phrase
```

Rotation has the same trap: `~>` shifts in cycles, so shifting a figure by a
multiple of its own period is a silent no-op — rotating `"[3 2 0 1]*4"`
(period ¼ cycle) by ¼ lands back on itself. Rotate bar-scoped material
instead: `0.5 ~> contour` gives a canon at the half-beat.

When checking a development, note that `queryArc` returns events in **query
order, not time order**. Inspect with the start time attached, or sort first —
otherwise a perfectly good `rev` reads as a no-op:

```tidal
[ (wholeStart e, value e) | e <- queryArc (rev contour) (Arc 0 1) ]
```

**How** — a contour is voicing-index degrees, so it re-realises against whatever chord is sounding; restate an idea at a new pitch by transposing the *harmony* beneath a fixed contour. `mirror axis d = 2*axis - d`.

**Try it** — re-execute the panel with `contour = "[0 1 2 3]/4"` or `rhythm = bossa32` and hear the piece reprogram in one step. Claves: `son32`, `rumba32`, `bossa32`, `bellpat32` (+ 2-3 rotations: `son23` …).

> **▶ VIDEO — One idea, developed**
> _~45s: the motif stated, then inverted against itself, then re-rhythm'd with one panel edit._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 19. The three layers (T/S/M) & `genP`

**Why** — every progression carries three bar-aligned layers, and patterns choose which to read:

| Layer | genP content | genE content | Practice analogue |
|---|---|---|---|
| `T` | the triads (3 PCs) | the foundation triads | chord playing |
| `S` | a pentatonic per bar (5 PCs) | the less dissonant partner triad | pattern playing |
| `M` | a diatonic mode per bar (7 PCs) | the more dissonant partner triad | scale/modal playing |
| `TS` `TM` `SM` | layer unions | 4-note pair structures | comping voicings |
| `TSM` | full union | the 5-tone pentad | tutti verticals |
| `PT` | (the triad) | the pivot tones all three layers share | pedal/anchor lines |

Every selector works on every context: combination bars are pointwise unions rooted on the lowest constituent layer (T before S before M — the foundation owns the merged bass), and on plain `gen` contexts every selector degrades to the triad progression. Plain `gen` fills all three stored layers with the triad. The strata-first generator **`genP`** populates them by walking eleven canonical pentatonic **strata** (I–XI) grouped into twelve curated **tristrata** whose pairwise unions are diatonic modes (full theory: [`OCTATRIPENTATONICS.md`](documents/OCTATRIPENTATONICS.md)); the polytonal generator **`genE`** fills them with independent partner triad chains (§20, [`POLYTONAL.md`](documents/POLYTONAL.md)).

**What** — [`Builder.hs`](src/Harmonic/Framework/Builder.hs), [`Strata.hs`](src/Harmonic/Framework/Builder/Strata.hs)

```haskell
-- 33 aliases: genI..genXI × {plain, ', ''}
pc19 <- seek "*" $ cue start $ len 8 $ entropy 0.5 $ genVI'

-- the layer argument of arrange selects the density:
arrange (0,1) k (-9,9) T flow (overlapF 0) ["~", "[0 1 2 3]/4"]   -- triads
arrange (0,1) k (-9,9) S flow (overlapF 0) ["~", "[0 1 2 3]/4"]   -- strata
arrange (0,1) k (-9,9) M flow (overlapF 0) ["~", "[0 1 2 3]/4"]   -- modes
```

`genP'` prints a per-bar block: triad selection + active strata (5 PCs) + classified mode (7 PCs) + tristrata identity — the full anatomy, in one coherent spelling system.

**Steering the walk:**

```haskell
hcTristrata "5" hContext            -- lock to tristrata #5 (IV-VI-X)
relStrata "1 1 2 2 3 3" $ genVI'    -- per-bar position in the active tristrata
absStrata "I V X" $ genI'           -- per-bar absolute strata labels
genPReport pc                       -- provenance + triads in one report
```

**Continuity boosts** — the walk prefers staying put; three multipliers bias it (defaults 0.90/0.80/0.70; `1.0` disables): `sameBoost` (same strata as last bar), `flipBoost` (same as two bars ago), `triBoost` (same tristrata).

**Try it** — `S`-layer patterns under a soloist is pentatonic practice with moving ground. `attempt 3 12 $ … $ genVI''` ranks whole walks. The walking bass (§16) reweights automatically over `genP` progressions.

> **▶ VIDEO — Three densities of one harmony**
> _~60s: the same genVI' walk played at T, then S, then M — three densities; the per-bar diagnostic on screen naming strata and mode._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 20. Polytonal generation (`genE`)

**Why** — three simultaneous triad progressions from one walk: a foundation plus two partner chains that share tones with it bar by bar, so any pair of layers sounds a 4-note structure and all three sound a 5-tone pentad. Independent patterns, coherent verticals.

**What** — the T layer is a foundation walk byte-identical to `gen` (all R constraints — key, roots, rise/fall, drift, pedal — bind it alone: the foundation owns the bass). The S/M layers are partner triad chains, each a corpus-valid walk of its own: every partner bar continues from ITS OWN previous bar's transitions, filtered to share exactly 2 pitch classes with that bar's foundation triad and union to exactly 5. The traversal chooses freely per bar between the two geometries this admits — **common-dyad** (all three triads share one dyad; every layer pair sounds 4 tones) and **base-anchored** (the partners take different dyads of the foundation; T+S and T+M sound 4 tones, S+M sounds the full pentad). Partners honour the harmonic space (`hcKey`/`hcRoots` note-set/`tonal`) but ignore direction specs and drift — they stay free to diverge. S is the chain with the lower whole-run dissonance total; M the other.

```haskell
start <- lead "C maj"
s <- seek "*" $ cue start $ len 8 $ entropy 0.3 $ genE'   -- genE / genE' / genE''
```

Cues are triadic (a 4-note cue is refused — the combined structures come from the layers). The zero-prime output prints the foundation grid plus the combined `TSM` grid; `genE'` adds a per-bar table (partners, pair structures, pentad, pivot tones); `genE''` adds the selection facts (geometry, pool tier and size, own-list ranks). At the REPL, `genEReport s` prints every layer view a pattern can select.

```haskell
arrange (0,1) k (-9,9) T   flow (overlapF 0) ["~", "[0 1 2 3]/4"]  -- foundation alone
arrange (0,1) k (-9,9) S   flow (overlapF 0) ["~", "[0 1 2 3]/4"]  -- a partner alone (its own roots)
arrange (0,1) k (-9,9) TS  flow (overlapF 0) ["~", "[0 1 2 3]/4"]  -- 4-note pair
arrange (0,1) k (-9,9) TSM flow (overlapF 0) ["~", "[0 1 2 3]/4"]  -- the 5-tone pentad
arrange (0,1) k (-9,9) PT  flow (overlapF 0) ["~", "[0 1]/4"]      -- the pivot tones
```

A layer used alone keeps its own roots and bass; T owns the bass only inside merged selectors. Everything composes as usual, including regeneration:

```haskell
s  <- seek "*" $ attempt 3 12 $ entropy 0.4 $ genE''      -- rank-and-select (scores the foundation)
s' <- seek "*" $ entropy 0.3 $ genFrom s 3 5              -- family-aware: partner chains regen too, seeded from the kept bars
```

Under a crushing context the foundation behaves exactly like `gen` (absorbing repetition at worst); partner selection can never fail — it relaxes from the corpus lists to a pure enumeration that always holds valid pairs. Full theory and the study behind the design: [`POLYTONAL.md`](documents/POLYTONAL.md).

For an explicit 4-note opening in HAND-BUILT material there is still `lead'`, which takes note names directly (first note = root/bass; typed accidentals drive spelling; optional `(N)` movement like `lead`) — such material plays and walks as-is; regenerating it produces triads with a printed notice:

```haskell
start <- lead' "Eb Gb Bb Db"        -- Eb m7, printed and used as-is
start <- lead' "A C E G (5)"        -- A m7, ascending-5th approach
```

**Try it** — generate once, then play `T` on one instrument, `S` on another, `M` on a third: three independent progressions that lock into pentads. Swap a part to `TS` for comping density; put the walking bass (§16) on `T`.

> **▶ VIDEO — Three progressions, one harmony**
> _~60s: one genE' run played layer by layer — T, then S, then M — then together; the per-bar partner table on screen._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 21. Jazz generation (`genJ`)

**Why** — a second corpus, a second harmonic language: leadsheet changes (m7, 7#9, 13b9#11, slash chords) walked with the same modifier chain, trained on the Jazz-Chord-Progressions-Corpus (Bunks/Weyde/Dixon/Di Giorgi, ISMIR 2023 — 2,614 tunes).

**What** — a generator family. The jazz graph (`:Change`) lives beside the classical one in the same database; nodes are anchor-relative chord sets at corpus arity (3–6 tones), named by a jazz-specific namer, and there is deliberately **no consonance fallback** — the corpus is rich enough that the graph IS the pool (measured: ~219-candidate typical steps, zero dead ends). Cue with `leadJ`, which parses leadsheet symbols in their own quality namespace (triadic `lead`/`lead'` untouched); a notated slash bass is honoured exactly — unioned into the set and made the anchor:

```haskell
start <- leadJ "Cm7"                -- corpus workhorse
start <- leadJ "Dm7/G"              -- G anchor, full Dm7 above (a 9sus4 set)
s <- seek "*" $ cue start $ len 8 $ entropy 0.3 $ genJ'    -- genJ / genJ' / genJ''
```

Jazz progressions play through every interface unchanged, and the walking bass (§16) understands them natively: `lineHarmony` reads each bar's quality as bass vocabulary — restoring the fifth a 13th chord omits, taking an altered dominant's #5 as its fifth, keeping b9/#9/#11/b13 off the strong beats — so `walk` over `genJ` changes sounds like a bassist reading the chart rather than the voicing.

One `seek` spec drives both corpora. Jazz composer names blend by corpus weights (substring-matched — `"monk:60 coltrane:40"` finds `theloniousmonk` and the co-credits). A **classical** name doesn't walk the jazz graph — it *steers* it: each step, the current chord's most consonant embedded triad asks the classical graph what that composer would do next, and jazz candidates containing a recommended triad (same movement) get boosted. Both can combine, and `steer` dials the boost strength:

```haskell
s <- seek "theloniousmonk:60 johncoltrane:40" $ cue start $ len 8 $ genJ'
s <- seek "debussy" $ steer 8 $ cue start $ len 8 $ genJ'   -- jazz walk, Debussy pull
s <- seek "monk debussy" $ cue start $ len 8 $ genJ'        -- blend AND steer
```

`seek "none"` is refused — the jazz graph has no offline mode. Everything else composes as usual: `tonal` runs the same R filters over the jazz candidates (key-of-C pins the walk to diatonic corpus vocabulary — Am7, Dm7, G7sus4, CM7...), `genFrom` regenerates ranges jazz-natively (the output is stamped as jazz-family), and `attempt` ranks against the jazz graph's own cadence statistics:

```haskell
s  <- seek "*" $ cue start $ tonal (hcKey "0#" hContext) $ len 8 $ genJ'
s' <- seek "*" $ entropy 0.6 $ genFrom s 3 4
s2 <- seek "*" $ attempt 3 8 $ cue start $ len 8 $ genJ''   -- scoreboard
```

Output is an ordinary `ProgressionContext`: `flow`/`grid` voice the 3–6-tone chords, `arrange` and the walking bass play them unchanged.

**Try it** — the guided tour with live database probes is [`live/JAZZ_DIAGNOSTIC.tidal`](live/JAZZ_DIAGNOSTIC.tidal); work down it section by section.

> **▶ VIDEO — Two corpora, one walk**
> _~90s: leadJ "Dm7/G" cue, a wildcard jazz walk, then the same cue under seek "debussy" steer 8 — the M7-colour pull audible side by side._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 22. Snippets & the liveness gradient

**Why** — choose, deliberately, how much of a performance is prepared and how much is conjured at the keyboard.

**What** — [`live/snippets.cson`](live/snippets.cson) (Pulsar, TidalCycles grammar: type a prefix, press Tab, fill the tab-stops). Three working modes on a gradient:

- **Prepared state file** — a complete launch block evaluated in one gesture. Instant sound; everything decided in advance.
- **Snippet expansion** — structure arrives from the library; decisions (key, composer, entropy, patterns) are made live at the tab-stops.
- **Blank page** — everything typed. Slowest to music; every decision visible and live.

None is "more correct" — they trade immediacy against decidedness. A performance can move along the gradient: open from a state file, develop by snippet, end blank-page.

**The prefix index:**

| Group | Prefixes |
|---|---|
| Session | `transport` · `state` · `movement` |
| Generation | `lead` · `ctx` · `gen` · `formless` · `dance` |
| Launchers | `launch` · `p` · `rrange` · `slate` · `minimal` · `deeptech` |
| Groove & lines | `subk` · `walk` |
| Motifs | `mpanel` · `motif` · `develop` |
| Control | `cc` · `disp` |
| Orchestra* | `orchsec` · `orchblend` · `tutti` · `instr` · `divisi` · `divisidesk` |
| Hardware* | `k909` · `kmpc` · `kgrv` · `m32` · `mdf` |

(\* rig-specific — see Known exceptions.)

The `state` snippet is the canonical prepared opening: tempo, context, a generated progression under `attempt`, a one-node form, and a launcher with commented instrument lines — the same shape this guide has been building by hand since §3.

**Try it** — rebuild §3's launcher from memory on a blank page, timing yourself. Then from the `state` snippet. The difference is the gradient, felt.

> **▶ VIDEO — The gradient**
> _~45s: the same music reached three ways — state file (one gesture), snippets (tab-stops), blank page — side by side on a clock._
>
> `[ youtube link — TBD ]`
>
> <!-- author discussion space -->

___

## 23. Going further

**Instrument catalogue** —
- [`live/ORCHESTRAL_CATALOGUE.tidal`](live/ORCHESTRAL_CATALOGUE.tidal) — range tests per instrument

**Documentation** —
- [`CHANGELOG.md`](CHANGELOG.md) — V3 feature summary
- [`OCTATRIPENTATONICS.md`](documents/OCTATRIPENTATONICS.md) — strata/tristrata reference
- [`POLYTONAL.md`](documents/POLYTONAL.md) — the polytonal genE framework
- [`ALGORITHMIC_ORCHESTRATION.md`](documents/ALGORITHMIC_ORCHESTRATION.md) — virtual orchestra
- [`ARCHITECTURE.md`](documents/ARCHITECTURE.md) — R→E→T pipeline, four-layer architecture, graph schema

**Drum patterns** —
- [`live/drumpats/`](live/drumpats/) — genre pattern library (kick/snare/hh helpers in BootTidal)

### Quick reference

**Generation** —

```haskell
start <- lead "C maj"                                        -- or initCadenceState
s <- seek "*" $ cue start $ len 4 $ entropy 0.5 $ gen        -- corpus aggregate
s <- seek "bach:30 debussy:70" $ ... $ gen'                  -- blend + diagnostics
s <- seek "none" $ ... $ gen                                 -- offline fallback
```

**Rank & select** — `attempt N K` (best of K, stop at N viable), `viability T` (floor, default 0.5); scoreboard under `gen''`.

**Regenerate in place** — `genFrom s a b` (wrap-aware, cue auto-inferred), `genFrom'`/`genFrom''`.

**Strata-first (T/S/M)** — `genI..genXI` (+`'`/`''`); `hcTristrata "5"`, `relStrata "1 2 3"`, `absStrata "I V X"`, `sameBoost`/`flipBoost`/`triBoost N`, `genPReport`.

**Polytonal (genE)** — `genE`/`genE'`/`genE''`: foundation walk + two partner triad chains; layer selectors `TS`/`TM`/`SM`/`TSM`/`PT` for pairs, the pentad and the pivot tones; `genEReport s` prints every layer view.

**Context modifiers** — compose with `$`, right-to-left: `hContext`, `hcOvertones`, `hcKey`, `hcRoots`, `hcPedal`, `consonant`, `dissonant`, `invSkip`, `hcTristrata`.

**Voicing** — `flow`, `grid`, `lite`, `root`, `fund`.

**Chord selection** — `rep s 1`, `warp "[1 2 3 4]/4"`.

**Arrangement** —

```haskell
arrange  (lo,hi) k (-9,9) LAYER voicing modifier [patterns] # ch N   -- LAYER = T | S | M | TS | TM | SM | TSM | PT
arrange' (lo,hi) k (-9,9) LAYER voicing modifier [patterns] # ch N   -- squeeze variant
```

**Form** — `at time kinetics dynamics progression`; siblings `at'` (secs+snap), `rh` (bars+smooth), `rh'` (bars+snap). `iK tempo [nodes] chordSel`; live: `lK kinSig dynSig s chordSel`; gating: `ki (lo,hi) k pat`, `slate (lo,hi) k pats`, `withForm k f`.

**Groove & lines** — `subKick d k root (maxDur, subOn, subOff, kickPat)` (ch 10); `lineHarmony d k root ["~", "[1 2 3 4]/4"]` (walking bass, beat positions).

**Divisi** — `divisi N instr T (lo,hi) k vl voicing Voice`; primed voices `Soprano'`/`Bass8vb'` pick a desk; `# divisi2`/`# divisi3` volume tags.

**Motifs** — `(rhythm, contour, motif) = (…, …, rhythm >:< contour)`; develop with `rev`, `mirror axis`, `slow`/`fast`, `|+`, `<~`, `struct`/`>:<`. Claves: `son32`, `rumba32`, `bossa32`, `bellpat32` (+ 2-3 rotations).

**Manipulation** — `rotate`, `excerpt`, `transposeP`, `reverse`, `fuse`, `fuse2`, `interleave`, `expandP`, `insert`, `switch`, `clone`, `extract`, `progOverlap`, `progOverlapF`, `progOverlapB`.

**Explicit** — `fromChords [[0,4,7], …]`; `prog (notesToPCs <$> [[C,E,G], …])`; `fromCadenceStates [initCadenceState mov "Root" [ints], …]`.

**Snippets (Pulsar)** — `transport` · `state` · `movement` · `lead` · `ctx` · `gen` · `formless` · `dance` · `launch` · `p` · `rrange` · `slate` · `minimal` · `deeptech` · `subk` · `walk` · `mpanel` · `motif` · `develop` · `cc` · `disp`.

___

Questions and feedback via the [GitHub Issues](https://github.com/OscarSouth/theHarmonicAlgorithm/issues) tracker.
