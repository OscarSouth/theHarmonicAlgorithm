# The Harmonic Algorithm — User Guide

This is the readable companion to [`live/USER_GUIDE.tidal`](live/USER_GUIDE.tidal). To work through it interactively, evaluating each block and hearing the results as you go, open the `.tidal` file in your TidalCycles editor and scroll from top to bottom. The two files present the same material in the same order.

Sections link a video walkthrough once one is published.

### MIDI output

This system outputs MIDI on TidalCycles channels 1–16 (`d01`–`d16`). **Everything in this guide transmits on MIDI channel 1** (`d01`), except the groove sections (channel 10, drums). Route channel 1 to a polyphonic sound (piano, Rhodes, pad, polysynth) in your MIDI setup. That's your instrument for the whole walkthrough.

### The graph

Generation draws on a Neo4j graph of harmonic transitions learned from over 10,000 compositions by over 1,000 classical and jazz composers (the Yale Classical Archives Corpus and the Jazz-Chord-Progressions-Corpus): 8,441 classical pieces by 488 composers, most of all Mozart, Bach, Scarlatti and Beethoven, and 2,614 jazz tunes by 679 composers, led by Richard Rodgers, Thelonious Monk, Harold Arlen and Duke Ellington. The [Composer Catalogue](documents/COMPOSERS.md) has the full breakdown and every composer key. Start it before you begin:

```bash
docker compose up -d neo4j
```

If you can't run Docker right now, `seek "none"` ([§5](#5-the-composer-graph)) runs the classical examples on the built-in consonance fallback instead. They stay fully musical, just without corpus-trained style. Jazz generation (§21) needs the graph.

### Setup

- [Haskell Stack](https://docs.haskellstack.org/en/latest/install_and_upgrade/)
- [Docker](https://www.docker.com/) for the composer graph
- [TidalCycles](https://tidalcycles.org/) with SuperCollider + SuperDirt
- `live/BootTidal.hs` loaded with `Harmonic.Lib`

```bash
stack build                       # compile the library
docker compose up -d neo4j        # graph backend
```

Load the graph once from the pre-built database published with each release; the steps are in the README's [Add the composer graph](README.md#2-add-the-composer-graph).

The MIDI streams (`d01`–`d16`) and the `hush`/`panic` family live in [`live/BootTidal.hs`](live/BootTidal.hs). The helpers themselves are library modules: `ch`/`vel` in `Harmonic.Interface.Tidal.Instruments`, `oct` in `.Utils`, the drum map in `.Devices.JV1010`.

### Known exceptions

Two areas are deliberately out of scope:

- **The Algorithmic Orchestration system** (a 16-instrument virtual orchestra) depends on a specific hardware and MIDI rig. See [`ALGORITHMIC_ORCHESTRATION.md`](documents/ALGORITHMIC_ORCHESTRATION.md) and [`live/docs/ORCHESTRAL_CATALOGUE.tidal`](live/docs/ORCHESTRAL_CATALOGUE.tidal).
- **Hardware-bound helpers**: the AIRA program's four layers (`s101`, `smpl`, `auto`, `gnlr`, backed by `Harmonic.Interface.Tidal.Devices.S1` and `.P6`), the JV-1010 drum map (`.Devices.JV1010`), the LED display feed (`Harmonic.Interface.Tidal.Display`), the Q-Link controller bridge, and the MPC kit program behind `subKick`. The rig they address is described in [`LIVE_ENVIRONMENT.md`](documents/LIVE_ENVIRONMENT.md). This guide stays on the single piano channel plus generic drums.

___

## 0. Setup & sound check

Before anything musical, confirm the three moving parts: MIDI routing, the library, and the graph.

```haskell
once $ note "0" # ch 01 # legato 0.9   -- one middle C

tempo = 90
start <- lead "C maj"
s <- seek "*" $ cue start $ len 4 $ entropy 0.5 $ gen
print s                                       -- a progression header
```

The single note confirms MIDI channel 1. The boot banner ("theHarmonicAlgorithm V3 boot complete.") confirms the library, and the printed progression confirms the graph. A connection error here means Neo4j isn't running.

<!-- video §0 -->

___

## 1. First generation

The simplest invocation: a four-chord progression from a starting chord, shaped by the corpus and an entropy dial.

```haskell
start <- lead "C maj"

s <- seek "*" $ cue start $ len 4 $ entropy 0.5 $ gen

print s
```

`lead` ([`Arranger.hs`](src/Harmonic/Interface/Tidal/Arranger.hs)) parses a readable string: `"C maj"`, `"E min"`, `"Bb 7 (7)"`, giving the root, the quality and an optional ascending movement. Use `initCadenceState 0 "C" [0,4,7]` for explicit interval control.

`lead` returns `IO CadenceState`, so bind it with `<-`. The `seek` terminal runs the whole modifier chain, and `"*"` aggregates the full corpus. `cue` sets the starting state and `len` the chord count. `entropy` (0.0–1.0+) dials between the familiar and the surprising: the value times ten is the rank the sampler aims at in the candidate list, so `0.2` reaches for roughly the second-ranked continuation and `0.9` for the ninth. It's a target, and the actual draw is a distribution around it.

Omit `cue` and the opening chord is drawn to fit: inside the `tonal` context for `gen`/`genE` (an uncued walk under `hcKey "3b"` opens in three flats), from inside the stratum for `genP`, and from the jazz graph's most-travelled structures for `genJ`.

**Try it.** Change `"C maj"` to `"E min"` or `"Bb 7 (7)"`. Swap `entropy 0.5` for `0.2` (conventional, cadence-hungry) or `0.9` (adventurous, distant modulations), and regenerate a few times at each.

<!-- video §1 -->

___

## 2. Diagnostic output (`gen'` and `gen''`)

Swap `gen` for `gen'` to see how the algorithm arrives at its choices: a bar-by-bar diagnostic showing the candidate pool, the selected chord and the top alternatives at each step.

```haskell
s <- seek "*" $ cue start $ len 4 $ entropy 0.5 $ gen'
```

Sample output (the opening of one run):

```
Generation: C ( tritone -> maj ) → 4 chords (ent 0.5)
Mode: online (composers: all)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  1: C ( tritone -> maj ) [starting state]

  2: C → C  [467G/660F]  pedal → C 7no5  [graph] γ=5
     Candidates: C maj | C min_1stInv | C 7no3 | C dim_2ndInv | C maj_2ndInv | C 7no5

  3: C → A  [639G/660F]  desc 3 → F# min_1stInv  [graph] γ=5
  …
```

The `Mode:` line says whether the graph was reached and which composers were drawn on. Then, field by field:
- **`1, 2, 3`**: bar number
- **`C → A`**: root movement (prior → posterior pitch class)
- **`[NG/MF]`**: candidate pool, N graph candidates and M fallback candidates
- **`desc 3`**: movement class (descending by 3 semitones; `pedal` keeps the root)
- **`F# min_1stInv`**: the chord selected at the posterior root (F# minor in first inversion, so A is in the bass)
- **`[graph]` / `[fallback]`**: which pool the selection came from
- **`γ=N`**: the index chosen by gamma-entropy sampling (lower is safer)
- **`Candidates`**: the top alternatives at the same posterior root

`gen''` adds verbose transform and advance traces: pitch-class arithmetic at every step, DB-stored vs computed functionality, and the full render pipeline.

**Overtone annotation.** When the context declares a tuning ([§4](#4-shape-the-generation-harmoniccontext)), `gen'` annotates each palette tone with its string and overtone sources. `{B: E2/G3}` means B is producible as string E overtone 2 or string G overtone 3. Overtones cover the playable harmonic domain: OT1 root, OT2 perfect fifth, OT3 major third. `"/"` separates alternative sources; `"+"` joins several node positions on one string.

Use `gen` for fast generation, `gen'` to understand selection, and `gen''` for debugging. All three have identical signatures and swap freely.

<!-- video §2 -->

___

## 3. Play the progression (`iK`, form, launcher)

The shortest route from a generated progression to sound. Uses `start` (§1) and `tempo` (§0).

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
- **`at time kinetics dynamics progression`** ([`Form.hs`](src/Harmonic/Interface/Tidal/Form.hs)) is a form node. With one node the signals are constant. (`at` has siblings: `at'` snaps, `rh`/`rh'` take bars; see [§13](#13-kinetics-form-programmed-arc).)
- **`iK tempo formNodes chordSelection`** bundles everything a launcher needs into one `IK` context.
- **`arrange (lo,hi) k (-9,9) T voicing modifier [patterns]`** ([`Bridge.hs`](src/Harmonic/Interface/Tidal/Bridge.hs)) maps patterns of voicing *degrees* across the progression. `(0,1)` is the kinetics gate, `(-9,9)` the register, and `T` the progression layer (triads; `S`/`M` arrive in [§19](#19-the-three-layers-tsm-genp--chordscale)).

`rep s 1` derives one chord per bar from the progression length. One cycle is one beat, so bar-length patterns carry `/4`. The `p01` launcher is reused through the rest of the guide.

**Try it.** Change `"[0 1 2 3]/4"` to `"[0 1 2 3 4 5 6 7]/8"` (double density) or `"[0 2 1 3]/4"` (a new contour). The whole launcher arrives prepared from the `launch` snippet (§22).

<!-- video §3 -->

___

## 4. Shape the generation (`HarmonicContext`)

Out of the box, generation is chromatic and unconstrained. The HarmonicContext modifiers shape what the generator can pick from: key, pitch palette, bass motion, required tones, inversions and the direction of tension. Uses `start` (§1).

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
| `hcOvertones` | `hcOvertones "E A D G"` | Pitch palette: each fundamental expands to its playable overtone set {root, P5, M3} |
| `hcKey` | `hcKey "0#"` | Key signature: `0#`=C, `1#`=G, `2b`=Bb |
| `hcRoots` | `hcRoots "C E G"` | Allowed bass notes |
| `hcPedal` | `hcPedal "C G?"` | Required tones; `?` = preferred |
| `consonant` / `dissonant` | `dissonant $ ...` | Drift direction across the progression |
| `invSkip` | `invSkip 2` | Min non-inversions between inversions |

Filter strings support the `"*"` wildcard, `"-Bb'"` subtraction, `"E'"` prime notation (exact pitch, no overtone expansion), the `"key"` / `"tones"` mirror keywords, and `"rise"` / `"fall"` for forced stepwise bass motion.

Compose modifiers right to left with `$`, and comment out individual lines to fall back to defaults. The whole context passes in through `tonal`.

**Try it.** Swap `consonant` for `dissonant`. Remove `hcPedal "C?"`. Add `hcOvertones "E A D G"` and watch `gen'` annotate every palette tone with its string and overtone sources; the pool narrows as the constraints tighten.

<!-- video §4 -->

___

## 5. The composer graph

The graph holds harmonic behaviour learned per composer. The `seek` string channels one composer's style, a weighted blend of several, the whole corpus, or none of it. Uses `start` (§1) and `ctx` (§4).

```haskell
s <- seek "bach"               $ cue start $ tonal ctx $ len 8 $ entropy 0.5 $ gen'
s <- seek "debussy"            $ cue start $ tonal ctx $ len 8 $ entropy 0.5 $ gen'
s <- seek "bach:25 debussy:75" $ cue start $ tonal ctx $ len 8 $ entropy 0.5 $ gen'
s <- seek "*"                  $ cue start $ tonal ctx $ len 8 $ entropy 0.5 $ gen'
s <- seek "none"               $ cue start $ tonal ctx $ len 8 $ entropy 0.5 $ gen'
```

Composer weight parsing lives in [`Query.hs`](src/Harmonic/Evaluation/Database/Query.hs). Blends print a portmanteau in the `gen'` header ("Bachdebussy"). Names are case-insensitive, and every composer key is listed in the [Composer Catalogue](documents/COMPOSERS.md).

`seek "none"` bypasses the graph entirely and generates from the built-in consonance fallback. Every classical example in this guide runs offline this way; what the corpus adds is style.

**Try it.** Compare Bach's cadential pull with Debussy's modal inflections on the same seed and context. Then run the same chain with `"none"` and hear what the corpus was contributing.

<!-- video §5 -->

___

## 6. Rank and select (`attempt`, `viability`)

Generate several candidate progressions and keep the best, instead of accepting the first. Uses `start` (§1) and `ctx` (§4).

```haskell
s <- seek "*" $ attempt 3 12 $ cue start $ tonal ctx $ len 8 $ entropy 0.6 $ gen

-- with the scoreboard:
s <- seek "*" $ attempt 3 12 $ cue start $ tonal ctx $ len 8 $ entropy 0.6 $ gen''

-- stricter / structural-only:
s <- seek "*" $ viability 0.7 $ attempt 5 24 $ cue start $ len 8 $ entropy 0.6 $ gen
s <- seek "*" $ viability 0.0 $ attempt 3 12 $ cue start $ len 8 $ entropy 0.6 $ gen
```

`attempt N K` produces up to `K` attempts, stops early once `N` *viable* ones are collected, and returns the single highest-scoring progression. Scoring ([`Scoring/Progression.hs`](src/Harmonic/Evaluation/Scoring/Progression.hs)) weights cadence favourability at half, with root motion and voice leading a quarter each; mode validity is a gate rather than a weight. `viability T` sets the floor an attempt must clear (default 0.5).

With `gen''`, a full scoreboard prints: one row per attempt with per-term scores, a viability marker, the truncated chord sequence, and `← PICK` on the winner. It shows what the evaluator rewards.

`attempt` wraps any generation chain (`gen`, `genE`, `genJ`, `genP`, `genFrom`) in the same way. `attempt 1 1` is a no-op.

**Try it.** Raise entropy to 0.9 and watch the scoreboard: more attempts fail viability and the spread widens.

<!-- video §6 -->

___

## 7. Voicing strategies

The same progression can sit vertically in very different ways. The voicing function distributes each chord's intervals across the register without changing the harmony. Uses `k` from the §3 launcher. Source: [`Arranger.hs`](src/Harmonic/Interface/Tidal/Arranger.hs).

| Function | Bass | Voice leading | Best for |
|---|---|---|---|
| `flow` | Any inversion | Smoothest (cyclic DP, globally optimal) | Pads, harmonic beds |
| `grid` | Root locked | Smooth upper voices | Grounded chords |
| `lite` | Any | None (raw intervals) | Direct control |
| `literal` | Any | None; register-normalised only | Hand-built material, as written |
| `root` | Root PC only | N/A | Bass lines, mono |
| `fund` | Harmonic fundamental | N/A | Sub bass, kick drums |

```haskell
arrange (0, 1) k (-9, 9) T flow (overlapF 0) ["~", "[0 1 2 3]/4"] # ch 01
arrange (0, 1) k (-9, 9) T grid (overlapF 0) ["~", "[0 1 2 3]/4"] # ch 01
arrange (0, 1) k (-9, 9) T root (overlapF 0) ["~", "[0]/1"]       # ch 01 |- oct 2
```

The voicing function is the fifth argument of `arrange`, after the layer; everything else stays identical. Stack several `arrange` calls with different voicings in one launcher to build a full texture.

Two things to know:
- **Mixed chord sizes voice-lead properly.** A triad moving into a 4-note chord (a `lead'` cue, or hand-built material) is costed by aligning the larger chord's outer and inner voices onto the smaller one, so the seam holds common tones instead of jumping register.
- **Scale layers are voiced on a lattice.** Through `arrange`, the S and M layers of `genP` and of chordscale-derived `gen`/`genJ` contexts, and any bar with six or more pitch classes, sit on a fixed pitch lattice grounded on bar 0: a held index keeps its pitch and inflects by accidental as the key moves, instead of transposing with each bar's root. Anything up to five voices gets the full cyclic voice-leading solve, including a pentatonic layer passed straight to `flow` outside `arrange`, so use the `arrange` layer argument when you want the lattice. Details in [`CHORDSCALE.md`](documents/CHORDSCALE.md).

**Try it.** Compare `flow` and `grid` on a long progression. Add a second `arrange` with `root` and an octave offset for a bass line.

<!-- video §7 -->

___

## 8. Chord selection (`rep` and `warp`)

You control which chord plays and when it changes. Uses `s` and `tempo`. Source: [`Bridge.hs`](src/Harmonic/Interface/Tidal/Bridge.hs).

```haskell
k = iK tempo [at 0 1 1 s] (rep s 1)               -- one chord per bar
k = iK tempo [at 0 1 1 s] (warp "[1 2 3 4]/4")    -- explicit, 1-indexed
k = iK tempo [at 0 1 1 s] (warp "[1 1 2 1]/4")    -- AABA-like cycle
k = iK tempo [at 0 1 1 s] (warp "[1 <2 3> 1 4]/4") -- Tidal operators
k = iK tempo [at 0 1 1 s] (warp "[1(3,8) 2]/4")   -- euclidean rhythm
```

The `/N` divisor maps directly to physical bars. Any TidalCycles operator (choice, probabilities, euclidean rhythms, nesting) applies, and indices wrap modulo the progression length.

**Try it.** `"[1 2 3 4 3 2 1 4]/4"` (8 events over 4 bars), `"[1 . [2 3]]/4"` (nested), `"[1 2 3 4]/8"` (half-speed harmonic rhythm).

<!-- video §8 -->

___

## 9. `arrange` vs `arrange'`

There are two ways a pattern can meet the progression's changes: it can flow across chords at its own speed, or compress into each chord slot. Uses `k` from the §3 launcher. Source: [`Bridge.hs`](src/Harmonic/Interface/Tidal/Bridge.hs).

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

`arrange` feels like a melody running over changing chords: each note's pitch maps to whichever chord is active at its onset. `arrange'` feels like an arpeggiator locked to each chord. Both sustain notes across harmony boundaries.

`overlapF N` expands each chord's pitch set with pitches from N bars ahead, for legato across transitions. `progOverlapB` / `progOverlap` ([§10](#10-progression-manipulation)) are the backward and bidirectional forms.

**Try it.** Same progression, same pattern, both functions. Then try `overlapF 2` and hear the sustain extend.

<!-- video §9 -->

___

## 10. Progression manipulation

Transform the harmony with pure functions. Each returns a new progression, so they chain freely. Uses `s`. Source: [`Arranger.hs`](src/Harmonic/Interface/Tidal/Arranger.hs).

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

**Chordscale staleness.** The bar-aligned combinators (`rotate`, `excerpt`, `reverse`, `expandP`, `interleave`, octave `transposeP`) carry each bar's derived S/M layers with it, but the key boundaries those layers encode come from the analysis of the progression before the edit. The bar-substituting edits (`insert`, `switch`, `clone`) mix chord sizes and drop the layers out of lattice voicing entirely. Re-run the analysis for a fresh reading:

```haskell
chordscale $ rotate 2 s
```

**Try it.** `expandP 2 s` slows the harmonic rhythm. `progOverlapF 1 s` gives a natural sustain. `insert (extract 5 sOther) 5 s` borrows a bar from another progression. `genGrid` in a seek chain skips traversal entirely and prints the raw candidate grid.

<!-- video §10 -->

___

## 11. Explicit composition

When you have specific changes in mind, skip the generator. Generated and explicit progressions are interchangeable everywhere. Source: [`Arranger.hs`](src/Harmonic/Interface/Tidal/Arranger.hs), [`Pitch.hs`](src/Harmonic/Rules/Types/Pitch.hs).

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

To keep a progression across sessions, `save "verse" s` ([`Recall.hs`](src/Harmonic/Interface/Tidal/Recall.hs)) prints it as a chart: one bar per line, with the T | S | M layers side by side, shaped as a complete `verse = recall …` statement. Pasted into the live file, that block binds the identical context next time. `recall` is pure and reads hand-written bars too.

**Try it.** Build your own 8-bar progression, apply `transposeP 2`, then `interleave` it with a generated one, so hand-written and generated harmony share one form. Then `save` a generation you like and paste the block back in.

<!-- video §11 -->

___

## 12. Regenerate in place (`genFrom`)

Keep the progression you like and regenerate only the bars you don't. Uses `start` (§1) and `ctx` (§4). Source: [`Builder.hs`](src/Harmonic/Framework/Builder.hs).

```haskell
s8  <- seek "*" $ cue start $ tonal ctx $ len 8 $ entropy 0.5 $ gen

s8' <- seek "*" $ entropy 0.5 $ genFrom s8 3 4          -- bars 3–4 only

s8'' <- seek "*" $ attempt 3 12 $ entropy 0.5 $ genFrom s8 3 4  -- ranked patch
```

`genFrom s a b` regenerates bars `a..b`. The range is 1-indexed and wraps: on a 5-bar progression, `genFrom s 4 2` walks bars 4, 5, 1, 2 and keeps 3. The cue is inferred from the bar before `a` so the new material connects; override it with `cue`. The printed grid is always the full spliced progression, the source with the new bars in place. On `gen`/`genJ` results the chordscale S/M layers are re-derived over the whole spliced progression, because key boundaries belong to the whole progression and a patch can move them.

The diagnostic variants mirror `gen`: `genFrom'` (standard trace) and `genFrom''` (verbose, with the scoreboard under `attempt`). It composes with the whole modifier chain.

**Try it.** Regenerate the final bars (`genFrom s8 7 8`); the seam back to bar 1 is handled, since progressions cycle. Then try a wrapping range (`genFrom s8 7 2`). A/B each patch against the original.

<!-- video §12 -->

___

## 13. Kinetics form (programmed arc)

Wall-clock time as a compositional parameter. A form is a list of nodes, each at a specific time, with kinetics (0–1 intensity) and dynamics (level). Between nodes the signals interpolate, and the progression switches at each node. Uses `start` (§1) and `tempo` (§0). Source: [`Form.hs`](src/Harmonic/Interface/Tidal/Form.hs).

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

The upper voice has the kinetics range `(0.7, 1)`, so it plays only during the climax in the middle. The pad, `(0, 1)`, plays throughout. Kinetics gate the `arrange` ranges, dynamics drive amplitude, and progression switches let calm and adventurous material occupy different parts of one piece.

**Snap transitions and bar-based nodes.** `at` has three siblings, varying two independent things:

| Constructor | Time unit | Transition |
|-------------|-----------|------------|
| `at`  | seconds | smooth (ramp) |
| `at'` | seconds | snap (hold, then jump) |
| `rh`  | bars (4/4) | smooth |
| `rh'` | bars | snap |

Mix them freely in one form. The `.tidal` guide's §13 also carries two form templates: `narrative`, a ~7m24s spectral arc of 10 nodes (scale the times to taste), and `popForm`, verse-chorus-bridge in bars.

**Try it.** The `dance` snippet (§22) expands a full symmetric arc to edit rather than build. Stretch `arcForm` to 5 minutes or compress it to 20 s. Change the upper gate to `(0, 0.3)` and it becomes the lead-in. In the `.tidal` guide's `cutForm`, flip `rh' 9` to `rh 9` and the hard cut becomes a ramp.

<!-- video §13 -->

___

## 14. Live kinetics & gating (`lK`, `ki`, `slate`)

Drive kinetics from a live signal instead of a keyframed form, so the arc becomes a control you perform. Uses `s`, `tempo` and `p01arc` (§13). Source: [`Form.hs`](src/Harmonic/Interface/Tidal/Form.hs).

```haskell
-- any Pattern Double (0–1) works as a kinetics/dynamics source:
do
  let kin = slow 64 $ lfo tri 0 1        -- a 64-beat triangle sweep
      k   = lK kin kin s (rep s 1)
  mapM_ id [hush, setbpm tempo, p01arc id k 0.85]
```

`lK kinSig dynSig s chordSel` builds an `IK` from two live signals: an LFO, a random walk, a MIDI CC. Everything downstream (range gating, dynamics) behaves exactly as with `iK`. (The Q-Link controller bridge in BootTidal, `qlink1..4`, `xyX`/`xyY` and `exP`, supplies hardware CC signals on this interface, and the LED display feed `display k` is likewise rig-specific. Both are pointers, outside this walkthrough.)

The range gating that `arrange` applies is available on any pattern:

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

**Try it.** The `slate` snippet (§22) inserts one gated layer. Replace the LFO with `slow 16 $ lfo sine 0 1` or `segment 8 $ perlin`. `withForm k f` passes the active progression to a custom pattern function as the form moves.

<!-- video §14 -->

___

## 15. Groove (`subKick`)

Kick and sub bass that follow the harmonic root. `subKick` combines a kick pattern, a sustained sub and an MPC-style CC64 sustain mechanism, locked to the root of whichever chord is active at each onset. Uses `s`, `tempo` and `p01` (§3). Source: [`Groove.hs`](src/Harmonic/Interface/Tidal/Groove.hs).

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

It routes to MIDI channel 10. A companion MPC kit program renders the full-range sub and kick, but any drum sound on channel 10 works for the walkthrough.

`subKick'` takes a list in each of the four slots and walks every list across the kinetics range on its own grid (N items, N equal windows, as `kinPick` does for `lineHarmony`), so a held drone, a sparse euclidean groove and a busy peak pattern are one block.

**Try it.** Push the kick (`"[1 0 0 1]"`), or swap `root` for `fund` so the sub follows the harmonic fundamental instead of the chord root, which keeps the low end steady through inversions.

<!-- video §15 -->

___

## 16. Walking lines (`walk` / `lineHarmony`)

A walking bass line built from the progression. The strong beats carry consonant anchors (the root on 1; the fifth, root or a third on 3). Beats 2 and 4 are weighted connectors that prefer sandwich motion and approach tones, with a soft bias towards keeping the line's direction from bar to bar. Uses `s` and `tempo`. Source: [`LineHarmony.hs`](src/Harmonic/Interface/Tidal/LineHarmony.hs), [`WalkingBass.hs`](src/Harmonic/Traversal/WalkingBass.hs), [`KeyArea.hs`](src/Harmonic/Evaluation/Analysis/KeyArea.hs).

The line follows the bars in the order they're performed. `warp` and `rep` selections are resolved as the pattern plays, a repeated bar walks root to fifth, and approach tones aim at whichever bar actually comes next. A selection that doesn't repeat cleanly falls back to stored order with a printed notice. Connector tones come from the same key-area analysis as the chordscale layers (§19), so the bass walks the sets those layers display; when a selection reorders the bars, the walk re-analyses what's actually played and adds chromatic approach tones.

The `walk` snippet (§22) expands the block below.

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

The same walk over a jazz progression reads the chart rather than the raw tone set. Each chord quality has its own bass vocabulary: the fifth a 13th chord omits is restored, an altered dominant walks its ♯5 in place of the natural 5, and the notated colours (♭9, ♯9, ♯11, ♭13) stay off the strong beats while remaining available as weak-beat tension:

```haskell
sJ <- seek "*" $ len 8 $ entropy 0.4 $ genJ
do
  let k = iK tempo [at 0 1 1 sJ] (rep sJ 1)
  mapM_ id [hush, setbpm tempo, walk id k 0.9]
```

Nothing in the block changes; the family the progression came from is what changes the line. `genP` walks its strata chroma, and for octatripentatonic progressions ([§19](#19-the-three-layers-tsm-genp--chordscale)) the connector pool reweights towards the 5-PC strata set. `genE` walks its foundation (T) layer, the one that owns the bass.

How the patterns and arguments behave:
- **The patterns are beat positions** (which beats of each bar the line sounds on, 1-indexed), not voicing degrees. Sparser feels compose like any Tidal pattern: `"[1 3]/4"` (two-feel), `"[1 2 ~ 4]/4"`, `"[1 [2 2] 3 4]/4"` (eighth-note fill).
- **The list is a kinetics ladder.** A list of N patterns splits the kinetics signal into N windows and plays only the pattern whose window holds the current value, so a leading `"~"` silences the low-kinetics half.
- **`d` is applied once, inside `lineHarmony`.** Don't also add `|* vel d` outside, which squares the dynamic.
- **Dynamics steer the walk.** Bars quieter than the piece's own mean push the register up, louder bars push it down, and a sudden drop (0.25 or more between bars) resets the line to its lowest note. Only dynamics known when the pattern is evaluated count: the `d` argument and form-node dynamics. Live control signals and a downstream `|* vel` don't affect the walk.
- **The register is a fixed 21-semitone double-bass range** (E1–C3 at the default patch; the absolute octave depends on the synth via `tidalNoteOffset`). `voiceFn` (`root` or `fund`) anchors beat 1.

**Try it.** Mute the pad and listen to the line alone: the strong beats carry the changes by themselves.

<!-- video §16 -->

___

## 17. Voice lines & the instrument paradigm

Address stacked voices as independent patterns, each with its own kinetics range, voicing function and register. The virtual orchestra is built on this. Uses `k` from the §3 launcher.

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

Four voices, soprano, alto, tenor and bass, are each an `arrange` call with a different degree and octave offset. The `orchsec` snippet (§22) expands this shape, and `instr` inserts a single line into an existing stack.

The layer argument (the fourth) is where the other generation families come in. On a `genP` context the `S` layer is that bar's five-note strata and `M` its seven-note mode, so the same call becomes pentatonic or modal material over the same harmony, in degrees rather than chord tones:

```haskell
sP <- seek "*" $ len 8 $ entropy 0.4 $ genVI
do
  let k = iK tempo [at 0 1 1 sP] (rep sP 1)
  mapM_ id [hush, setbpm tempo
    , (d01 $ arrange (0, 1) k (-9, 9) T grid (overlapF 0) ["~", "[0]/1"]       # ch 01 |- oct 1 |* vel 0.8)
    , (d02 $ arrange (0, 1) k (-9, 9) S flow (overlapF 0) ["~", "[0 2 4 6]/2"] # ch 02 |+ oct 1 |* vel 0.5)
    ]
```

Swap `S` for `M` for the mode, or `TS`/`TSM` for the unions. Every layer works on every family (§19–§21).

### Divisi

When a voice splits into desks, each desk is quieter than the undivided line. Divisi handles the split and the equal-power (`1/√n`) compensation. Each voice carries three tiers in one `voiceLines` record (`soprano`, `soprano'`, `soprano''`), all ordinary degree patterns; unset tiers default to one and two degrees up.

```haskell
vl = voiceLines {_vl = "~"
  , soprano  = "[3 2 1 0]/4"    -- desk 1
  , soprano' = "[5 4 3 2]/4"    -- desk 2
  }
-- hand-built desks, tagged:
, arrange (0,1) k (-9,9) T flow (overlapF 0) ["~", vlGet Soprano  vl] # o # divisi2
, arrange (0,1) k (-9,9) T flow (overlapF 0) ["~", vlGet Soprano' vl] # o # divisi2 |+ oct 1
```

With the orchestral instrument functions the same split is one line, `divisi 2 violin1 T (0, 1) k vl grid Soprano`, and primed voices (`Soprano'`, `Bass8vb'`) pick desks by hand. The full system (16 instruments, 14 pitched and 2 unpitched percussion, with articulations, section blocks and timbral blends) is in [`ALGORITHMIC_ORCHESTRATION.md`](documents/ALGORITHMIC_ORCHESTRATION.md). It depends on a specific MIDI rig, so this guide stops at the principle.

<!-- video §17 -->

___

## 18. Motifs (motivic development)

Name the recurring material a piece is built from, and develop it with the classic tools instead of retyping strings. Uses `k` from the §3 launcher.

Source: `Harmonic.Interface.Tidal.Motif` (`>:<`, `mirror`, `retro`) and `.Groove` (the clave library). Motifs are plain patterns: a `Pattern Bool` rhythm and a `Pattern Int` contour. The contour is in voicing degrees, realised against the harmony by `arrange`, so it tracks the chords. One statement binds all three names:

```haskell
(rhythm, contour, motif) =
  ( son32                 -- rhythm gate (a son clave)
  , "[3 2 1 0]/4"         -- contour (a descending figure)
  , rhythm >:< contour
  ) :: (Pattern Bool, Pattern Int, Pattern Int)
```

Primed tiers (`rhythm'`, `contour''`, …) fall back to BootTidal defaults. Blocks read a contour as a degree pattern, gate a voice or drum with it (`struct rhythm $ …`), or use the pre-combined `motif`.

| Development | Tool | Example |
|---|---|---|
| Retrograde | `retro` / `retroN` | `retro contour` |
| Inversion | `mirror axis` | `mirror 3 contour` |
| Augmentation / diminution | `slow` / `fast` | `slow 2 motif` |
| Transposition | `\|+` / `\|-` | `contour \|+ 2` |
| Rotation | `<~` / `~>` | `"<0 1>" <~ rhythm` |
| Combination | `struct` / `>:<` | `rhythm >:< contour` |

**Tidal's transforms work on the cycle, not the bar.** One cycle is one beat, so a bar-length figure carries `/4` and spans four cycles. Tidal's `rev` then reverses each beat on its own and leaves the phrase intact, which reads as though it did nothing. `retro` is provided for this: it reverses the whole bar, and `retroN n` reverses over `n` cycles for longer or shorter phrases.

```tidal
rev    contour   -- each beat reversed; phrase order unchanged
retro  contour   -- true phrase retrograde  (retro . retro = id)
retroN 8 motif   -- retrograde a two-bar phrase
```

Rotation has the same trap. `~>` shifts in cycles, so shifting a figure by a multiple of its own period does nothing: rotating `"[3 2 0 1]*4"` (period ¼ cycle) by ¼ lands back on itself. Rotate bar-scoped material instead: `0.5 ~> contour` gives a canon at the half-beat.

When checking a development, note that `queryArc` returns events in query order, not time order. Inspect with the start time attached, or sort first; otherwise a perfectly good `rev` reads as a no-op:

```tidal
[ (wholeStart e, value e) | e <- queryArc (rev contour) (Arc 0 1) ]
```

Because a contour is in voicing degrees, it re-realises against whatever chord is sounding. To restate an idea at a new pitch, transpose the harmony beneath a fixed contour. `mirror axis d = 2*axis - d`.

**Try it.** The `motif` snippet (§22) expands the panel with its type annotation. Re-execute it with `contour = "[0 1 2 3]/4"` or `rhythm = bossa32` and the whole piece changes in one step. Claves: `son32`, `rumba32`, `bossa32`, `bellpat32` (and their 2-3 rotations: `son23` …).

<!-- video §18 -->

___

## 19. The three layers (T/S/M): `genP` & chordscale

Every progression carries three bar-aligned layers, and each pattern chooses which one to read. Uses `start` (§1).

| Layer | genP content | gen / genJ content | genE content | Practice analogue |
|---|---|---|---|---|
| `T` | the triads (3 PCs) | the chords | the foundation triads | chord playing |
| `S` | a pentatonic per bar (5 PCs) | the best-fit pentatonic per bar (5 PCs) | the less dissonant partner triad | pattern playing |
| `M` | a diatonic mode per bar (7 PCs) | the detected key as the mode on the bar root (7 PCs) | the more dissonant partner triad | scale/modal playing |
| `TS` `TM` `SM` | layer unions | layer unions | 4-note pair structures | comping voicings |
| `TSM` | full union | full union (= M) | the 5-tone pentad | tutti verticals |
| `PT` | (the triad) | the chord tones the pentatonic keeps | the pivot tones all three layers share | pedal/anchor lines |

Every selector works on every context. Combined selectors are bar-by-bar unions, rooted on the lowest layer in them (T before S before M), so the foundation owns the merged bass.

How each family fills the layers:
- **`genP`** walks eleven canonical pentatonic strata (I–XI), grouped into twelve curated tristrata whose pairwise unions are diatonic modes. Full theory: [`OCTATRIPENTATONICS.md`](documents/OCTATRIPENTATONICS.md).
- **`genE`** fills S and M with two independent partner triad chains (§20, [`POLYTONAL.md`](documents/POLYTONAL.md)).
- **`gen` and `genJ`** derive the layers by chordscale analysis. The progression is divided into key areas (major, or composite minor, with the natural, harmonic or melodic form chosen per bar). M is the key expressed as a mode on each bar's root. S is the pentatonic that best covers the bar's guide tones with the fewest changes across the progression, leaving the key only where the harmony needs it. The walking bass (§16) uses the same analysis.

`chordscale` applies the analysis by hand to `lead'`-built contexts. `chordscaleReport` prints the key, form, mode and pentatonic per bar, flagging boundaries (`<`), overrides (`!`) and out-of-key choices (`*`).

Source: [`Builder.hs`](src/Harmonic/Framework/Builder.hs), [`Strata.hs`](src/Harmonic/Framework/Builder/Strata.hs), [`KeyArea.hs`](src/Harmonic/Evaluation/Analysis/KeyArea.hs), [`ChordscaleT.hs`](src/Harmonic/Interface/Tidal/ChordscaleT.hs). Full theory: [`CHORDSCALE.md`](documents/CHORDSCALE.md).

```haskell
-- 33 aliases: genI..genXI × {plain, ', ''}
pc19 <- seek "*" $ cue start $ len 8 $ entropy 0.5 $ genVI'

-- the layer argument of arrange selects the density:
arrange (0,1) k (-9,9) T flow (overlapF 0) ["~", "[0 1 2 3]/4"]   -- triads
arrange (0,1) k (-9,9) S flow (overlapF 0) ["~", "[0 1 2 3]/4"]   -- strata
arrange (0,1) k (-9,9) M flow (overlapF 0) ["~", "[0 1 2 3]/4"]   -- modes
```

`genP'` prints a block per bar: the triad selection, the active strata (5 PCs), the classified mode (7 PCs) and the tristrata identity, all in one spelling system. For `gen` / `genJ` contexts the analysis view is:

```haskell
pcCS <- seek "*" $ cue start $ len 8 $ entropy 0.5 $ gen
chordscaleReport pcCS               -- key | form | mode | pentatonic per bar
```

**Steering the walk:**

```haskell
hcTristrata "5" hContext            -- lock to tristrata #5 (IV-VI-X)
relStrata "1 1 2 2 3 3" $ genVI'    -- per-bar position in the active tristrata
absStrata "I V X" $ genI'           -- per-bar absolute strata labels
genPReport pc                       -- provenance + triads in one report
```

**Continuity boosts.** The walk prefers staying put. Three multipliers bias it (defaults 0.90, 0.80 and 0.70; `1.0` disables each): `sameBoost` (same strata as the last bar), `flipBoost` (same as two bars ago) and `triBoost` (same tristrata).

`genP` picks its own starting chord from inside the stratum it names, so `genI`..`genXI` work uncued. Pass `cue` when you want a particular way in; a chord that falls outside the stratum is reported along with the triads that would have fitted, rather than silently swapped.

**Try it.** `S`-layer patterns under a soloist make pentatonic practice over moving harmony. `attempt 3 12 $ … $ genVI''` ranks whole walks. The walking bass (§16) reweights automatically over `genP` progressions.

<!-- video §19 -->

___

## 20. Polytonal generation (`genE`)

Three simultaneous triad progressions from one walk: a foundation plus two partner chains that share tones with it bar by bar, so any pair of layers sounds a 4-note structure and all three sound a 5-tone pentad.

The T layer is a foundation walk identical to `gen`. Every R constraint (key, roots, rise/fall, drift, pedal) binds the foundation alone, because it owns the bass. S and M are partner triad chains, each a corpus-valid walk of its own: every partner bar continues from its own chain's previous bar, and must share exactly two pitch classes with that bar's foundation triad, for a five-note union. That allows two geometries, and the walk chooses between them bar by bar:
- **Common-dyad**: all three triads share one dyad, so every pair of layers sounds four tones.
- **Base-anchored**: the partners take different dyads of the foundation, so T+S and T+M sound four tones and S+M sounds the full pentad.

Partners respect the harmonic space (`hcKey`, `hcRoots` note sets, `tonal`) but ignore direction specs and drift, so they stay free to diverge. S is the chain with the lower total dissonance across the run; M is the other.

```haskell
start <- lead "C maj"
s <- seek "*" $ cue start $ len 8 $ entropy 0.3 $ genE'   -- genE / genE' / genE''
```

Cues are triadic: a 4-note cue is refused, because the combined structures come from the layers. The unprimed output prints the foundation grid plus the combined `TSM` grid; `genE'` adds a table per bar (partners, pair structures, pentad, pivot tones), and `genE''` adds the selection facts (geometry, pool tier and size, rank in its own list). At the REPL, `genEReport s` prints every layer view a pattern can select.

Two things work differently from the other three families:
- **Every layer is voiced as harmony.** The caller's `flow`/`grid` solves every layer, the `SM` and `TSM` pentads included. A genE pentad is three stacked triads sounding at once, a polytonal sonority rather than a scale, so vertical voice leading is the right treatment. The pentatonic and modal layers of `gen`/`genP`/`genJ` use the lattice instead (§7, §19).
- **`attempt` ranks all three layers**: half the weight on the foundation and a quarter on each partner, plus a divergence score for how often the partners take the base-anchored geometry and how widely the three roots spread. Divergence is what this family is for, so it competes with quality rather than breaking ties, and raising K raises both. `genE''` shows the trade-off in its own scoreboard columns.

```haskell
arrange (0,1) k (-9,9) T   flow (overlapF 0) ["~", "[0 1 2 3]/4"]  -- foundation alone
arrange (0,1) k (-9,9) S   flow (overlapF 0) ["~", "[0 1 2 3]/4"]  -- a partner alone (its own roots)
arrange (0,1) k (-9,9) TS  flow (overlapF 0) ["~", "[0 1 2 3]/4"]  -- 4-note pair
arrange (0,1) k (-9,9) TSM flow (overlapF 0) ["~", "[0 1 2 3]/4"]  -- the 5-tone pentad
arrange (0,1) k (-9,9) PT  flow (overlapF 0) ["~", "[0 1]/4"]      -- the pivot tones
```

A layer used alone keeps its own roots and bass; T owns the bass only inside merged selectors. Everything composes as usual, including regeneration:

```haskell
s  <- seek "*" $ attempt 3 12 $ entropy 0.4 $ genE''      -- rank-and-select across all three layers
s' <- seek "*" $ entropy 0.3 $ genFrom s 3 5              -- family-aware: partner chains regen too, seeded from the kept bars
```

Under a very restrictive context the foundation behaves exactly like `gen` (at worst it repeats). Partner selection can't fail: it relaxes from the corpus lists to a plain enumeration that always holds valid pairs. Full theory and the study behind the design: [`POLYTONAL.md`](documents/POLYTONAL.md).

For an explicit 4-note opening in hand-built material there is still `lead'`, which takes note names directly (the first note is the root and bass, typed accidentals drive spelling, and an optional `(N)` movement works as in `lead`). Such material plays and walks as written; regenerating it produces triads, with a printed notice:

```haskell
start <- lead' "Eb Gb Bb Db"        -- Eb m7, printed and used as-is
start <- lead' "A C E G (5)"        -- A m7, ascending-5th approach
```

**Try it.** Generate once, then play `T` on one instrument, `S` on another and `M` on a third: three independent progressions that lock into pentads. Swap a part to `TS` for comping density, and put the walking bass (§16) on `T`.

<!-- video §20 -->

___

## 21. Jazz generation (`genJ`)

A second corpus and a second harmonic language: leadsheet changes (m7, 7#9, 13b9#11, slash chords) walked with the same modifier chain, trained on the Jazz-Chord-Progressions-Corpus (Bunks/Weyde/Dixon/Di Giorgi, ISMIR 2023; 2,614 tunes).

`genJ` is a generator family of its own. The jazz graph (`:Change`) lives beside the classical one in the same database. Its nodes are anchor-relative chord sets at corpus arity (3–6 tones), named by a jazz-specific namer, and there is deliberately no consonance fallback: the corpus is rich enough that the graph is the pool (measured: typically ~219 candidates per step, with no dead ends). Cue with `leadJ`, which parses leadsheet symbols in their own quality namespace and leaves the triadic `lead`/`lead'` untouched. A notated slash bass is honoured exactly, joined to the chord and made the anchor:

```haskell
start <- leadJ "Cm7"                -- corpus workhorse
start <- leadJ "Dm7/G"              -- G anchor, full Dm7 above (a 9sus4 set)
s <- seek "*" $ cue start $ len 8 $ entropy 0.3 $ genJ'    -- genJ / genJ' / genJ''
```

Jazz progressions play through every interface unchanged, and the walking bass (§16) reads each chord symbol's bass vocabulary, so `walk` over `genJ` changes sounds like a bassist reading the chart rather than the voicing.

One `seek` spec drives both corpora. Jazz composer names blend by corpus weights and match by substring, so `"monk:60 coltrane:40"` finds `theloniousmonk` and the co-credits. A classical name doesn't walk the jazz graph; it steers it. At each step, the current chord's most consonant embedded triad asks the classical graph what that composer would do next, and jazz candidates containing a recommended triad (with the same movement) get a boost. Both can combine, and `steer` sets the boost strength:

```haskell
s <- seek "theloniousmonk:60 johncoltrane:40" $ cue start $ len 8 $ genJ'
s <- seek "debussy" $ steer 8 $ cue start $ len 8 $ genJ'   -- jazz walk, Debussy pull
s <- seek "monk debussy" $ cue start $ len 8 $ genJ'        -- blend AND steer
```

`seek "none"` is refused, because the jazz graph has no offline mode. Everything else composes as usual. `tonal` runs the same R filters over the jazz candidates (key of C pins the walk to diatonic corpus vocabulary: Am7, Dm7, G7sus4, CM7…), `genFrom` regenerates ranges as jazz (the output is stamped as the jazz family), and `attempt` ranks against the jazz graph's own cadence statistics:

```haskell
s  <- seek "*" $ cue start $ tonal (hcKey "0#" hContext) $ len 8 $ genJ'
s' <- seek "*" $ entropy 0.6 $ genFrom s 3 4
s2 <- seek "*" $ attempt 3 8 $ cue start $ len 8 $ genJ''   -- scoreboard
```

The output is an ordinary `ProgressionContext`: `flow`/`grid` voice the 3–6-tone chords, and `arrange` and the walking bass play them unchanged. It carries chordscale S/M layers like every `gen` result (`chordscaleReport s` prints the detected keys, modes and pentatonics; §19).

**Try it.** The guided tour with live database probes is [`live/docs/JAZZ_DIAGNOSTIC.tidal`](live/docs/JAZZ_DIAGNOSTIC.tidal); work down it section by section.

<!-- video §21 -->

___

## 22. Snippets & the liveness gradient

Choose deliberately how much of a performance is prepared and how much is made at the keyboard.

Source: [`live/system/pulsar/snippets.cson`](live/system/pulsar/snippets.cson), for Pulsar with the TidalCycles grammar: type a prefix, press Tab, fill the tab-stops. There are three working modes on a gradient:

- **Prepared state file**: a complete launch block evaluated in one gesture. Instant sound, with everything decided in advance.
- **Snippet expansion**: the structure arrives from the library, and the decisions (key, composer, entropy, patterns) are made live at the tab-stops.
- **Blank page**: everything typed. Slowest to music, with every decision visible and live.

None is more correct than another; each trades immediacy against how much is decided in advance. A performance can move along the gradient: open from a state file, develop by snippet, end on a blank page.

**What earns a prefix.** A snippet prepares a repeated, fiddly path of syntax around a musical action, so the hands reach for music rather than boilerplate. It isn't one block per feature. Fewer prefixes are better, because there's less to recall mid-performance. Anything short and memorable lives here in the guide instead: switching generation family is four characters (`$ gen` → `$ genJ`), and the verbosity primes are one. If you'd type a line faster than you'd remember its prefix, it shouldn't be a snippet.

**The prefix index:**

| Group | Prefixes |
|---|---|
| Session | `transport` · `state` |
| Generation | `ctx` · `gen` · `formless` · `dance` |
| Launchers | `launch` · `p` · `rr` · `slate` · `cc` · `mark` · `harm` · `minimal` · `deeptech` |
| Groove & lines | `subk` · `walk` |
| Motifs | `motif` |
| Orchestra* | `orchsec` · `tutti` · `instr` |
| Hardware* | `k909` · `kmpc` · `m32` · `mdf` · `rc10` · `dkit` |
| Studio rig* | `grnd` · `mini` · `jura` · `hmnx` · `omap` · `slce` · `kgrv` · `s101` · `smpl` · `auto` · `gnlr` |

(\* rig-specific; see Known exceptions.)

The `state` snippet is the standard prepared opening: tempo, context, a generated progression under `attempt`, a one-node form, and a launcher with commented instrument lines. It's the same shape this guide builds by hand from §3 on.

**Try it.** Rebuild §3's launcher from memory on a blank page, timing yourself. Then do it from the `state` snippet, and compare.

<!-- video §22 -->

___

## 23. Going further

**Instrument catalogue**
- [`live/docs/ORCHESTRAL_CATALOGUE.tidal`](live/docs/ORCHESTRAL_CATALOGUE.tidal): range tests per instrument

**Documentation**
- [`COMPOSERS.md`](documents/COMPOSERS.md): the corpora and every composer key
- [`CHORDSCALE.md`](documents/CHORDSCALE.md): key areas and the chordscale layers
- [`OCTATRIPENTATONICS.md`](documents/OCTATRIPENTATONICS.md): strata and tristrata reference
- [`POLYTONAL.md`](documents/POLYTONAL.md): the polytonal genE framework
- [`ALGORITHMIC_ORCHESTRATION.md`](documents/ALGORITHMIC_ORCHESTRATION.md): the virtual orchestra
- [`LIVE_ENVIRONMENT.md`](documents/LIVE_ENVIRONMENT.md): the studio and stage rig
- [`ARCHITECTURE.md`](documents/ARCHITECTURE.md): the R→E→T pipeline, four-layer architecture and graph schema
- [`CHANGELOG.md`](CHANGELOG.md): V3 feature summary

**Jazz**
- [`live/docs/JAZZ_DIAGNOSTIC.tidal`](live/docs/JAZZ_DIAGNOSTIC.tidal): a guided tour of the jazz graph with live database probes

**Drum patterns**
- [`live/drumpats/`](live/drumpats/): a genre pattern library (kick/snare/hh helpers in `Harmonic.Interface.Tidal.Devices.JV1010`)

### Quick reference

**Generation**

```haskell
start <- lead "C maj"                                        -- or initCadenceState
s <- seek "*" $ cue start $ len 4 $ entropy 0.5 $ gen        -- corpus aggregate
s <- seek "bach:30 debussy:70" $ ... $ gen'                  -- blend + diagnostics
s <- seek "none" $ ... $ gen                                 -- offline fallback
```

**Rank & select**: `attempt N K` (best of K, stop at N viable), `viability T` (floor, default 0.5); scoreboard under `gen''`. Polytonal walks rank on all three layers plus divergence (§20).

**Regenerate in place**: `genFrom s a b` (wrap-aware, cue inferred), `genFrom'`/`genFrom''`.

**Strata-first (T/S/M)**: `genI..genXI`, one alias per stratum, each in the usual three verbosities (bare silent, `'` standard, `''` verbose), so `genIII''` is the verbose walk from stratum III; `hcTristrata "5"`, `relStrata "1 2 3"`, `absStrata "I V X"`, `sameBoost`/`flipBoost`/`triBoost N`, `genPReport`.

**Polytonal (genE)**: `genE`/`genE'`/`genE''`, a foundation walk plus two partner triad chains; the layer selectors `TS`/`TM`/`SM`/`TSM`/`PT` give the pairs, the pentad and the pivot tones; `genEReport s` prints every layer view.

**Chordscale (gen / genJ)**: automatic S (pentatonic) and M (mode) layers from key-area analysis; `chordscale` opts hand-built contexts in; `chordscaleReport s` prints key / form / mode / pentatonic per bar.

**Context modifiers**: compose with `$`, right to left: `hContext`, `hcOvertones`, `hcKey`, `hcRoots`, `hcPedal`, `consonant`, `dissonant`, `invSkip`, `hcTristrata`.

**Voicing**: `flow`, `grid`, `lite`, `literal`, `root`, `fund`; `voiceRange` clips a line to an instrument's compass.

**Chord selection**: `rep s 1`, `warp "[1 2 3 4]/4"`.

**Pattern helpers** (`Harmonic.Interface.Tidal.Utils`): `swing8`/`swing16` (swing by proportion: `0.5` straight, `0.667` triplet), `humanise n` (velocity jitter, combine with `|+`), `oct n`, `pullBy`/`pushBy` (rotate earlier/later), `binaryrange lo hi` (a wandering boolean gate), `over`/`-->` (step a list under a 0–1 control), and the note-length constants `hemidemisemiquaver` … `minim`.

**Claves** (`Harmonic.Interface.Tidal.Groove`): `son32`, `rumba32`, `bossa32`, `bellpat32` and their 2-3 rotations `son23`, `rumba23`, `bossa23`, `bellpat23`; `noteoff` releases a held sub. Use with `struct` or `mask`.

**Melody source**: `melodyStateFrom` builds a starting state from a `ScaleSource`, for lines that follow a scale rather than the corpus.

**Arrangement**

```haskell
arrange  (lo,hi) k (-9,9) LAYER voicing modifier [patterns] # ch N   -- LAYER = T | S | M | TS | TM | SM | TSM | PT
arrange' (lo,hi) k (-9,9) LAYER voicing modifier [patterns] # ch N   -- squeeze variant
```

**Form**: `at time kinetics dynamics progression`; siblings `at'` (seconds, snap), `rh` (bars, smooth), `rh'` (bars, snap). `iK tempo [nodes] chordSel`; live: `lK kinSig dynSig s chordSel`; gating: `ki (lo,hi) k pat`, `slate (lo,hi) k pats`, `withForm k f`.

**Groove & lines**: `subKick d k root (maxDur, subOn, subOff, kickPat)` (ch 10), and `subKick'` with a list in each slot, walked across the kinetics range; `lineHarmony d k root ["~", "[1 2 3 4]/4"]` (walking bass, beat positions).

**Divisi**: `divisi N instr T (lo,hi) k vl voicing Voice`; primed voices `Soprano'`/`Bass8vb'` pick a desk; `# divisi2`/`# divisi3` volume tags.

**Motifs**: `(rhythm, contour, motif) = (…, …, rhythm >:< contour)`; develop with `retro`, `mirror axis`, `slow`/`fast`, `|+`, `<~`, `struct`/`>:<`. Claves: `son32`, `rumba32`, `bossa32`, `bellpat32` (and their 2-3 rotations).

**Manipulation**: `rotate`, `excerpt`, `transposeP`, `reverse`, `fuse`, `fuse2`, `interleave`, `expandP`, `insert`, `switch`, `clone`, `extract`, `progOverlap`, `progOverlapF`, `progOverlapB`.

**Explicit**: `fromChords [[0,4,7], …]`; `prog (notesToPCs <$> [[C,E,G], …])`; `fromCadenceStates [initCadenceState mov "Root" [ints], …]`; `save "name" s` prints a context as a `name = recall …` chart to paste back.

**Snippets (Pulsar)**: the full index is in §22.

___

Questions and feedback via the [GitHub Issues](https://github.com/OscarSouth/theHarmonicAlgorithm/issues) tracker.
