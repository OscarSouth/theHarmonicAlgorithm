# The Harmonic Algorithm — User Guide

This is the readable companion to [`live/USER_GUIDE.tidal`](live/USER_GUIDE.tidal). If you'd rather work through this interactively — evaluating each block and hearing the results as you go — open the `.tidal` file in your TidalCycles editor and scroll from top to bottom. The two files present the same material in the same order.

### MIDI output

This system outputs MIDI on TidalCycles channels 1–16 (`d01`–`d16`). **Everything in this guide transmits on MIDI channel 1** (`d01`). Route channel 1 to a polyphonic sound — piano, Rhodes, pad, polysynth — in your MIDI setup. That's your instrument for the whole walkthrough. If you're running SuperDirt with a MIDI bridge, `ch 01` routes to channel 1 on whatever output is bound to the `"thru"` device.

### Offline-first

Every executable block here runs without Neo4j. Generation uses the consonance fallback (`seek "none"`). Graph-enhanced lines are shown as comments — uncomment them once you've run `docker compose up -d neo4j` to hear the same structure shaped by a composer corpus of 80+.

### Setup

- [Haskell Stack](https://docs.haskellstack.org/en/latest/install_and_upgrade/)
- [Docker](https://www.docker.com/) (optional — only for composer corpus examples)
- [TidalCycles](https://tidalcycles.org/) with SuperCollider + SuperDirt
- `live/BootTidal.hs` loaded with `Harmonic.Lib`

```bash
stack build                       # compile the library
docker compose up -d neo4j        # optional — graph backend
stack run                         # optional — populate the graph
```

See [`live/BootTidal.hs`](live/BootTidal.hs) for all available helpers (`ch`, `vel`, `oct`, the `d01`–`d16` MIDI streams, `hush`/`panic`, the drum patterns).

___

## 1. First generation (offline)

**Why** — before we make any sound, let's see what the generator actually does. The simplest possible invocation produces a 4-chord progression from a starting chord, shaped only by entropy.

**What** —

```haskell
tempo = 90

start <- lead "C maj"

s <- seek "none" $ cue start $ len 4 $ entropy 0.5 $ gen

print s
```

`lead` ([`Arranger.hs:506`](src/Harmonic/Interface/Tidal/Arranger.hs#L506)) parses a readable string: `"C maj"`, `"E min"`, `"Bb 7 (7)"` — root, quality, optional ascending movement. Use `initCadenceState 0 "C" [0,4,7]` if you need explicit interval control.

`seek "none"` ([`Builder.hs:702`](src/Harmonic/Framework/Builder.hs#L702)) bypasses the graph entirely — fully musical generation using only the consonance fallback. The composer string `"*"`, `"bach"`, `"debussy"`, `"bach:25 debussy:75"` all require Neo4j; `"none"` does not.

**How** — `lead` returns `IO CadenceState`, so bind with `<-`. The `seek` terminal runs the whole modifier chain. `cue` sets the starting state, `len` sets the chord count, `entropy` (0.0–1.0+) dials between the familiar and the surprising.

**Try it** — change `"C maj"` to `"E min"` or `"Bb 7 (7)"`. Swap `entropy 0.5` for `0.2` (conservative) or `0.9` (adventurous). Run `print s` to see the chord names.

<!-- [video ~15s: execute the 4 lines above in the TidalCycles editor. The progression prints to the terminal — show the `Generation: C maj → 4 chords` header and the chord list, then change "C maj" to "E min" and re-run to show a different progression] -->

___

## 2. Diagnostic output (`gen'` and `gen''`)

**Why** — to understand *how* the algorithm is arriving at its choices. Swap `gen` for `gen'` and you get a bar-by-bar diagnostic showing the candidate pool, the selected chord, and the top alternatives at each step.

**What** —

```haskell
s <- seek "none" $ cue start $ len 4 $ entropy 0.5 $ gen'
```

Sample output (annotated):

```
Generation: C maj → 4 chords (entropy 0.5)
Mode: offline (fallback only — no graph)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  1: C maj [starting state]

  2: C → G  [0G/660F]  asc 7 → G maj  [fallback] γ=12
     Candidates: G maj | G dom7 | G min | E min | B min | B dom7

  3: G → D  [0G/660F]  asc 7 → D min  [fallback] γ=4
     Candidates: D min | A min | F maj | D maj | B dim | G maj
  …
```

Field by field:
- **`1, 2, 3`** — bar number in the progression
- **`C → G`** — root movement (prior → posterior pitch class)
- **`[NG/MF]`** — candidate pool: N graph candidates, M fallback candidates
- **`asc 7`** — movement class (ascending by 7 semitones)
- **`G maj`** — the chord selected at the posterior root
- **`[graph]` / `[fallback]`** — which pool the selection came from
- **`γ=N`** — the index chosen by gamma-entropy sampling (lower = safer, higher = more exploratory)
- **`Candidates`** — the top 6 alternatives at the same posterior root

`gen''` ([`Builder.hs:428`](src/Harmonic/Framework/Builder.hs#L428)) adds verbose transform and advance traces — pitch-class arithmetic at every step, DB-stored vs computed functionality, the full render pipeline. Use it when you're debugging rendering edge cases.

**How** — use `gen` for fast generation, `gen'` to understand selection, `gen''` for debugging. All three have identical type signatures and can be swapped without touching anything else.

**Try it** — run `gen'` on the same seed with `entropy 0.2` vs `entropy 0.9` and watch the gamma indices shift. Add `hcKey "0#"` to the context and watch the pool sizes narrow.

<!-- [video ~30s: run gen' with entropy 0.5, then gen' again with entropy 0.9 — annotate the bar-by-bar output with arrows showing "prior root", "pool counts", "selected chord", "gamma index", "top candidates". Highlight how high entropy pulls from further down the candidate list] -->

___

## 3. Play the progression (`iK`, form, launcher)

**Why** — the shortest possible bridge from Haskell value to sound. We'll turn the progression into a TidalCycles pattern with a minimal launcher.

**What** —

```haskell
s <- seek "none" $ cue start $ len 4 $ entropy 0.5 $ gen

form = [ at 0 1 1 s ]

p01 f k d = d01 $ do
  let o = ch 01
  f
    $ stack [silence
      , arrange (0, 1) k (-9, 9) flow (overlapF 0) ["~"
        , "[0 1 2 3]/4"
      ] # o |* vel 0.8 |= legato 0.95
    ] |* vel d

do
  let k = iK tempo form (rep s 1)
  mapM_ id [hush, setbpm tempo
    , p01 id k 0.9
    ]
```

Three concepts here:
- **`at time kinetics dynamics progression`** ([`Form.hs:61`](src/Harmonic/Interface/Tidal/Form.hs#L61)) — a form node. With one node the kinetics and dynamics signals are constant.
- **`iK tempo formNodes chordSelection`** ([`Form.hs:67`](src/Harmonic/Interface/Tidal/Form.hs#L67)) — bundles everything a launcher needs into a single `IK` context.
- **`arrange (lo,hi) k (-9,9) voicing modifier [patterns]`** ([`Bridge.hs:105`](src/Harmonic/Interface/Tidal/Bridge.hs#L105)) — maps the pattern across the progression. `(0,1)` is the kinetics range (active when kinetics is in this window), `(-9,9)` is the register.

`rep s 1` ([`Bridge.hs:86`](src/Harmonic/Interface/Tidal/Bridge.hs#L86)) auto-derives a one-chord-per-bar selection from the progression length.

**How** — define the launcher once. Every section below reuses `p01`. The pattern `"[0 1 2 3]/4"` cycles through chord tones. Change the pattern, re-run the `do` block, listen.

**Try it** — change `"[0 1 2 3]/4"` to `"[0 1 2 3 4 5 6 7]/8"`. Try `"[0 2 1 3]/4"`. Swap `flow` for `grid` (next section).

<!-- [video ~30s: execute the three blocks — the launcher definition, then the do-block, hear the progression cycling. Swap the pattern from "[0 1 2 3]/4" to "[0 1 2 3 4 5 6 7]/8" and re-run — hear the density double] -->

___

## 4. Shape the generation (`HarmonicContext`)

**Why** — out-of-the-box generation is chromatic and unconstrained. The HarmonicContext modifiers shape what the generator can pick from: key, pitch set, bass motion, required tones, inversions, tension direction.

**What** —

```haskell
ctx = invSkip 1
    $ consonant
    $ hcPedal "C?"
    $ hcKey "0#"
    $ hContext

s <- seek "none" $ cue start $ tonal ctx $ len 8 $ entropy 0.5 $ gen'

do
  let k = iK tempo [at 0 1 1 s] (rep s 1)
  mapM_ id [hush, setbpm tempo
    , p01 id k 0.9
    ]
```

All modifiers live in [`Builder/Types.hs`](src/Harmonic/Framework/Builder/Types.hs#L74):

| Modifier | Example | Effect |
|---|---|---|
| `hcOvertones` | `hcOvertones "E A D G"` | Pitch set from overtone series (bass tuning here) |
| `hcKey` | `hcKey "0#"` | Key signature — `0#`=C, `1#`=G, `2b`=Bb |
| `hcRoots` | `hcRoots "C E G"` | Allowed bass notes |
| `hcPedal` | `hcPedal "C G?"` | Required tones; `?` = preferred |
| `consonant` / `dissonant` | `dissonant $ ...` | Drift direction across the progression |
| `invSkip` | `invSkip 2` | Min non-inversions between inversions |

Filter strings support `"*"` wildcard, `"-Bb'"` subtraction, `"E'"` prime notation (exact pitch, no overtones), `"key"` / `"tones"` mirror keywords, and `"rise"` / `"fall"` for forced stepwise bass motion.

**How** — compose modifiers right-to-left with `$`. Comment out individual lines to fall back to defaults. The whole context is passed via `tonal`.

**Try it** — swap `consonant` for `dissonant`. Remove `hcPedal "C?"`. Change `hcKey "0#"` to `hcKey "2b"`. Watch the `gen'` output — the pool shrinks as constraints tighten.

<!-- [video ~45s: layer modifiers one at a time — start with hContext, add hcKey "0#", add hcPedal "C", add dissonant — re-running gen' after each. Show how the candidate pool narrows in the gen' output and how the sound shifts audibly] -->

___

## 5. Going online (composer graph)

**Why** — the Neo4j graph holds harmonic transitions learned from 80+ composers in the Yale Classical Archives Corpus. Swapping `seek "none"` for a composer name channels that style into your generation. Blends produce weighted combinations.

**What** —

```haskell
-- docker compose up -d neo4j   (then uncomment below)

s <- seek "bach" $ cue start $ tonal ctx $ len 8 $ entropy 0.5 $ gen'

s <- seek "debussy" $ cue start $ tonal ctx $ len 8 $ entropy 0.5 $ gen'

s <- seek "bach:25 debussy:75" $ cue start $ tonal ctx $ len 8 $ entropy 0.5 $ gen'

s <- seek "*" $ cue start $ tonal ctx $ len 8 $ entropy 0.5 $ gen'
```

Composer weight parsing lives in [`Query.hs`](src/Harmonic/Evaluation/Database/Query.hs). Blends produce a portmanteau name in the `gen'` header ("Bachdebussy"). `"*"` aggregates across the full corpus.

**How** — `seek` is the terminal that runs everything. The string is case-insensitive. Everything else in your modifier chain stays the same.

**Try it** — compare Bach's cadential pull with Debussy's modal inflections. Stack a composer you know with one you don't. Use the same context you built in Section 4 — only `seek` changes.

<!-- [video ~45s: run seek "bach" then seek "debussy" then seek "bach:30 debussy:70" on the same starting state and context. Play each one audibly (single-state form) — the harmonic character shifts each time. Show the portmanteau in the gen' header] -->

___

## 6. Voicing strategies

**Why** — the same progression can sit vertically in radically different ways. The voicing function determines how each chord's intervals are distributed across registers without changing the harmony itself.

**What** — [`Arranger.hs:269`](src/Harmonic/Interface/Tidal/Arranger.hs#L269)

| Function | Bass | Voice leading | Best for |
|---|---|---|---|
| `flow` | Any inversion | Smoothest (cyclic DP, globally optimal) | Pads, harmonic beds |
| `grid` | Root locked | Smooth upper voices | Grounded chords |
| `lite` | Any | None (raw intervals) | Direct control |
| `root` | Root PC only | N/A | Bass lines, mono |
| `fund` | Harmonic fundamental | N/A | Sub bass, kick drums |

Swap the voicing in a launcher:

```haskell
arrange (0, 1) k (-9, 9) flow (overlapF 0) ["~", "[0 1 2 3]/4"] # ch 01
arrange (0, 1) k (-9, 9) grid (overlapF 0) ["~", "[0 1 2 3]/4"] # ch 01
arrange (0, 1) k (-9, 9) root (overlapF 0) ["~", "[0]/1"]       # ch 01 |- oct 2
```

**How** — the voicing function is the 4th argument of `arrange`. Everything else stays identical. Stack multiple `arrange` calls with different voicings in a single launcher to build a full texture.

**Try it** — compare `flow` and `grid` on a long progression. Add a second `arrange` with `root` and an octave offset for a bass line.

<!-- [video ~30s: run the same progression with flow, then grid, then stack flow (mid) + root (bass -2 oct) in one launcher. The textural difference is audible immediately] -->

___

## 7. Chord selection (`rep` and `warp`)

**Why** — you control *which* chord plays and *when* it changes. `rep` does the default one-chord-per-bar; `warp` gives you explicit mininotation control.

**What** — [`Bridge.hs:76`](src/Harmonic/Interface/Tidal/Bridge.hs#L76) for `warp`, [`Bridge.hs:86`](src/Harmonic/Interface/Tidal/Bridge.hs#L86) for `rep`.

```haskell
-- Default: one chord per bar
k = iK tempo [at 0 1 1 s] (rep s 1)

-- Explicit: 1-indexed chord selection over 4 bars
k = iK tempo [at 0 1 1 s] (warp "[1 2 3 4]/4")

-- AABA-like cycle
k = iK tempo [at 0 1 1 s] (warp "[1 1 2 1]/4")

-- With TidalCycles operators
k = iK tempo [at 0 1 1 s] (warp "[1 <2 3> 1 4]/4")

-- Euclidean rhythm on chord 1
k = iK tempo [at 0 1 1 s] (warp "[1(3,8) 2]/4")
```

The `/N` divisor in `warp` maps directly to physical bars — `"[1 2 3 4]/4"` steps through the four selected chords over exactly four bars. Any TidalCycles operator (choice, probabilities, euclidean rhythms, nesting) applies.

**How** — swap `rep` for `warp` inside `iK`. Nothing else changes in the launcher. You can warp any pattern as long as the indices exist in the progression (they wrap modulo-length otherwise).

**Try it** — `"[1 2 3 4 3 2 1 4]/4"` — 8 events, 4 bars, extended harmonic rhythm. `"[1 . [2 3]]/4"` — nested rhythm. `"[1 2 3 4]/8"` — double-length, slower harmonic change.

<!-- [video ~30s: start with rep s 1, then warp "[1 2 3 4]/4", then warp "[1 1 2 1]/4" (AABA), then warp "[1(3,8) 2]/4" (euclidean) — each time re-run and hear the harmonic rhythm reshape] -->

___

## 8. `arrange` vs `arrange'`

**Why** — two ways that a pattern interacts with progression changes. `arrange` lets the pattern flow across chords at its own speed; `arrange'` compresses the full pattern into each chord slot. Both handle notes crossing harmony boundaries by sustaining naturally — no spurious onsets.

**What** — [`Bridge.hs:105`](src/Harmonic/Interface/Tidal/Bridge.hs#L105) (arrange), [`Bridge.hs:205`](src/Harmonic/Interface/Tidal/Bridge.hs#L205) (arrange').

```haskell
-- arrange: pattern flows across the progression
arrange (0, 1) k (-9, 9) flow (overlapF 1) ["~"
  , "[0 1 2 3 4 5 6 7]/2"
] # ch 01

-- arrange': the same pattern repeats in every chord slot
arrange' (0, 1) k (-9, 9) flow (overlapF 0) ["~"
  , "[0 1 2 3 4 5 6 7]/2"
] # ch 01
```

`arrange` feels like a melody running over changing chords — each note's pitch maps to whichever chord is active at its onset. `arrange'` feels like an arpeggiator locked to each chord — every chord gets the whole pattern, compressed to fit its duration.

`overlapF N` expands each chord's pitch set with pitches from N bars ahead, producing natural legato across transitions. Use `overlapB` for backward merging, `progOverlap` for bidirectional.

**How** — swap the function. Everything else identical. `overlapF` / `overlapB` are applied as the 5th argument — the progression modifier.

**Try it** — on the same progression and pattern, run `arrange` then `arrange'`. Listen for the melodic difference. Change `overlapF 0` to `overlapF 2` and hear the sustain extend.

<!-- [video ~45s: run the same progression and same pattern with arrange, then arrange'. Annotate visually: arrange = "pattern flows over harmony", arrange' = "pattern locked into each chord slot". Then increase overlapF and show the sustain] -->

___

## 9. Progression manipulation

**Why** — transform the harmony in real time with pure functions. Rotate, excerpt, transpose, reverse, fuse, interleave — all return new progressions that you can chain.

**What** — [`Arranger.hs:80-230`](src/Harmonic/Interface/Tidal/Arranger.hs#L80).

| Function | Signature | Effect |
|---|---|---|
| `rotate` | `Int -> Progression -> Progression` | Rotate by N bars |
| `excerpt` | `Int -> Int -> Progression -> Progression` | Range (1-indexed, inclusive) |
| `transposeP` | `Int -> Progression -> Progression` | Transpose by N semitones |
| `reverse` | `Progression -> Progression` | Reverse the order |
| `fuse` | `[Progression] -> Progression` | Concatenate a list |
| `fuse2` | `Progression -> Progression -> Progression` | Binary concatenate |
| `interleave` | `Progression -> Progression -> Progression` | Alternating chords |
| `expandP` | `Int -> Progression -> Progression` | Repeat each chord N times |
| `insert` | `CadenceState -> Int -> Progression -> Progression` | Replace bar N |
| `switch` | `Int -> Int -> Progression -> Progression` | Swap bars M and N |
| `clone` | `Int -> Int -> Progression -> Progression` | Copy bar M to N |
| `extract` | `Int -> Progression -> CadenceState` | Pull out one state |
| `progOverlapF` | `Int -> Progression -> Progression` | Merge pitches from ahead |
| `progOverlapB` | `Int -> Progression -> Progression` | Merge pitches from behind |
| `progOverlap` | `Int -> Progression -> Progression` | Both directions |

```haskell
-- Rotate by 2 bars
rotate 2 s

-- Extract bars 1–4
excerpt 1 4 s

-- Palindrome: original + reverse
fuse2 s (Harmonic.Interface.Tidal.Arranger.reverse s)

-- Interleave with a fourth-up version
interleave s (transposeP 5 s)

-- Chain transformations
transposeP 7 $ rotate 3 $ s
```

**How** — every function is pure. Assign the result to a new variable and rebuild your `iK` context with it. No launcher changes needed.

**Try it** — `expandP 2 s` to slow the harmonic rhythm. `progOverlapF 1 s` for natural sustain across transitions. Chain three or four transformations and see what comes out.

<!-- [video ~30s: take an 8-chord progression, show it under rotate, then excerpt, then transposeP, then interleave with a transposed copy — each time re-running the launcher briefly to hear the shape] -->

___

## 10. Explicit composition

**Why** — when you have specific chord changes in mind, skip the generator and build progressions by hand. Three levels of explicit control:

**What** — [`Arranger.hs:336`](src/Harmonic/Interface/Tidal/Arranger.hs#L336)

```haskell
-- Pitch-class list construction
sExplicit = fromChords
  [ [0, 4, 7]    -- C maj
  , [5, 9, 0]    -- F maj
  , [7, 11, 2]   -- G maj
  , [9, 0, 4]    -- A min
  ]

-- Note name syntax — more readable
sNamed = prog sharp (notesToPCs <$>
  [ [C, E, G]
  , [F, A, C']
  , [G, B, D]
  , [A, C', E]
  ])

-- Full CadenceState construction with explicit root movement
sStates = fromCadenceStates
  [ initCadenceState 0 "C" [0, 4, 7]
  , initCadenceState 5 "F" [0, 4, 7]
  , initCadenceState 2 "G" [0, 4, 7]
  , initCadenceState 2 "A" [0, 3, 7]
  ]
```

**How** — once you have a `Progression`, everything else in the library works exactly the same: voicing, arrangement, form, manipulation. The generator and explicit construction are interchangeable.

**Try it** — build your own 8-bar progression. Apply `transposeP 2` to it. Use `interleave` to weave it with a generated progression.

<!-- [video ~30s: write a 4-chord progression using notesToPCs with note-name syntax, assign to sNamed, play it with the standard p01 launcher. Then apply transposeP 2 and play again] -->

___

## 11. Kinetics form (programmed arc)

**Why** — wall-clock time as a compositional parameter. A form is a list of nodes, each placed at a specific second, with kinetics (0–1 continuous signal) and dynamics (amplitude envelope) values. Between nodes the signals interpolate linearly; the progression switches discretely at each node.

**What** — [`Form.hs`](src/Harmonic/Interface/Tidal/Form.hs)

```haskell
sA <- seek "none" $ cue start $ tonal (consonant $ hcKey "0#" $ hContext) $ len 4 $ entropy 0.3 $ gen
sB <- seek "none" $ cue start $ tonal (dissonant $ hcKey "0#" $ hContext) $ len 4 $ entropy 0.85 $ gen

arcForm =
  [ at   0   0.0  0.0  sA
  , at   5   0.3  0.4  sA
  , at  20   0.5  0.6  sA
  , at  30   0.8  0.8  sB   -- progression switch at the rise
  , at  40   1.0  1.0  sB
  , at  50   0.5  0.65 sA
  , at  60   0.0  0.0  sA
  , at  70   0.0  0.0  sA   -- silence gap
  ]

p01arc f k d = d01 $ do
  let o = ch 01
  f
    $ stack [silence
      , arrange (0, 1) k (-9, 9) flow (overlapF 0) ["~", "[0 1 2 3]/4"] # o |* vel 0.75
      , arrange (0.7, 1) k (-9, 9) flow (overlapF 0) ["~", "[0 2 4 7 4 2]/4"] # o |+ oct 1 |* vel 0.6
    ] |* vel d

do
  let k = iK tempo arcForm (rep sA 1)
  mapM_ id [hush, setbpm tempo, p01arc id k 0.85]
```

The upper-voice `arrange` has kinetics range `(0.7, 1)` — it only plays when the kinetics signal is at or above 0.7, i.e. during the climactic middle section. The chord pad `(0, 1)` plays throughout.

Reusable form templates live in [`live/forms.tidal`](live/forms.tidal):
- **7m24s** spectral narrative (454s loop, 11 nodes)
- **12m** spectral narrative (730s loop)
- **19m24s** spectral narrative (1174s loop)
- **3min pop** @ 112 BPM (190s loop, verse-chorus-bridge)

**How** — kinetics drive range-gating on `arrange` calls; dynamics drive amplitude via `|* vel`; progression switches at each node let you have the generator run one progression in the calm sections and another in the climactic ones.

**Try it** — stretch the timings to 5 minutes or compress to 20s. Change `(0.7, 1)` to `(0, 0.3)` so the upper line plays during the lead-in instead. Add a third arrange gated at `(0.3, 0.7)` for a middle layer.

<!-- [video ~60s: launch the arc — show the kinetics signal visually rising and falling, the progression switching at the peak, the upper line entering at kinetics 0.7 and dropping out on the way down. Let it play for ~45s to demonstrate the full arc audibly] -->

___

## 12. Groove (`subKick`)

**Why** — kick and sub bass that follow the harmonic root. `subKick` combines a kick pattern, a sustained sub, and an MPC-style CC64 sustain mechanism. It locks to the root of whichever chord is active at each onset.

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

Routes to MIDI channel 10. A complementary MPC kit program is provided separately for full-range sub+kick rendering.

___

## 13. Voice lines & the instrument paradigm

**Why** — address stacked voices as independent patterns with their own kinetics ranges, voice functions, and register placements. This is the foundation that the full virtual orchestra is built on.

**What** —

```haskell
p01satb f k d = d01 $ do
  let o = ch 01
  f
    $ stack [silence
      , arrange (0, 1) k (-9, 9) flow (overlapF 0) ["~", "[3]/1"] # o |+ oct 1 |* vel 0.7
      , arrange (0, 1) k (-9, 9) flow (overlapF 0) ["~", "[2]/1"] # o              |* vel 0.6
      , arrange (0, 1) k (-9, 9) flow (overlapF 0) ["~", "[1]/1"] # o              |* vel 0.6
      , arrange (0, 1) k (-9, 9) root (overlapF 0) ["~", "[0]/1"] # o |- oct 1 |* vel 0.8
    ] |* vel d
```

Four voices: soprano (highest, +1 oct), alto, tenor (middle), bass (-1 oct with `root` voicing). Each is just an `arrange` call with a different pattern index and octave offset.

**How** — stack `arrange` calls inside a launcher's `stack`. The voice functions (`flow`, `grid`, `root`) pick which pitches are selected; the octave offset places the voice in register.

### The full virtual orchestra

This stacked-voice paradigm is the foundation for the **Algorithmic Orchestration** system: 15 pitched instruments (flute, oboe, clarinet, bassoon, horn, trombone, bass trombone, harp, timpani, violin 1/2, viola, cello, contrabass), each with physical MIDI range clipping, addressed by voice (`Soprano`/`Alto`/`Tenor`/`Bass`, plus `8va`/`15va`/`8vb`/`15vb` octave variants). String articulations (`pizz`, `spicc`, `marc`, `legg`, `arco`) switch channel aliases per block. Section blocks (`wind`, `brss`, `strg`, `perc`) and timbral blends (`chalumeau`, `pastorale`, `brillante`, `maestoso`, `tutti`) group instruments into ensemble presets.

For this walkthrough we stay on the single piano channel. For the full orchestra:

- [`live/ORCHESTRAL_CATALOGUE.tidal`](live/ORCHESTRAL_CATALOGUE.tidal) — test each instrument's range and tonal character
- [`ALGORITHMIC_ORCHESTRATION.md`](ALGORITHMIC_ORCHESTRATION.md) — full documentation of the system

<!-- [video ~90s: full virtual orchestra demo — start with solo flute playing a kinetics-gated line, progressively add horn (brass), then violin 1 + viola (strings), then the full tutti at the form peak. Show the kinetics signal driving instrument entrances. This is the Algorithmic Orchestration showcase — one video tells the whole story without the guide needing to document every function] -->

___

## 14. Going further

**Complete examples** —
- [`live/examples/blue_in_green.tidal`](live/examples/blue_in_green.tidal) — jazz chorus-improv-chorus on the 12m spectral narrative
- [`live/examples/rosslyn_castle.tidal`](live/examples/rosslyn_castle.tidal) — folk form on the 7m24s spectral narrative

**Performance starter** —
- [`live/perform/state.tidal`](live/perform/state.tidal) — minimal blank state file

**Form templates** —
- [`live/forms.tidal`](live/forms.tidal) — 7m24s / 12m / 19m24s spectral narratives + pop song form

**Documentation** —
- [`CHANGELOG.md`](CHANGELOG.md) — V3 feature summary
- [`ALGORITHMIC_ORCHESTRATION.md`](ALGORITHMIC_ORCHESTRATION.md) — virtual orchestra
- [`ARCHITECTURE.md`](ARCHITECTURE.md) — R→E→T pipeline, four-layer architecture, graph schema

### Quick reference

**Generation** —

```haskell
start <- lead "C maj"                                      -- or initCadenceState
s <- seek "none" $ cue start $ len 4 $ entropy 0.5 $ gen   -- offline
s <- seek "bach" $ cue start $ tonal ctx $ len 4 $ entropy 0.5 $ gen   -- online
```

**Context modifiers** — compose with `$`, right-to-left. `hContext`, `hcOvertones`, `hcKey`, `hcRoots`, `hcPedal`, `consonant`, `dissonant`, `invSkip`.

**Voicing** — `flow`, `grid`, `lite`, `root`, `fund`.

**Chord selection** — `rep s 1`, `warp "[1 2 3 4]/4"`.

**Arrangement** —

```haskell
arrange  (lo,hi) k (-9,9) voicing modifier [patterns] # ch N
arrange' (lo,hi) k (-9,9) voicing modifier [patterns] # ch N   -- squeeze variant
```

**Form** — `at time kinetics dynamics progression`, `iK tempo [nodes] chordSelection`.

**Manipulation** — `rotate`, `excerpt`, `transposeP`, `reverse`, `fuse`, `fuse2`, `interleave`, `expandP`, `insert`, `switch`, `clone`, `extract`, `progOverlap`, `progOverlapF`, `progOverlapB`.

___

Questions and feedback via the [GitHub Issues](https://github.com/OscarSouth/theHarmonicAlgorithm/issues) tracker.
