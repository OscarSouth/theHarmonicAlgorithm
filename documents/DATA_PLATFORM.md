# The Data Platform — how the cadence graph is built

This document explains the full analytic chain that turns the Yale Classical
Archives Corpus into the `(:Cadence)-[:NEXT]->(:Cadence)` transition graph the
generator walks. It is written Why → What → How: every curated decision in the
pipeline is stated together with its rationale, because those decisions — not
the plumbing — are what the graph's musical character comes from.

Related reading: `ARCHITECTURE.md` §6 (Layer A in context),
`Harmonic.Rules.Import.Transform` (the counting semantics, in haddock),
`app/README.md` (the executable's stage list).

---

## 1. Why

The generator needs a trained model of *how harmony moves*: given the cadence
that just happened, what tends to happen next, per composer. The Creative
Systems Framework separation (Wiggins 2006) puts this squarely in **R** — the
corpus defines the valid/learned universe of transitions — while scoring (E)
and the walk (T) consume it downstream. Everything in this pipeline is
therefore judged by one question: *does the graph faithfully capture what the
composers actually did?* Data quality, information richness, and interpretive
accuracy here are synonymous with musicality downstream.

Two abstractions make a classical corpus tractable as a Markov model:

- **Zero-form (pitch relativity).** Every cadence is stored as a movement
  interval plus a chord shape rooted at 0 (`[P 0, ...]`). An absolute
  D→G progression in one piece informs generation in all 12 keys. This
  multiplies the effective training data twelvefold and matches how the
  generator thinks (movement + quality, root applied at realisation).
- **Triad reduction.** Every vertical slice, however dense, is interpreted as
  its plausible triads. 55 zero-form triads × 12 movements = a complete,
  closed 660-node keyspace — small enough that every node genuinely
  accumulates evidence, rich enough to voice anything downstream (extensions
  are reintroduced by the voicing and gen4/strata layers).

## 2. What

```
~/musicdata/YCACL  (source corpus, outside the repo)
   │  scripts/export_ycacl.R           — slice extraction, Z12 reduction,
   ▼                                     fundamental detection
data/artefacts/ycacl_sequences.csv     — 6.0M rows: composer, piece, order,
   │                                     pitch classes, fundamental
   │  Harmonic.Rules.Import.CSV        — parse to composer → piece → [ChordSlice]
   │  Harmonic.Rules.Import.Merge      — slug normalisation, reported curation
   │  Harmonic.Rules.Import.Transform  — interpretation expansion + counting
   │  Harmonic.Evaluation.Analysis.Markov — per-source normalisation
   │  Harmonic.Rules.Import.Merge      — sparse per-edge composer weights
   ▼  Harmonic.Rules.Import.Graph      — batched parameterised UNWIND writes
Neo4j  (:Cadence {show, movement, chord, dissonance})-[:NEXT {confidence, weights}]->
```

The graph is **derived data**: the artefact (plus this code) is the truth, and
a rebuild must be reproducible from it. Writes go to whatever `HA_NEO4J_URL`
points at — rebuild into a scratch container, compare, then promote. The live
database is never the first target of an experiment.

## 3. How — the curated decisions

### 3.1 Slice → weighted triad interpretations

An ambiguous vertical (say 5 distinct PCs) does not have one correct triad
reading — and picking only the "best" one would throw away genuine harmonic
movement that the reduction is supposed to recover. So every slice expands to
its top three candidate triads (built over the exporter-detected fundamental
from the overtone machinery), ranked by Hindemith dissonance
(`Evaluation.Scoring.Dissonance` — most consonant first), carrying the
preference profile **[3,2,1]**, **normalised to sum to 1**.

Why normalised: an unnormalised profile lets a 7-PC vertical inject several
times the probability mass of a plain triad *purely because it was ambiguous*.
Normalisation makes every moment in the corpus speak at the same volume;
ambiguity changes how a moment's vote is *split*, never how loud it is.

Ties keep the overtone enumeration order (the sort is stable) — a documented,
deterministic tie-break.

### 3.2 Slice triples → consistent-path transition counts

A `Cadence` is a movement onto a target quality, so a Markov **edge** (one
cadence following another) spans a slice **triple** `(s1, s2, s3)`: the
cadence `a→b` followed by the cadence `b→c`. The middle interpretation `b` is
**shared** by both sides — the model never claims "arrived at reading b, then
departed from reading b′". Each `(a, b, c)` combination contributes weight
`w_a · w_b · w_c`, so a triple's total contribution is exactly 1.

This is the un-flattened form of the historical Cartesian expansion. The
expansion intent is preserved verbatim: every reasonable reading of every
moment informs the model. What was removed (2026-08-24) is the flattening —
the previous pipeline zipped adjacent elements of the *flattened* expansion,
so 94.34% of its 105.8M learned transitions connected two alternative readings
of the *same* moment, concentrating 57.7% of all probability mass on
self-loops the corpus never played. Corpus pedals are real (~24% of slices
repeat their predecessor) and still produce self-edges — via the same triple
rule as every other transition.

Pieces are counted independently: no transition is invented across a piece
boundary. Pieces with fewer than three slices contribute nothing.

### 3.3 Node identity — the corpus naming contract

A node's key is its cadence `show` string (movement + functionality). The
functionality half is stamped through `corpusFunctionality` — the 55-form
legacy name table (`corpusNameTable`, `Harmony.hs`) — on **both** the write
side (`Transform`) and the read side (`Query`). The modern chord namers
deliberately diverge from the legacy names on 8 forms; routing both sides
through the table keeps one contract and makes re-ingestion reproduce the
live keyspace exactly. The tripwire is the NAMING CONTRACT test in
`TransformSpec`: it fails the moment any emitted key leaves the table.

### 3.4 Composer keys — curation is reported, never silent

Raw composer strings normalise through `slug` (lowercase, punctuation
stripped, spaces → `_`). Curation is an explicit exclude list in `app/Main.hs`
(empty by default), and **every** dropped composer is printed with its piece
count at ingestion time. The historical 574-entry allow-list was deleted: it
had been generated under a different normaliser, silently dropped 22 composers
(96,133 slices — both Strausses, de Falla, Nunes Garcia, three Bach sons), and
carried 105 entries matching nothing in the artefact.

### 3.5 Per-source normalisation and sparse merge

Per composer, edge counts normalise so each source cadence's outgoing edges
sum to 1 (`Markov.probabilitiesFromCounts`) — a composer's opinion about
"what follows X" is a probability distribution, and prolific composers do not
drown quiet ones at the per-edge level. Per-edge maps are then merged
**sparsely**: a composer absent from an edge carries implicit weight 0 (the
read side already treats missing keys as 0; the historical dense zero-padding
produced a 928MB store for no scoring benefit). `r.confidence` is stored as
the sum of the edge's weights — the invariant the wildcard (`"*"`) fast path
depends on, pinned by `GraphSpec`.

### 3.6 The write seam

All writes travel as JSON parameters into one `UNWIND $rows` + `MERGE`
statement per 1000-row batch: transactional, idempotent (a failed run is
simply re-run), immune to injection by construction, and a few hundred round
trips instead of one per edge. Schema application is a single idempotent
uniqueness constraint on `show` — nothing outside this pipeline's own
subgraph is ever touched.

## 4. Verification

- **Pure seams first**: `TransformSpec` (counting semantics, mass
  conservation, naming contract, transposition invariance), `MergeSpec`
  (normaliser, curation reporting, sparse merge), `GraphSpec` (write/read
  round-trip of the weights payload) — all run offline in `stack test`.
- **Scratch rebuild**: point `HA_NEO4J_URL` at a throwaway container, run
  `stack run`, and compare against the live graph (node/edge counts,
  keyspace containment in the 660-key grid, self-loop mass, top edges,
  per-composer coverage) before any promotion.
- **Online smoke**: after promotion, an online `gen'` run must keep graph
  counts (`[nG/...]`) nonzero across steps that select inversion forms.

## 5. Provenance

- **Corpus**: Yale Classical Archives Corpus (see `NOTICES` §1).
- **Artefact**: `data/artefacts/ycacl_sequences.csv` — produced by
  `scripts/export_ycacl.R` from `~/musicdata/YCACL`. The artefact is
  gitignored; the R script and this document are its provenance record.
- **Dissonance ranking**: Hindemith (see `NOTICES` §3).
