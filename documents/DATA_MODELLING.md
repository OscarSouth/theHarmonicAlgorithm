# Data Modelling — how corpus harmony becomes the cadence graph

How the Yale Classical Archives Corpus is modelled as the
`(:Cadence)-[:NEXT]->(:Cadence)` transition graph the generator walks. The
emphasis is the model — the abstractions and the reasoning behind them — with
just enough of the build mechanics for context. Every modelling decision here
shapes the graph's musical character, so each is stated with its rationale.

Related reading: `ARCHITECTURE.md` §6 (Layer A in context),
`Harmonic.Rules.Import.Transform` (counting semantics, in haddock),
`app/README.md` (the executable's stage list).

---

## 1. The modelling problem

The generator needs a trained model of *how harmony moves*: given the cadence
that just happened, what tends to follow, per composer. In Creative Systems
Framework terms (Wiggins 2006) this model is **R** — the corpus defines the
learned universe of transitions — while scoring (E) and the walk (T) consume
it downstream. Every choice below is judged by one question: *does the graph
faithfully capture what the composers did?* Data quality, information
richness, and interpretive accuracy in the model are synonymous with
musicality downstream.

A classical corpus is millions of vertical moments in specific keys and
registers. Two abstractions make it tractable as a Markov model:

- **Zero-form (pitch relativity).** A cadence is a movement interval plus a
  chord shape rooted at 0 (`[P 0, ...]`). An absolute D→G progression in one
  piece informs generation in all 12 keys — twelvefold effective training
  data, and the same shape the generator thinks in (movement + quality, root
  applied at realisation).
- **Triad reduction.** Every vertical, however dense, is read as its
  plausible triads. 55 zero-form triads × 12 movements = a complete, closed
  660-node keyspace — small enough that every node accumulates real
  evidence, rich enough to voice anything downstream (extensions are
  reintroduced by the voicing engine and the strata/polytonal layers).

## 2. The model

**Nodes** — the complete 660-key grid: one node per (movement, zero-form
triad). Node identity is the cadence's `show` string; the functionality half
follows the 55-form corpus name table (`corpusFunctionality`), the single
naming contract shared by the write side and every read-side fetch.

**Edges** — evidence-weighted cadence bigrams: `NEXT` holds one weight per
composer (`r.weights`, sparse — absent composer means zero) plus their sum
(`r.confidence`, what a `"*"` query ranks on). A composer's weights out of
any node form a probability distribution: prolific composers do not drown
quiet ones at the per-edge level.

**Evidence** — each slice contributes through *weighted interpretation*,
and each slice triple contributes exactly unit mass (§3–4).

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

The graph is **derived data**: the artefact plus this code is the truth, and
a rebuild must reproduce it. Writes target whatever `HA_NEO4J_URL` points at —
rebuild into a scratch container, compare, then promote.

## 3. Interpreting a vertical: weighted readings

An ambiguous vertical (say 5 distinct PCs) has no single correct triad
reading, and choosing only the "best" would discard genuine harmonic movement
that the reduction exists to recover. So every slice is read as its top three
candidate triads (built over the exporter-detected fundamental by the
overtone machinery), ranked by Hindemith dissonance — most consonant first —
carrying a **[3,2,1]** preference profile **normalised to sum to 1**.

Why normalised: unnormalised replication lets a 7-PC vertical inject several
times the probability mass of a plain triad *purely because it was
ambiguous*. Under normalisation every moment speaks at the same volume;
ambiguity splits a moment's vote, never raises it.

Ties keep the overtone enumeration order (the sort is stable) — a
deterministic, documented tie-break.

## 4. Counting movement: consistent paths over slice triples

A `Cadence` is a movement onto a target quality, so a Markov **edge** — one
cadence following another — spans a slice **triple** `(s1, s2, s3)`: the
cadence `a→b` followed by `b→c`. The middle reading `b` is **shared** by both
sides; each `(a, b, c)` combination contributes `w_a · w_b · w_c`, so a
triple's total contribution is exactly 1.

Sharing the middle reading is what keeps interpretation expansion honest.
Every reasonable reading of every moment informs the model, but the model
never asserts "arrived at reading b, then departed from reading b′" — two
readings of the *same* moment are never counted as movement between moments.
Without this constraint, same-moment pairs dominate the counts and the graph
learns enumeration artefacts (overwhelmingly self-loops) instead of harmony.

Corollaries of the triple rule:

- **Pedals are evidence, not artefacts.** ~24% of YCACL slices repeat their
  predecessor; sustained harmony produces self-edges through the same rule
  as every other transition, in genuine proportion (~2.3% of total mass).
- **Pieces are islands.** Counting runs per piece; no transition is invented
  across the boundary between one piece's end and the next one's start.
- **Short pieces are silent.** Fewer than three slices → no complete triple
  → no edges.

## 5. Composer identity

Raw composer strings normalise through `slug` (lowercase, punctuation
stripped, spaces → `_`) — the key under which every edge weight is stored
and fetched. The exporter and ingester must share this one contract: keys
produced under two different normalisations silently orphan every composer
whose renderings disagree. Curation is an explicit exclude list, empty by
default, and **every** excluded composer is reported with its piece count at
ingestion time — no silent drops. See `documents/COMPOSERS.md` for the
available keys and query syntax.

## 6. Verification

- **Pure seams first** (offline, in `stack test`): `TransformSpec` (counting
  semantics, unit mass, middle-consistency, naming contract, transposition
  invariance), `MergeSpec` (normaliser, reported curation, sparse merge),
  `GraphSpec` (write/read round-trip of the weights payload).
- **Scratch rebuild**: point `HA_NEO4J_URL` at a throwaway container, run
  `stack run`, compare against the live graph (node/edge counts, keyspace
  containment in the 660-key grid, self-loop mass, top edges, composer
  coverage) before promotion.
- **Online smoke**: after promotion, an online `gen'` run must keep graph
  counts (`[nG/...]`) nonzero across steps that select inversion forms.

## 7. Provenance

- **Corpus**: Yale Classical Archives Corpus (`NOTICES` §1).
- **Artefact**: `data/artefacts/ycacl_sequences.csv`, produced by
  `scripts/export_ycacl.R` from `~/musicdata/YCACL`. The artefact is
  gitignored; the R script and this document are its provenance record.
- **Dissonance ranking**: Hindemith (`NOTICES` §3).
- **Published graph**: the `corpus-v2` release artefact (a Neo4j dump of the
  live graph; `scripts/export_graph.sh`).
