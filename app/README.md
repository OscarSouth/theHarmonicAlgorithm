# app

`Main.hs` is the run-once ingestion executable (`stack run`): it rebuilds the
`(:Cadence)-[:NEXT]->(:Cadence)` graph from the YCACL artefact.

1. Loads `data/artefacts/ycacl_sequences.csv` via `Harmonic.Rules.Import.CSV`,
   producing composers → pieces → `ChordSlice`s (pitch classes 0-11, exporter
   fundamentals).
2. Normalises composer names to graph keys (`slug`) and applies curation via
   `Harmonic.Rules.Import.Merge` — every excluded composer is REPORTED, never
   dropped silently.
3. Per composer, folds each piece's slices into Markov transition counts with
   `Harmonic.Rules.Import.Transform.buildTransitionCountsPerPiece`: every
   consistent triad-interpretation path over each slice triple contributes
   weighted counts (per-slice weights normalised to 1). Node keys are stamped
   through `corpusFunctionality`, so the keyspace matches the live graph and
   the read side exactly.
4. Normalises counts into per-source probabilities
   (`Harmonic.Evaluation.Analysis.Markov`) and merges them into one sparse
   composer-weight map per edge.
5. Truncates the cadence subgraph, applies the schema constraint
   (idempotent, non-destructive), and writes the edges in parameterised
   `UNWIND` batches (`Harmonic.Rules.Import.Graph`).

The write targets whatever `HA_NEO4J_URL` points at (default
`http://localhost:7474`) — rebuild into a scratch container and compare
before promoting to the live database.

`Main.hs` is also the user-facing log surface: add descriptive `putStrLn`s
whenever new long-running work is introduced.
