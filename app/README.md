# app

`Main.hs` orchestrates the ingestion pipeline:

1. Loads `data/ycacl_sequences.csv` via `Harmonic.Ingestion.CSV`, producing a map of composers → pieces → `ChordSlice`s.
2. Logs coverage metrics so long exports can be monitored without inspecting Neo4j manually.
3. Builds duplicated cadence streams per composer (top triads replicated 3/2/1) by calling `Harmonic.Ingestion.Transform`.
4. Computes Markov transitions, merges them per `(:Cadence)-[:NEXT]->(:Cadence)` edge, and attaches per-composer weight maps.
5. Runs `truncateCadenceGraph` (APOC batched delete) followed by `initGraph` to keep the graph deterministic before writing the latest transitions.

When editing `Main.hs`, keep in mind that the file is the user-facing log surface—add descriptive `putStrLn`s any time new long-running work is introduced.
