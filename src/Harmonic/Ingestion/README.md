# Harmonic/Ingestion

Glue modules that turn the YCACL export into cadence transitions ready for Markov analysis.

## Files

- `Types.hs` — lightweight definition of `ChordSlice`, capturing the deduped pitch stack and exporter-provided fundamental pitch class. Every ingestion stage should pass this around instead of naked `[Int]` lists so we never lose bass context.
- `CSV.hs` — loads `data/ycacl_sequences.csv` with cassava, groups slices by composer/piece, and returns `Map Composer -> Map Piece -> [ChordSlice]`. All filtering decisions (composer include/exclude) happen upstream in the R exporter, so this module simply trusts the `fundamental` column.
- `Transform.hs` — converts each `[ChordSlice]` sequence into duplicated cadences:
  - Computes `possibleTriads''` rooted on `sliceFundamental` and ranks candidates by Hindemith dissonance.
  - Duplicates the top three picks 3/2/1 times, falling back to a single `flatTriad` if no candidates exist.
  - Cross-multiplies adjacent slices so every plausible `from`→`to` pair contributes to the Markov corpus.

## Design Notes

- Duplicating cadences instead of storing fractional weights keeps `transitionCounts` untouched; the Markov module still just counts edges.
- Because `ChordSlice` retains fundamentals, future experiments (e.g., bass-only filtering or inversion tracking) can be implemented without touching the R exporter again.
