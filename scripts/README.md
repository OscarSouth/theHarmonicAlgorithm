# scripts

This folder holds data-prep helpers used before the Haskell ingestion pipeline runs.

## export_ycacl.R

Purpose:
- Scan every YCACL CSV and normalize composer names using the slug list in `app/Main.hs`.
- Parse note tokens while preserving accidentals and octave signs so spellings like `B-1` versus `B--1` stay distinct.
- Sort pitch classes from low to high, remove upper-register doublings, and infer the true bass fundamental for each slice.
- Promote ultra-low pedals that simply double the same pitch class (within two octaves) so the exported `fundamental` column reflects the harmonic root rather than a repeated drone.
- Filter to harmonically useful slices (3–7 voices) and emit `composer,piece,order,pitches,fundamental` rows.

Key knobs:
- `allowed_composers` (top of the script) keeps the dataset small while we iterate; uncomment more names when you are ready for longer runs.
- `max_voices` defaults to 7 via the CLI but can be overridden when calling the script.

Usage:
```bash
Rscript scripts/export_ycacl.R ../musicdata/YCACL ../musicdata/YCAC-metadata.csv/YCAC-metadata.csv data/ycacl_sequences.csv
```
The ingestion modules expect the output file at `data/ycacl_sequences.csv`.
