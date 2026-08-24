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
Rscript scripts/export_ycacl.R ../musicdata/YCACL ../musicdata/YCAC-metadata.csv/YCAC-metadata.csv data/artefacts/ycacl_sequences.csv
```
The ingestion modules expect the output file at
`data/artefacts/ycacl_sequences.csv` (the path is defined once, as
`ycaclArtifactPath` in `src/Harmonic/Config.hs`). `data/artefacts/` is
gitignored — the export is a large derived file, regenerated rather than
distributed. The script creates the directory if it does not exist.

---

## export_graph.sh

Exports the composer graph as a single-file archive for publication.

The dump is produced by hand rather than in CI: it needs a populated Neo4j, and at a few
hundred megabytes it has no business moving through GitHub Actions. It is published to a
dedicated `corpus-v1` release so one stable URL serves every code release — replace the
asset under the same tag and filename and existing links keep working.

```bash
scripts/export_graph.sh [output-dir]     # default: out/ (gitignored)
```

Prints the size, a SHA256, the `gh release` upload command, and the matching `load`
command for users.

**Syntax note:** this stack runs neo4j 4.4.13, which uses `neo4j-admin dump` /
`neo4j-admin load`. The `neo4j-admin database dump` form is Neo4j 5+ and does not work
here. 4.4 also cannot dump a running database, so the script stops the container and
restarts it via a trap.

---

## ci/

Checks used by `.github/workflows/ci.yml`. Every one runs locally too — the point is that
a failure is reproducible on your machine, not only in a runner.

| script | what it guards |
|---|---|
| `check_guide.py` | Type-checks every code block in `live/USER_GUIDE.tidal` against the real library |
| `check_lockstep.py` | `USER_GUIDE.md` section N matches `live/USER_GUIDE.tidal` SECTION N, titles included |
| `check_snippets.py` | The prefix index in `USER_GUIDE.md` matches `live/snippets.cson` |
| `check_links.py` | Relative links in the public docs resolve to **tracked** files |
| `check_haddock.py` | Haddock coverage and markup baselines from the documentation pass |
| `extract_guide_blocks.py` | Helper for `check_guide.py`; also useful standalone for inspecting the payload |
| `changelog_slice.py` | Prints one `CHANGELOG.md` section, used for release notes |
| `release_notes_appendix.md` | Boilerplate appended to generated release notes |

Run them all:

```bash
for s in lockstep snippets links guide haddock; do python3 scripts/ci/check_$s.py; done
```

### How check_guide.py works

The guide is cumulative — later sections use bindings from earlier ones (`s`, `start`,
`tempo`, `form`). So every code block is spliced into a **single `do` block** and *bound*,
never forced: GHC type-checks the whole thing with types flowing exactly as they would in
a live session, and nothing executes.

Nothing executing is deliberate. The guide is graph-first — its examples use `seek "*"`,
so running them would need Neo4j. Type errors are the drift that actually matters here
(renamed functions, changed signatures), and those need no database.

Tidal boots headlessly for this: `midi = s "thru"` in `live/BootTidal.hs` is an OSC
control name, not a hardware binding, and `cEnableLink` is `False`. No SuperCollider, no
MIDI device, no audio hardware.

Errors are reported at their real `live/USER_GUIDE.tidal` line with the enclosing section
name. Pass `--keep` to retain the generated payload for inspection.
