# Third-Party Notices

Every third-party source this project draws on: the two corpora behind the
graphs, the libraries it builds against, the theory it implements, and the
influences behind its pattern library.

## 1. YCACL — classical (`:Cadence` graph)

The classical transition graph is trained on the Yale Classical Archives Corpus
(YCACL), obtained from the UCI Machine Learning Repository:

> Dua, D. and Karra Taniskidou, E. (2017). UCI Machine Learning Repository.
> Irvine, CA: University of California, School of Information and Computer
> Science. http://archive.ics.uci.edu/ml

Original data: `jsbach_chorals_harmony.data` (Bach chorale harmonisations).
Extended corpus: `ycacl_sequences.csv` (Yale Classical Archives).

Users rebuilding the database from source can obtain the corpus files from the
UCI repository directly. See [`documents/DATA_MODELLING.md`](documents/DATA_MODELLING.md)
for the full provenance chain.

## 2. Jazz-Chord-Progressions-Corpus — jazz (`:Change` graph)

The jazz transition graph is trained on the Jazz-Chord-Progressions-Corpus by
Carey Bunks (CC BY 4.0), derived from the open-source Impro-Visor "Imaginary
Book" collection:

> C. Bunks, T. Weyde, S. Dixon, and B. Di Giorgi, "Modeling Harmonic
> Similarity for Jazz Using Co-occurrence Vectors and the Membrane Area,"
> in Proc. of the 24th Int. Society for Music Information Retrieval
> Conf. (ISMIR), Milan, Italy, 2023.

Corpus repository: https://github.com/carey-bunks/Jazz-Chord-Progressions-Corpus

## 3. TidalCycles

The `Interface.Tidal` modules depend on TidalCycles (GPL-3.0). The
project is itself licensed GPL-3.0 (see `LICENSE`), so the combined
work's licence and its strongest dependency's agree:

> TidalCycles — Live coding music with Haskell.
> Alex McLean and contributors. https://tidalcycles.org — GPL-3.0
>
> hosc — Haskell Open Sound Control.
> Rohan Drape and contributors — GPL-3.0

## 4. Hindemith dissonance table

The dissonance scoring module is based on interval consonance values derived
from Paul Hindemith's harmonic theory:

> Hindemith, P. (1937). *The Craft of Musical Composition, Book 1.*
> Schott Music. English translation (1942) by Arthur Mendel.

## 5. Creative Systems Framework

The R→E→T architectural model follows Wiggins' Creative Systems Framework:

> Wiggins, G. A. (2006). A preliminary framework for description, analysis and
> comparison of creative systems. *Knowledge-Based Systems*, 19(7), 449–458.

## 6. Haskell dependencies

All Haskell package dependencies are listed in `package.yaml`. The majority are
licensed under BSD-2-Clause or BSD-3-Clause. Notable exceptions:

> `tidal`, `hosc`, `tidal-core`, `tidal-link` — GPL-3.0 (see §3)

Full licence texts are available via each package on Hackage:
https://hackage.haskell.org

## 7. Fonts

The generated API documentation is set in Geist and Geist Mono, licensed under
the SIL Open Font License. The licence text ships alongside the font files in
`.github/haddock-theme/fonts/OFL.txt`.

## 8. Musical influences

The drum pattern library (`live/drumpats/`) is assembled from many free and
open sources. Among them, a handful of patterns were referenced from *Pocket
Operations*, and heavily modified and expanded in the process:

> Wenzel, P. *Pocket Operations — a collection of classic drum machine
> patterns.* Shittyrecording Studio. https://shittyrecording.studio

Referenced with the author's permission.
