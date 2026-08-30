# Chordscale Framework

Key-area analysis and chord-scale layer derivation for the `gen` and `genJ`
families: every generated (or hand-opted-in) progression carries a real
S layer (one anhemitonic pentatonic per bar) and M layer (one seven-note
mode per bar), derived from a whole-progression reading of its key areas.

## Theory

**Chord-scale, key-first.** Chord-scale theory maps each chord to a scale,
but per-chord assignment fragments a progression into false modulations.
The framework instead detects **key areas** — contiguous spans governed by
one key — and expresses each bar's scale as *the key's set rooted on the
bar's harmonic root*. Inside one key area the familiar assignments emerge
with no lookup table: the ii takes Dorian, the V Mixolydian, a IVmaj7
Lydian, because those are what the key's pitch-class set *is* from those
roots.

**The 24-key lattice.** Candidate keys are 12 major areas plus 12
**composite minor** areas. A minor key is one tonic with three
interchangeable scale forms — natural (the relative-major set), harmonic
minor, melodic minor — selected **per bar** by chord fit. A minor ii-V-i
is therefore ONE key area: the iiø7 bar realises the harmonic form
(Locrian ♮6), the V7♭9 bar the harmonic form from its own root (Phrygian
dominant) or the melodic form (altered), the tonic bars the melodic or
natural forms. Melodic minor is never an independent key whose ii-V-i sits
on its own degrees. Harmonic major and other exotic parents are available
only as per-bar overrides, never as keys.

**Detection.** A cyclic Viterbi walk over the lattice: per-bar emission
scores weight each chord tone's key-membership by structural importance
(the bass-vocabulary tiers: target > strong > passing > colour — a V7♭9's
♭9 is the harmonic-minor marker, so notated colour counts), plus
functional-harmony votes (a dominant shell votes for the key a fourth
above; an altered dominant votes a *minor* tonic; mM7/m6 mark a tonic
minor). Switching keys costs a calibrated penalty, discounted when the bar
before the boundary is dominant-functioning in the new key or the arrival
bar is its tonic — boundaries land on functional resolutions. The
progression is treated as a cycle (the wrap edge is real), so rotating the
bars rotates the answer.

**The M layer.** Each bar's mode set is its key's form rooted at the bar's
harmonic root. When a chord escapes every form of its key (a genuinely
chromatic bar), an override ladder keeps the layer honest: seeded special
cases (half-diminished outside a minor area → harmonic minor a whole step
below; altered dominant → its altered scale), then a search of the full
28-mode vocabulary rooted at the bar root (chord contained, fewest tones
foreign to the key), then a best-coverage gap fallback. Override sets are
always built from mode templates — the layer can never leave the
vocabulary.

**The S layer.** The pentatonic vocabulary is exactly the 12 transpositions
of the anhemitonic set `[0 2 4 7 9]` (`MajorPenta`; its rotations include
the minor pentatonic). Structural facts: any diatonic set contains exactly
**3** such pentatonics, melodic minor exactly **1**, harmonic minor and
harmonic major **0**. A second cyclic Viterbi assigns one pentatonic per
bar — scored by guide-tone coverage (3rds/7ths weighted highest), penalised
for avoid-tone and out-of-mode content — under its own switch penalty, so
one pentatonic covers as many bars as it musically can (the classic
"three pentatonics per key" pedagogy emerges as the argmax). A pentatonic
MAY leave the key where the harmony demands it (harmonic-minor spans have
no in-key candidate); this is chromaticism carried by the pentatonic's own
melodic self-sufficiency, and the report flags it.

**Layer semantics.** The M bar is rooted on the bar's harmonic root — it
IS the mode of the bar. The S bar is rooted on the *pentatonic's* root
(the way a player names the set) — a pentatonic legitimately excludes the
chord root (the pent-on-the-fifth over a maj7 does so by design). Both
voice through the same chroma engine as genP (`strataModeFlow`): pattern
index i addresses the i-th slot of a fixed pitch **lattice** grounded on
bar 0's stack and inflected to each bar's set by minimal accidental
movement — a held index pedals its pitch across key areas (C over an F
Mixolydian bar stays C; over a B Aeolian bar it inflects to C#) rather
than restarting on each bar's root. 7-PC bars print as mode names. `PT` on a derived context is the
chord tones the pentatonic keeps — an anchor-tone selection.

## Applied — code surface

- `Harmonic.Evaluation.Analysis.KeyArea` — the analysis (`analyzeProgression`,
  `barPalettes`) and the layer derivation (`chordscale`). All constants
  probe-calibrated: switch penalty λ=6, dominant-approach bonus 3,
  tonic-arrival bonus 1.5, pentatonic penalty λₚ=4
  (`archive/analysis/keyarea.md`, `penta.md` — segment-length sweeps on
  live gen/genJ output plus hand-encoded cyclic standards).
- `gen` and `genJ` producers apply `chordscale` automatically, including
  after a `genFrom` regen (the whole spliced progression is re-analysed —
  key boundaries are global). Hand-built contexts opt in:
  `chordscale (fromChords …)`. Identity on genP/genE, whose layers already
  mean something else. Provenance stays `Nothing`, so `attempt` scoring and
  genP dispatch are untouched.
- `Harmonic.Interface.Tidal.ChordscaleT` — `chordscaleReport`: per bar the
  chord, key area, form, mode name, pentatonic, and flags (`<` boundary,
  `!` override, `?` gap, `*` out-of-key pentatonic).
- The **walking bass** consumes the same detector (`barPalettes` feeds the
  beat-3 palette tier and the Minor-Thirds rule in `walkLine`/`walkLineJ`),
  replacing the old vote-window key inference. Under identity chord
  selection the bass walks the sets the layers display; under a reordering
  selection it re-analyses the performed sequence — deliberately — and its
  lines add chromatic approach tones beyond any stored set. The genP walk
  is untouched (strata chroma, not key detection).
- Silent bars are key-neutral: a bar with no chord tones casts no votes and
  inherits its span's key through the switch penalty.

## Cross-references

- `documents/OCTATRIPENTATONICS.md` — the genP three-layer framework this
  extends to gen/genJ (by chord-scale analysis, deliberately not by strata).
- `documents/POLYTONAL.md` — genE's independent-triad reading of the same
  three layers.
- `USER_GUIDE.md` §16 (walking lines), §19 (the three layers), §21 (genJ).
- `archive/analysis/{keyarea,penta,walk_diff,mt_narrow}.md` — the viability
  study, calibration sweeps, and walk-unification measurements.

## Status (V3.1.0)

Shipped 2026-08-29 alongside the walk unification; hardened the same day
(analysis forced at generation time, silent-bar neutrality, switch-cost
hoisting, Minor-Thirds narrow-palette spot test — see `mt_narrow.md`).
