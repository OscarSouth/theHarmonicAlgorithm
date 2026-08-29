# Polytonal Framework

Three simultaneous triad progressions from one corpus walk: a foundation
plus two partner chains that share tones with it bar by bar, so any pair
of layers sounds a 4-note structure and all three sound a 5-tone pentad.
The `genE` generation family.

## Theory

### The overlap algebra

Three 3-PC sets with each partner sharing exactly 2 pitch classes with
the foundation admit, by inclusion–exclusion (union = 3 + |triple
intersection| when all pairwise intersections are 2), exactly three
shapes:

| shape | triple ∩ | union | any two layers | all three |
|---|---|---|---|---|
| **common-dyad** | 2 (one dyad in all three) | 5 | 4 tones | 5 tones |
| **base-anchored** | 1 (partners take different foundation dyads) | 5 | T-pairs 4; S+M 5 | 5 tones |
| hub-tone | 1 (three different dyads through one tone) | 4 | 4 tones | 4 tones |

The generator admits the first two — one predicate covers both: each
partner shares exactly 2 tones with the foundation AND the three-way
union is exactly 5 — and excludes hub-tone (it cannot produce five
voices). The traversal chooses freely per bar; neither geometry is
privileged.

### Foundation and partners

The foundation (T layer) is a plain `gen` walk and the only carrier of
R constraints — key, allowed roots, rise/fall direction, drift, pedal,
inversion spacing. It owns the bass whenever it is present. The
partners (S/M layers) are corpus walks of their own: every partner bar
continues from its OWN previous bar's transition list, so each layer is
a plausible progression in its own right, not a memoryless vertical.
Partners honour the harmonic space (key, allowed-roots note set,
overtones) through the same R predicate as the walk, with no bass
target — direction specs and strata machinery never bind them, keeping
the harmonic space unconstrained for the additive layers.

S and M are assigned once, after generation: the chain with the lower
whole-layer dissonance total becomes S (ties — 0–2% of runs — break on
canonical zero-forms, then roots). Per-bar assignment would swap chain
membership at ~45% of bar boundaries and destroy the layer identity the
chains provide.

### Supply

Feasibility is foundation-independent: every one of the 55 triad
zero-forms admits exactly 108 common-dyad and 216 base-anchored partner
pairs from the full space of 220 absolute 3-PC sets. Against the live
classical graph the corpus lists alone supplied jointly valid pairs at
100% of 2,100 measured real steps (zero starvation, 100% chain edge
purity, shallow chosen ranks). Selection relaxes down tiers when a list
runs dry — one side from the space-constrained pure enumeration, then
both, then the unconstrained enumeration floor — so partner selection
is total: under a crushing context the foundation degrades exactly as
`gen` does (absorbing repetition at worst), never the partners.

## Applied — code surface

### Types (`Harmonic.Rules.Types.ProgressionContext`)

- `Family` gains `FPoly`; stamped explicitly by the producer, never
  inferred.
- `Layer = T | S | M | TS | TM | SM | TSM | PT`. Single tags project
  the stored layers; combinations synthesize pointwise pitch-class
  unions per bar rooted on the lowest constituent layer (T before S
  before M — the foundation owns a merged bass); `PT` synthesizes the
  pivot tones all three layers share (a dyad on common-dyad bars, the
  single hub tone on base-anchored bars). Total for every family:
  duplicated layers make every selector collapse to the stored
  progression.

### Generation (`Harmonic.Framework.Builder.PolyGen`)

- `runPolyGen` — foundation walk (byte-identical to `gen`: the same
  chain builder, untouched), then the partner pass: per bar, each
  partner fetches its own transition list, advances candidates from its
  own root, filters to the overlap rules against the foundation bar and
  the harmonic space, and one entropy-scaled gamma draw selects from the
  jointly valid pair pool ranked by summed own-list rank. Bar-0
  partners come from the dissonance-ranked enumeration (a cue has no
  transition list). Cues are triadic — a >3-note cue is refused.
- `runPolyGenFrom` — `genFrom` on an `FPoly` source: the foundation
  range regenerates as a `gen` regen; partner chains regenerate over
  it, seeded from the KEPT partner bars before the range, with a
  seam preference that the final regenerated bar can continue onto the
  kept next partner bars as real graph edges. The source's S/M
  labelling is preserved — a partial regen never reorders chains.
- Verbosity: zero prime prints the foundation grid plus the combined
  `TSM` grid; `genE'` adds the per-bar musical table (partners, pair
  structures, pentad, pivot tones); `genE''` adds the selection facts
  (geometry, pool tier and size, own-list ranks) via `PolyDiag` records
  on the step diagnostics (`printPolyDiagnostics`).

### Live-coding helpers (`Harmonic.Interface.Tidal.PolytonalT`)

- `genEReport :: ProgressionContext -> IO ()` — prints every layer view
  a pattern can select (T, S, M, TS, TM, SM, TSM, PT) as progression
  grids; `polyLayerViews` is the pure half.
- Every instrument and `arrange` takes `Layer` as an argument, so the
  combination selectors work everywhere with no signature changes. A
  layer used alone keeps its own roots and bass; the walking bass over
  an `FPoly` context walks the foundation (T) layer.

## Cross-references

- `archive/analysis/poly_viability.md` — the consolidated viability
  study (structural/algorithmic; classical graph only), with probes
  `poly_space/pool/seq/edge/chain/roots_probe.hs` and their reports.
- `documents/OCTATRIPENTATONICS.md` — the other three-layer paradigm
  (genP); same container, different layer semantics.
- `USER_GUIDE.md` §19–20 and `live/USER_GUIDE.tidal` SECTION 19–20.

## Status (V3.1.0)

Implemented: `FPoly`, the eight-constructor `Layer`, `PolyGen`
(fresh + regen), `PolyDiag` diagnostics at all three prime levels,
`PolytonalT` reporting, offline (`seek "none"`) operation via the
enumeration floor. The retired 4-note fusion family's machinery
(`quad`, `fuseState`) is removed; hand-built 4-note material (`lead'`)
remains first-class for playing and walking, and regenerates as triads
with a printed notice.

### Per-bar diagnostic shape (`genE''`)

```
    2:         C → Ab              maj                     [graph] γ=2
    partners   S Ab maj/C          M F min                 pivot C+Eb
    combined   TS Ab 6#11          TM C madd11             SM Ab madd9   TSM Ab 6add9#11
    poly       common-dyad         tier list pool 216      ranks S:3 M:7
```
