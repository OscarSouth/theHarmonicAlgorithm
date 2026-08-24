# Changelog for theHarmonicAlgorithm

__________________________________________________________________________________

## Version 3.1.0 — in progress (2026)

The cumulative modernisation release: each block below lands as it completes;
the version tags once the sweep is done.

### The Data Model & corpus-v2

The trained model itself got the upgrade this time. The ingestion pipeline was
rebuilt end-to-end and the graph re-trained as **corpus-v2**.

**Corpus-v2** — Transition counting is now consistent-path: every plausible
reading of a musical moment still informs the model, but alternative readings
of the *same* moment are never mistaken for movement. In corpus-v1 that
artefact concentrated 57.7% of all probability mass onto self-loops; in
corpus-v2 self-loops carry 2.3% — the genuine pedals — and the strongest
learned patterns are real harmonic rhetoric (arrive by fifth, then hold).
487 composers now carry weight (22 were silently lost to a normaliser
mismatch), and ambiguous verticals no longer shout louder than plain triads.

**The rebuild pipeline** — now something you can actually run:
`stack run` streams per composer (minutes, not tens of GB), writes in
transactional parameterised batches, reports every refusal instead of
dropping data silently, and reproduces the live keyspace exactly (write-side
naming is routed through the same 55-form corpus table the read side uses,
locked by tests). The whole analytic chain is documented in
`documents/DATA_MODELLING.md`.

__________________________________________________________________________________

## Version 3.0.0 is here! (2026)

Version 3.0.0 is a complete rebuild. The Harmonic Algorithm has grown into a
live performance instrument built around three interlocking systems: **The
Harmonic Algorithm** — the R→E→T generation engine, now capable of channelling
and blending the harmonic sensibilities of over 460 composers from the Yale
Classical Archives Corpus; **The Spectral Narrative** — a form and kinetics
framework that programs macro-level compositional arc as data, in wall-clock
seconds; and **Algorithmic Orchestration** — a full virtual orchestra of 15
instruments brought into the TidalCycles live coding environment.

The design throughout is composable. Harmonic contexts, generation parameters,
voicing strategies, form structures, and orchestral assignments are all small
pieces that can be combined, overridden, or commented out and inferred.
The aim is an instrument you can navigate fluidly in real time.

### Harmonic Generation

**Composer Blending** — Channel a single composer's harmonic style, blend
multiple with weighted ratios (`"debussy:0.75 bach:0.25"`), or aggregate across
the full corpus with `"*"`. Blended composers get a portmanteau name in the
output. Composer names are matched case-insensitively, at both parse and
corpus-lookup time — `"Bach"`, `"BACH"`, and `"bach"` all resolve identically.

**Offline Mode** — Pass `"none"` as the composer string to bypass the graph
entirely and generate using only the fallback mechanism. No Neo4j required.
Progressions are shaped by context filters (overtones, key, roots, drift,
inversion spacing) and entropy — fully musical, without corpus-trained style.

**Entropy Control** — A single float (0.0–1.0) dials between the familiar and
the surprising. At 0.3 the algorithm favours the most common cadences — safe,
consonant. At 0.8 it reaches into less-travelled harmonic territory for 
unexpected turns and distant modulations.

**Dissonance Drift** — `dissonant ctx` and `consonant ctx` shape the tension
arc across a progression. The algorithm selects the best musical choice meeting
the constraint — not just the most or least dissonant option.

**Bass Direction** — Force the bass line to ascend or descend stepwise through
the filtered pitch space for roots with `rise`/`fall` in the roots string. 
A numeric suffix skips notes: `rise2` selects every second, `rise3` every 
third, up to `rise6` for a tritone leap in a chromatic context. 
Combine with key and root filters to shape the path. Bracketed step lists give
per-step control: `rise<1 2 3>` rotates through step sizes across successive
generation steps, `rise<1,2>` picks one at random each step, and a trailing
`?` makes the direction optional for that step rather than mandatory.

**Inversion Spacing** — `invSkip N` enforces a minimum number of root-position
chords between any two inversions — direct control over harmonic density and
stability across a progression.

**`lead`** — Construct a starting state from a readable string: `lead "E min
(5)"` gives E minor arrived at by ascending five semitones. Root, quality, and movement are
each optional, falling through to random when omitted.

**Octatripentatonic Generation (`genP`)** — A second generation mode layered
over the same R→E→T engine, not a parallel generator. Eleven canonical
five-note strata (`I`–`XI`) are grouped into twelve tristrata, each a trio of
strata whose pairwise unions form a seven-note diatonic mode and whose triple
union forms the eight-note octatripentatonic set. `genP`/`genP'`/`genP''` walk
the strata graph bar by bar while the underlying triad generation stays fully
constrained by it; 33 `genI`–`genXI` aliases (plain and `'`/`''` diagnostic
variants) fix the starting strata for quick access. `hcTristrata` locks
generation to one tristrata; `relStrata`/`absStrata` set per-bar position
within it, relative or absolute. Every progression now carries three parallel
layers — triad, strata, and mode — selected with a `Layer` (`T`/`S`/`M`)
argument on `arrange`, `instrument`, and `divisi`; the strata and mode layers
get their own dedicated voicing strategy, `strataModeFlow`, alongside
`flow`/`grid`/`lite`/`root`/`fund`.

**Rank & Select (`attempt`/`viability`)** — `attempt N K` runs up to `K` full
generation passes, stops early once `N` of them are viable, and keeps only the
single highest-scoring progression — scored on root motion, voice leading,
cadence favourability (online, from the corpus), and mode validity.
`viability T` sets the score floor a pass must clear to count (default `0.6`).
Works identically online and offline. At Verbose (`gen''`), a full scoreboard
prints every attempt's per-term scores with a `← PICK` marker on the winner.

**Four-Note Generation (`gen4`)** — A third generator family alongside `gen`
and `genP`: every bar carries a 4-note chord. Each step selects a triad
through the same corpus-trained walk, then fuses in one more tone from the
R-valid palette (most-consonant-first, drawn at the same entropy); the walk
continues from the fused chord's most consonant embedded triad, so the added
tone can reinterpret the harmony while generation stays fully online.
`quad` is the underlying modifier (`gen4 = quad gen`; `genFrom` is
family-aware — a 4-note source regenerates 4-note bars automatically,
families never mix); `lead'` builds cues from explicit note-name lists
(`lead' "Eb Gb Bb Db"` → Eb m7). Shipped alongside: chord names now print at
full cardinality everywhere (4-note chords no longer display reduced to
triads), and multi-bar voicings of 4+-note material under `flow`/`grid` stay
in register.

### Voicing

**Five Voicing Strategies** — `flow` finds the smoothest path through the full
progression using cyclic dynamic programming. `grid` keeps the root locked in
the bass with optimised upper voices. `lite` gives the raw intervals. `root`
extracts the root as a melodic line. `fund` always returns the harmonic
fundamental regardless of inversion — essential for kick drums and sub bass.

**Rebuilt Voice Leading Engine** — Voicings are now solved globally across the
entire progression including wrap-around, producing the kind of smooth contrary
motion and minimal leaps that previously required manual arrangement. The
improvement in voice smoothness is audible. The cost function checks parallel
5ths and octaves across every voice pair, not just adjacent ones, penalizes
register-exchange leaps (opposite-direction leaps of a 4th or more between two
voices), and rewards contrary and stepwise motion. Bar 0 of every progression
anchors to a compact root-position voicing rather than being left to the
solver, so the whole progression starts from a predictable register.

**Seam-Aware Voice Leading** — Mixed-cardinality transitions (a triad into a
4-note chord, a 5-note set into a triad) are now voice-led for real: the
smaller voicing is padded by an optimal monotone alignment — the larger
chord's outer and inner voices lead the smaller's — and the full cost
function runs on the aligned pair. A held chord is strictly cheaper than any
motion. Chroma layers (genP's 5/7-PC strata and modes) route by provenance to
their dedicated engine, whose octave placement now maximises common-tone
overlap so shared tones pedal in register; hand-built scale-sized sets (≥6
voices) safety-route there too. `genFrom` is family-aware — a 4-note (gen4)
source regenerates 4-note bars automatically, and families never mix.
Attempt scoring's voice-leading axis measures the heard surface at full
cardinality, with empirically recalibrated anchors.

**Walking Bass Lines (`lineHarmony`/`walk`)** — A three-pass deterministic
bass-line generator, distinct from the chord voicings above: chord tones land
on beats 1 and 3, a weighted connector pool (diatonic approach, chromatic
leading tones, chord-tone bonuses) fills beats 2 and 4, and the line is built
once as a pure function of the progression rather than sampled per query.
Entropy isn't set by hand — it's derived from the progression itself, blending
root-motion angularity with chord-internal dissonance. Register is fixed to
double bass (E1–C3). Feed it a `genP`-derived progression and it automatically
reweights toward that bar's strata pitches and drops chromatic passing tones
entirely, keeping the line inside the octatripentatonic set.

**Performed-Sequence Walking** — The walk follows the bars the audience
actually hears: `warp` reorderings and `rep` repeats are resolved at eval time
and the line is synthesised over that performed order, so approach tones aim
at the true next chord and a repeated bar walks on rather than photocopying
itself (consecutive repeats alternate root and fifth on beat 1). Non-periodic
selections fall back to stored order with a printed notice.

**Walking-Bass Musicality Overhaul** — Strong beats are consonant anchors: an
explicit beat-3 consonance table (P5, then root, then 3rds) replaces the old
root-motion heuristic, scaled by a progression-level consonance measure so
consonant material walks stricter and dissonant material earns tension
licence. Beat-1 contour gains soft direction persistence, a register-centre
opening anchor, and loop-aware closure; connectors gain Willis's sandwich and
Minor-Thirds passing rules, backed by a Markov-safe regional key inference
that never touches the generation path. Dynamics now steer the walk too:
quieter-than-mean bars lift the register arc, louder bars settle it, and a
sudden drop resets the line to its lowest note.

### Arrangement

**Rewritten `arrange`** — V2 arrangement had two persistent problems: erratic
behaviour at TidalCycles cycle boundaries, and notes crossing a harmony boundary
would generate a spurious new onset rather than sustaining. Both are fixed. Notes
now sustain naturally through harmony changes; new onsets only occur within the
harmonic boundary that the note began in. `arrange` maps the input pattern across
progression states — the pattern runs at its own speed, pitch-mapped to whichever
chord is active at each onset.

**`squeeze`** — An alternative where the pattern plays within each state rather
than across them. Each chord slot gets the full pattern from the start, compressed
to fit its duration. Use when you want per-chord pattern distribution; use
`arrange` when the pattern should flow independently of harmony.

**`warp`** — Defines the chord selection pattern in mininotation:
`warp "[1 2 1 3]/4"` steps through chords 1, 2, 1, 3 over four bars. The
divisor maps directly to physical bars, so the harmonic rhythm is readable at a
glance. Any TidalCycles operator — probabilities, euclidean rhythms, palindromes
— applies. `rep prog 1` auto-derives the selection from a progression's length for
the common case of N bars per chord.

**Overlap** — `overlapF N` (and `overlapB`, `progOverlap`) expand a chord's pitch
set with pitches from neighbouring chords, producing natural sustain and legato
across harmony changes.

### Progression Tools

**Progression Manipulation** — `rotate`, `excerpt`, `transposeP`, `reverse`,
`fuse`, `fuse2`, `interleave`, `expandP` reshape generated harmony in real
time. Position operations `insert`, `switch`, `clone`, `extract` edit individual
bars. `progOverlap`, `progOverlapF`, `progOverlapB` expand a chord's pitch set
with pitches from neighbouring bars for natural sustain. All composable — chain
them for complex transformations.

**Explicit Construction** — Build progressions by hand with `fromChords`
(pitch-class lists), `prog` + `notesToPCs [C, E, G]` (readable note names), or
`fromCadenceStates` (full state construction with explicit root movement).
Define musical sections and assemble them into different formal arrangements.

**Three-Part Filtering** — Overtones, key signatures, and root motion filters
in a single composable chain. Removal syntax (`-Bb'`) subtracts specific
pitches. The filter shapes musical character as much as the notes themselves.

**Regenerate In Place (`genFrom`)** — `genFrom s a b` regenerates only bars `a`
through `b` of an existing progression — 1-indexed and wrap-aware, so
`genFrom s 4 2` on a 5-bar progression regenerates bars 4, 5, 1, 2 and leaves
bar 3 untouched. The cue is auto-inferred from the bar immediately before the
range. Progressions carrying strata/mode layer provenance are spliced
strata-aware, coherently regenerating all three layers across the seam;
legacy triad-only progressions fall back to the original splice-and-fix-seam
behaviour. Composes with `attempt`/`viability` and the rest of the modifier
chain.

### Groove

**subKick** — MPC-style kick and sub bass logic that follows the harmonic root
of the current chord. The low end always locks to the harmony, even as the
progression changes underneath. Complimentary MPC program will be provided.
Sub-bass LED indication (CC 20-31, one per pitch class) is no longer computed
and dispatched from Haskell — it's now derived externally, SC-side, from the
sub channel's outgoing MIDI traffic. Only the kick's high-C indicator (CC 32)
still emits directly from the generation engine.

**Drum Pattern Library** — Around 200 hand-programmed grooves spanning 21
genres — from afro-cuban, dub, and jazz to phonk, techno, and trance — live in
`live/drumpats/`, each playable through the `kgrv` interface on its own
dedicated orbit. `noteoff N` shapes any drum gate pattern's note-length to a
fraction of a bar (or extends it to the next onset if shorter) — pair with
`# legato 1` to hear the cut. `swing8`/`swing16` add proportional swing at the
8th- or 16th-note level for jazz, funk, UK garage, and house feels.

### Form & Kinetics

**Form & Kinetics Framework** — Programs macro-level compositional arc as data.
Forms are defined in wall-clock seconds and loop endlessly, with continuous
interpolation for kinetics and dynamics, and discrete progression switching at
defined nodes. Concise, explicit, long form evolving and dynamic structure.

**`at` / `formK`** — Declare form nodes with `at time kinetics dynamics
progression`, realise into looping TidalCycles patterns with `formK bpm nodes`.
Single-node forms produce constant signals, recreating formless behaviour.

**`at'` / `rh` / `rh'` — snap transitions and bar-based nodes** — Two orthogonal
axes on every form node: time unit (`at` seconds, `rh` rehearsal-mark bars) and
transition (unprimed smooth-ramp, primed `'` snap — hold then jump on the exact
time). Mix freely in one form; `rh'` snaps a bar-aligned scene cut, `at'` a
seconds one. The 12-step display still reads the form in seconds.

**`ki` / `slate` / `withForm`** — Range-gate patterns by kinetics level, combine
gated layers into stacks, and reactively switch progressions as the form
unfolds. Instrument layers activate and deactivate as the signal rises and falls.

**Live Kinetics (`lK`)** — Builds an `IK` context directly from live
`Pattern Double` signals — an LFO, a random walk, an incoming MIDI CC —
instead of a pre-programmed `at`/`rh` timeline, with the same downstream
range-gating and dynamics behaviour as `iK`. `kinPick` dispatches between a
list of patterns by partitioning `[0,1]` into matching windows and playing
whichever pattern the current kinetics signal falls into. `display'` is a
bar-counting variant of `display`, broadcasting loop length and the current
1-indexed bar as MIDI CC rather than elapsed wall-clock seconds.

### Algorithmic Orchestration

**Instrument Functions** — 15 pitched instruments — `flute`, `oboe`,
`clarinet`, `bassoon`, `horn`, `trombone`, `basstrom`, `harp`, `timpani`, and
the full string section — each with physically accurate MIDI range clipping.
Assign a voice and a kinetics range; the instrument handles everything else.

**VoiceLines / Voice** — SATB voice assignment with `8va`, `15va`, `8vb`,
`15vb` octave shifts for fine register placement across the full orchestral
range.

**String Articulations** — `pizz`, `spicc`, `marc`, `legg`, `arco` channel
aliases for instant timbral switching per instrument block.

**Divisi** — Split a section or a single instrument+voice into independent desks
with equal-power (`1/√n`) loudness compensation. Primed `VoiceLines` fields
(`soprano'`, `soprano''` …) hold each desk's line; primed `Voice` constructors
(`Soprano'`, `Bass8vb'`) select it, composing with the octave suffix. `divisi N`
auto-wraps an instrument into `N` tier-reading desks; `# divisi2` / `# divisi3`
are standalone volume tags for hand-built desks that differ in articulation or
entry.

**Motifs** — Recurring material as plain patterns: a `Pattern Bool` rhythm and a
`Pattern Int` contour (voicing degrees that auto-track harmony). `mirror` (melodic
inversion) and `>:<` (combine a rhythm with a fragment) fill the gaps the standard
Tidal developments (`rev`, `slow`, `fast`, `|+`, `<~`, `struct`) leave. A per-piece
one-statement panel binds `rhythm`/`contour`/`motif` (with `'`/`''` tiers); editing
a slot and re-executing reprograms the piece. Clave/bell rhythm shorthands
(`son32`, `rumba32`, `bossa32`, `bellpat32`) live in the boot file (`rumba32` fixed
— it had duplicated `son32`).

**Section Blocks + Timbral Blends** — `wind`, `brss`, `strg`, `perc` group
instruments into sections. `chalumeau`, `pastorale`, `brillante`, `maestoso`,
`tutti` are orchestral colour presets for quick ensemble changes.

**Progressive Crescendo** — Instruments enter as the kinetics signal rises from
0→1, building from solo to full orchestral texture. The Spectral Narrative
drives the orchestration.

### Harmonic Generation API

**Modifier-Based Context API** — `hContext` is a zero-argument chromatic
default. Filters apply as a composable chain: `invSkip 2 $ consonant $ hcKey
"0#" $ hContext`. Comment out individual lines to fall back to defaults.
`hcPedal "C G?"` adds a pedal tone constraint: required tones must appear
in every chord; tones marked `?` are preferred but relaxed when the pool
would fall below a minimum viable size.

**Modifier-Based Gen API** — `gen` is a bare config value composed with
modifiers: `s <- seek "*" $ cue start $ tonal ctx $ len 4 $ entropy 0.3 $ gen`.
The composer string stays visible at the call site.

**Pattern Launcher Paradigm** — Reusable instrument blocks with transformation,
progression, chord selection, and dynamics. Launch and relaunch through a
session with different progressions and contexts.

**Diagnostic Output** — `gen'` and `gen''` print a bar-by-bar summary of the
generation process alongside the progression. Each step shows the state
transition, candidate pool composition (graph vs fallback counts), movement
class, selected chord name, selection source, gamma index, and the top
alternatives at that posterior root. `gen''` adds verbose transform and
advance traces — pitch-class arithmetic and the full render pipeline for
debugging edge cases. Designed to expose the algorithm's decision-making at
a glance without breaking performance flow.

### Performance

**~500× Faster Generation at Runtime** — A deep-dive into Haskell's native
data types and algorithmic complexity produced a ground-up overhaul of the
generation engine's core data paths. The harmonic context was reparsing text
strings ~690 times per generation step; it now uses a pre-parsed `IntSet` for
O(1) membership. Chord chain construction was O(n²) list append; it is now
constant-time. The random number generator was opening `/dev/urandom` ~4,600
times per 8-chord progression; it is now a single shared handle. The voice
leading solver moved from O(n) list indexing to O(1) with `Data.Vector`. These
improvements compound: generation that once felt sluggish or impractical now
runs immediately.

**400× Faster in Full Orchestral Mode** — The generation engine was completely
rewritten: voicings are pre-computed once at pattern construction time rather
than solved per frame. With 16+ stacked instrument calls, voice leading work
drops from ~800 solver calls per second to 2–3.

**~30× Faster Online Generation** — Wildcard (`"*"`) generation now reads each
edge's pre-aggregated corpus score directly instead of parsing the full
per-composer weights payload: ~115× less allocation and ~30× less time per
step, with byte-identical musical output. This is the default seek mode, so
every online session feels it — especially multi-attempt searches
(`attempt N K`).

### Infrastructure

**Neo4j 5.26 + HTTP Query API** — The database moved from the end-of-life
Neo4j 4.4 to the 5.26 LTS, and the Haskell side moved from the Bolt binary
protocol (whose last maintained Haskell driver spoke a protocol modern servers
no longer accept) to Neo4j's supported HTTP Query API. Same credentials, same
`docker compose up -d neo4j`, one fewer protocol to care about — and the
database ports now bind to localhost only.

**A 14MB Composer Graph** — The published corpus artefact shrank from a
projected ~350MB to 14MB: 99% of the stored weight entries were zeros (every
edge carried every composer), and dropping them is provably score-neutral —
verified end-to-end with zero drift across every composer blend tested. Fetch
the pre-built graph from the `corpus-v1` release and load it with one command.

### Testing

**Comprehensive Test Suite** — V3 ships with a 13-module HSpec and QuickCheck
test suite covering the full library: Z₁₂ pitch-class algebra, chord naming,
progression monoid laws, filter and overtone constraints, dissonance scoring,
voice leading costs, composer weight parsing, probabilistic selection, the
generation engine, the TidalCycles bridge, groove interface, and form/kinetics
framework. Property-based tests verify algebraic invariants across arbitrary
inputs — pitch wrap, transposition, voice leading cost bounds. The suite runs
on every change and provides a stable foundation for ongoing development.

__________________________________________________________________________________

## Version 2.0.0 has arrived! (2022)

Version 2.0.0 takes the algorithm out of the terminal and into the concert
hall or studio. Two architectural changes make this possible:

The in-memory Markov tables have been replaced by a Neo4j graph database as a
persistent backend. Cadence transitions from the Yale Classical Archives Corpus
are stored as transposition-invariant zero-form nodes — fast enough for
adjacency-based traversal in an interpreted environment that the algorithm can
now run inside TidalCycles without interrupting the performance. The graph is
populated once from the corpus CSV data and queried at pattern time.

The initial TidalCycles interface allows The Harmonic Algorithm to be
interacted with during a live session. Generative functions, the launcher,
and performance interfaces allow generated progressions to be patterned and
transformed in real time — chords becoming musical patterns that can be
manipulated, layered, and interracted with within a TidalCycles setup.

## Version 1.1.1

More intelligent naming logic with regard to slash chord notation.
Removed some system specific path dependencies.

## Version 1.1.0

Version 1.1.0 makes a few refinements to the codebase as well as introducing
a new feature -- Random Sequences!

Random sequences allow the performer/composer to traverse deterministic space
(move through musical cadences!) at a much faster rate and give a 'higher up'
viewpoint to the character and nature of harmonic motion in a given musical
context. The performer/composer can then 'jump in' to any point of the
generated sequence and move through musical space in 'blocks' of harmony.

__________________________________________________________________________________

## Version 1.0.0 is complete! (2018)

The Harmonic Algorithm 1.0.0 implements a generation, filtering and
exploration algorithm for triadic musical data, with a focus on composing
with the overtones of an instrument and scope for use in traditional
composition, instrumental study or even live performance.

This functionality is augmented by a conceptually complete Markov Chain
Machine Learning implementation, trained on Bach Chorale harmonisation data
retrieved from the UCI Machine Learning Repository (Dua, D., Karra Taniskidou,
E., 2017 http://archive.ics.uci.edu/ml).

In version 1.0.0 of The Harmonic Algorithm, a command line interface is
provided for interaction with the underlying musical and numerical algorithms.
