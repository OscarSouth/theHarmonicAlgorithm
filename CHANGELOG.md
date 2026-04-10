# Changelog for theHarmonicAlgorithm

__________________________________________________________________________________

## Version 3.0.0 is here! (2026)

Version 3.0.0 is a complete rebuild. The Harmonic Algorithm has grown into a
live performance instrument built around three interlocking systems: **The
Harmonic Algorithm** — the R→E→T generation engine, now capable of channelling
and blending the harmonic sensibilities of over 80 composers from the Yale
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
output.

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
Combine with key and root filters to shape the path.

**Inversion Spacing** — `invSkip N` enforces a minimum number of root-position
chords between any two inversions — direct control over harmonic density and
stability across a progression.

**`lead`** — Construct a starting state from a readable string: `lead "E min
(5)"` gives E minor arrived at by ascending five semitones. Root, quality, and movement are
each optional, falling through to random when omitted.

### Voicing

**Five Voicing Strategies** — `flow` finds the smoothest path through the full
progression using cyclic dynamic programming. `grid` keeps the root locked in
the bass with optimised upper voices. `lite` gives the raw intervals. `root`
extracts the root as a melodic line. `fund` always returns the harmonic
fundamental regardless of inversion — essential for kick drums and sub bass.

**Rebuilt Voice Leading Engine** — Voicings are now solved globally across the
entire progression including wrap-around, producing the kind of smooth contrary
motion and minimal leaps that previously required manual arrangement. The
improvement in voice smoothness is audible.

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
`fuse`, `expandP` reshape generated harmony in real time. All composable — chain
them for complex transformations.

**Explicit Construction** — Build progressions by hand with pitch-class lists or
readable note names (`notesToPCs [C, E, G]`). Define musical sections and
assemble them into different formal arrangements.

**Three-Part Filtering** — Overtones, key signatures, and root motion filters
in a single composable chain. Removal syntax (`-Bb'`) subtracts specific
pitches. The filter shapes musical character as much as the notes themselves.

### Groove

**subKick** — MPC-style kick and sub bass logic that follows the harmonic root
of the current chord. The low end always locks to the harmony, even as the
progression changes underneath. Complimentary MPC program will be provided.

### Form & Kinetics

**Form & Kinetics Framework** — Programs macro-level compositional arc as data.
Forms are defined in wall-clock seconds and loop endlessly, with continuous
interpolation for kinetics and dynamics, and discrete progression switching at
defined nodes. Concise, explicit, long form evolving and dynamic structure.

**`at` / `formK`** — Declare form nodes with `at time kinetics dynamics
progression`, realise into looping TidalCycles patterns with `formK bpm nodes`.
Single-node forms produce constant signals, recreating formless behaviour.

**`ki` / `slate` / `withForm`** — Range-gate patterns by kinetics level, combine
gated layers into stacks, and reactively switch progressions as the form
unfolds. Instrument layers activate and deactivate as the signal rises and falls.

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

**Modifier-Based Gen API** — `gen` is a bare config value composed with
modifiers: `s <- seek "*" $ cue start $ tonal ctx $ len 4 $ entropy 0.3 $ gen`.
The composer string stays visible at the call site.

**Pattern Launcher Paradigm** — Reusable instrument blocks with transformation,
progression, chord selection, and dynamics. Launch and relaunch through a
session with different progressions and contexts.

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
