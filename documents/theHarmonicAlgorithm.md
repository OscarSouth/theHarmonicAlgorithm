# The Harmonic Algorithm: From Overtone Physics to Computational Live Performance

**Oscar South, 2016--2026**

---

## Abstract

This document synthesizes a decade of research spanning three academic works and a production software system. *The Harmonic Algorithm* (2016) established an exhaustive combinatorial methodology for mapping overtone harmonics on the Electric Contrabass Cittern. *Harmonic Algorithm Reflections* (2016) documented the technique developments that emerged from its practice. *Data Science In The Creative Process* (2018) implemented the algorithm computationally in Haskell, diagnosing its manual limitations through Wiggins' Creative Systems Framework and resolving them via Markov-chain probabilistic traversal. The V3 system (2026) extends these foundations into a live performance platform integrating Neo4j graph database, TidalCycles live coding, cyclic dynamic programming voice leading, and entropy-controlled probabilistic generation. Together, these works trace a path from the physical overtone series of a single instrument to a general-purpose computational system for harmonic composition in real time.

---

## 1. Introduction

### 1.1 The Electric Contrabass Cittern

The instrument at the centre of this research is a five-string bass guitar in a non-standard tuning: **E A e G B** (lowercase denoting a higher octave string), referred to throughout as *Electric Contrabass Cittern* tuning or *ecbc*. A Hipshot D-Tuner on the highest string enables dynamic retuning between B and C during performance, yielding two composite overtone series: **EAeGB** and **EAeGC**.

The tuning is built from irregular intervals (fourth, fifth, flat third, natural third) and was originally designed for two-handed tapping techniques. The upper three strings (e, G, B) form a minor triad in root position, allowing access to any major or minor triadic inversion within a two-fret span while retaining standard E and A strings for bass movement. This study extends that principle to natural harmonics rather than stopped notes.

Additional preparations include:

- **Hipshot D-Tuner**: Placed on the highest string (not the conventional lowest), providing access to a second series of natural harmonics without manual retuning. The B/C configuration was chosen after testing B/C/D (insufficient range) and A#/B/C (impractical upkeep).
- **Soft muting device**: Placed at the nut to dampen unused strings, with the secondary effect of transforming sustained harmonics into short, percussive tones reminiscent of pitched percussion.
- **Piezo/magnetic pickup blend**: Primarily piezo signal with magnetic blend for body, enabling clean amplification of harmonics and effective E-Bow performance without the distortion caused by magnetic pickup proximity.

### 1.2 Research Timeline

| Year | Work | Focus |
|------|------|-------|
| 2016 | *The Harmonic Algorithm* | Exhaustive overtone combinatorial analysis |
| 2016 | *Harmonic Algorithm Reflections* | Technique development, The Parting Glass |
| 2018 | *Data Science In The Creative Process* | Haskell implementation, Markov models |
| 2026 | V3 system (`theHarmonicAlgorithm`) | Neo4j, TidalCycles, live performance |

---

## 2. The Overtone Paradigm (2016)

### 2.1 Tuning Systems and Preparations

Three tuning systems are analysed throughout the thesis:

| Tuning | Strings | Context |
|--------|---------|---------|
| **EAeGB** | E2 A2 E3 G3 B3 | Primary: Cittern with B position |
| **EAeGC** | E2 A2 E3 G3 C4 | Secondary: Cittern with C position |
| **EADG** | E1 A1 D2 G2 | Standard electric bass |

The temperament analysis in the thesis demonstrates the tension between equal temperament (required for stopped-note intonation across all keys) and just intonation (which produces more in-tune harmonics, particularly thirds). For the ecbc tuning, optimum temperament and string tension were calculated specifically for harmonic performance.

### 2.2 Composite Overtone Series

Each string's natural harmonic series follows the physical overtone series, approximated in equal temperament as:

| Overtone | Interval from fundamental | Semitone offset |
|----------|--------------------------|-----------------|
| OT1 | Unison (fundamental) | 0 |
| OT2 | Perfect fifth | 7 |
| OT3 | Major third | 4 |
| OT4 | Minor seventh | 10 |
| OT5 | Major second | 2 |

The *Composite Overtone Series* is the union of all overtones available across all strings in a given tuning. For EAeGB, with fundamentals E(4), A(9), e(4), G(7), B(11):

- **E string**: E, B, G#, D, F#
- **A string**: A, E, C#, G, B
- **e string**: E, B, G#, D, F# (same pitch classes as E, higher octave)
- **G string**: G, D, B, F, A
- **B string**: B, F#, D#, A, C#

This yields a rich chromatic palette with extensive pitch-class redundancy across strings---a property that becomes crucial for practical performance, as each pitch may be producible from multiple string/node combinations.

### 2.3 The Harmonic Algorithm: Exhaustive Combinatorial Analysis

The titular algorithm is a systematic method for charting all three-note overtone combinations over twelve chromatic bass notes, for each tuning system. The resulting data charts---large matrices with bass notes on one axis and triad combinations on the other---catalogue every possible harmonic voicing available on the instrument.

Each cell in the chart records:

- The resulting chord name (e.g., "Em", "D6no5", "F#7sus4")
- The overtone source annotation (e.g., "E3/e1", "A2/G4", "b1+3")
- A quality classification (Simple, Complex, or Impractical)

The annotation notation follows the convention: **string name + overtone number**. The "/" separator indicates alternative sources (e.g., "E3/e1" means the pitch can come from E string overtone 3 *or* e string overtone 1). The "+" connector indicates multiple overtone numbers from the same string producing the same pitch class.

### 2.4 Harmonic Extrapolation

The charts reveal voicings spanning conventional harmony (major, minor, diminished triads) through extensions (add9, sus4, 7ths) to complex structures that arise naturally from overtone combinations (e.g., "C#7sus4b9no5", "D#-Mj#5"). These are classified into three categories:

- **Simple**: Practically useful, musically conventional voicings
- **Complex**: Usable but requiring careful musical context
- **Impractical**: Technically possible but musically or physically unwieldy

### 2.5 Semi-Diatonicism

A significant finding is that the overtone-derived pitch sets are *semi-diatonic*: they overlap substantially with diatonic key signatures but include chromatic pitches that create characteristic colour. The three focus keys (D, G, C major) were chosen because their diatonic pitch content has the greatest overlap with the EAeGB composite overtone series.

### 2.6 The Melodic Paradigm

The melodic exercises (Audio Examples 3--26) establish fundamental fluency:

- **Key signatures** (Audio 3--8): D, G, C major scales performed descending through the instrument's range, first using only natural harmonics (revealing available pitches), then supplemented with artificial harmonics to complete each scale.
- **Pentatonic scales** (Audio 9--13): Five pentatonic scales spanning the three focus keys (D/Bm, G/Em, C/Am) plus neighbouring keys (A/F#m, F/Dm).
- **Diatonic arpeggios** (Audio 14--26): All diatonic triads from D, G, and C major as individual broken chords.

### 2.7 The Harmonic Paradigm

The harmonic exercises (Audio Examples 27--66) develop chordal vocabulary:

- **Chord scales by seconds** (Audio 27--32): Diatonic triads moving stepwise through D, G, C in ascending and descending forms.
- **Movement by thirds** (Audio 33--38): Triads at diatonic third intervals (I-iii-V-vii-ii-IV-vi) through each key.
- **Movement by fourths** (Audio 39--44): Triads at diatonic fourth intervals (I-IV-vii-iii-vi-ii-V) through each key.
- **ii-V-I cadences** (Audio 45--55): Two-Five-One progressions resolving to every major and minor target chord across the three keys, employing secondary dominants and overtone-specific voicings.
- **Diminished resolutions** (Audio 56--66): The three unique diminished seventh chords (C dim7, F dim7, Bb dim7) resolving by ascending semitone to each target, exploiting the enharmonic symmetry of diminished structures.

---

## 3. Reflections and Technique (2016)

### 3.1 Evolution of the Data Charts

The Reflections document chronicles multiple iterations of the Harmonic Algorithm charts. Early versions presented triads "as derived" from combinatorial analysis, with all inversions listed per bass note. Later iterations reorganised the data by flipping axes (placing bass notes vertically, triads horizontally) and ordering triads by ascending interval size. A system for showing overtone source locations was added, culminating in the final charts published in the thesis.

A fourth chart (for the D position of the re-tuner) was completed but omitted from the final work when the D position proved physically impractical.

### 3.2 Three Point Playing and the Pass Technique

The "pass" technique emerged from a technical roadblock: operating the re-tuner lever while simultaneously playing a new bass note. The solution involves transferring the fretting role from left hand to right, tapping the next bass note with the forefinger while freeing the left hand for the lever. The resulting articulation on "the pass" creates a musical effect where the bass note changes as higher overtones enter through portamento.

### 3.3 Re-Tuner Movements in Performance

The re-tuner shifts specific overtones by one semitone:

- B(11) to C(0)
- F#(6) to G(7)
- D#(3) to E(4)

This creates smooth chromatic voice leading within sustained harmonics---the musical effect itself of these transitions is noted as a significant performance feature beyond mere pitch access.

### 3.4 The Artificial Harmonic Toolbox

Rather than choosing artificial harmonics ad hoc, a systematic repertoire was established for combining with natural harmonics. The Reflections document catalogues specific fret/node combinations for each string, specifying overtone number and resulting pitch. This "toolbox" approach enables fluent integration of natural and artificial harmonics in performance.

### 3.5 Overtone 5 Discovery

A significant discovery during practice was the existence of usable nodes for Overtone 4 and Overtone 5 in the area above the 24th fret. On the G string, Overtone 5 produces a D pitch that is particularly useful for higher melodic playing. While not suitable for tapped harmonics and therefore not formally incorporated into the Harmonic Algorithm charts, this extension provides valuable melodic freedom in the upper register.

### 3.6 The Parting Glass

The traditional melody "The Parting Glass" was arranged as a musical application exercise, scored for three versions in the relative minors of the thesis's three focus keys:

| Version | Key | Relative Major |
|---------|-----|----------------|
| 1 | B minor Aeolian | D major |
| 2 | E minor Aeolian | G major |
| 3 | A minor Aeolian | C major |

The score is written in 3/4 time, marked "Freely & Expressively", in bass clef with diamond noteheads for harmonics and regular noteheads for stopped bass notes. Overtone source annotations appear below the staff using the thesis notation system. All three versions follow the same 16-bar harmonic structure, transposed to each key.

---

## 4. Computational Implementation (2018)

### 4.1 Wiggins' Creative Systems Framework

The 2018 paper adopts Wiggins' Creative Systems Framework as both analytical lens and architectural blueprint. The framework describes a creative system as a triple **\<R, T, E\>** operating within a universe **U** of possible concepts, expressed through a language **L**:

- **R** (Rules): Constraints that define the valid conceptual space
- **T** (Traversal): Strategy for locating concepts within U
- **E** (Evaluation): Criteria for judging the quality of located concepts

### 4.2 Generative Uninspiration and Its Resolution

Applying the framework to the original Harmonic Algorithm reveals a diagnosis: the manual chart-based workflow suffers from **Generative Uninspiration**---the inability of the creative agent to effectively find valued concepts within a theoretically sound conceptual space. The R component (the charts) is well-formed, but the T component (manual lookup in large spreadsheets) is inadequate for practical use.

Wiggins prescribes transformation of T as the remedy. The 2018 implementation achieves this through:

1. **Automated R**: The `Overtone` module generates pitch sets programmatically, replacing manual chart consultation.
2. **Probabilistic T**: A Markov model trained on the Yale Classical Archives Corpus (YCACL) Bach chorales provides instantaneous ranked recommendations for next states.
3. **Interactive E**: The user retains creative control through interactive filters and selection.

### 4.3 The MusicData Module (Pitch Class Algebra as Z12)

Musical pitch is modelled as arithmetic modulo 12 (the integers of Z12), implementing a `MusicData` class that redefines standard arithmetic operators for musical data. This design decision solves a critical modelling problem: representing harmonic movement as *relative* rather than *absolute*.

A cadence of Dm to G (root movement ascending 5 semitones) is musically identical to F#m to B transposed up four semitones. Treating each literal state as unique would:

- Artificially inflate probabilities toward keys preferred in training data
- Produce different predictions for different instruments/tunings
- Massively inflate the transition matrix
- Lose information through treating identical movements as unrelated

The solution: all analysis operates on relative cadence representations (zero-form intervals), converted to concrete chord representations only for display.

### 4.4 The Markov Module

The Markov module processes the YCACL dataset into a transition matrix: a mapping from current state to a probability-weighted list of possible next states. The type signature is:

```
Map CadenceKey [(CadenceKey, Double)]
```

When no transitions exist from a current state (an empty row), the system treats this as a self-transition---movement into an identical state. This avoids probability rows that don't sum to 1 and maps to Wiggins' concept of "Hopeless Uninspiration": an empty conceptual space. The solution is acceptable because such states are extremely rare and the user can always manually specify a movement.

### 4.5 The Overtone Module (Programmatic R)

The Overtone module is the computational realisation of the thesis's combinatorial charts. It parses user-input musical syntax to generate sets of theoretically possible overtone combinations, serving as the programmatic bridge between domain-expert user input and the machine-readable backend.

### 4.6 Monadic Computation and Sequential State

The system uses Haskell's monadic composition to model the inherently sequential nature of harmonic progression---moving from state to state under defined rules. The Reader monad provides access to the trained Markov model throughout the interactive session, while the IO monad handles user interaction. This architecture directly mirrors the musical reality: a progression unfolds as a sequence of state transitions, each influenced by its predecessor.

---

## 5. V3: The Live Performance System (2026)

### 5.1 Architecture: R-E-T Pipeline in Four Layers

The V3 system reorganises the \<R, T, E\> framework into four architectural layers:

| Layer | Name | Role | Modules |
|-------|------|------|---------|
| A | Memory | R (Rules/Constraints) | Import (CSV, Transform, Graph), Filter, Overtone |
| B | Brain | Types and algebra | Pitch, Harmony, Progression, Dissonance, VoiceLeading |
| C | Hands | E-T (Evaluation, Traversal) | Query, Markov, Probabilistic, Builder |
| D | Voice | Interface | Bridge, Arranger, Groove, Instruments, Form |

Layer boundaries enforce a dependency hierarchy: B imports nothing from C or D; C may import from B but not D; D may import from B and C.

### 5.2 Neo4j Graph Database (YCACL Corpus)

The CSV-based transition matrix from the 2018 implementation is replaced by a **Neo4j graph database**. Each cadence state is a node; each observed transition is a weighted, directed edge. Edge weights encode transition probabilities, with composer attribution enabling weighted blending:

- `"*"` aggregates all composers (sum of edge weights)
- `"bach"` filters by single composer
- `"bach:30 debussy:70"` blends multiple composers with weighted scoring

The graph structure enables native traversal algorithms and eliminates the need to load the entire transition matrix into memory.

### 5.3 TidalCycles Integration and the Arrange Interface

The V3 system integrates with **TidalCycles**, a domain-specific language for live coding music in Haskell. The `Interface.Tidal` modules provide:

- **Bridge**: Pattern-level lookup functions (`lookupChord`, `arrange`, `warp`) that map TidalCycles patterns to progression states
- **Arranger**: Progression combinators (`rotate`, `excerpt`, `fuse`, `transpose`) and voicing paradigms (`flow`, `root`, `grid`, `lite`, `literal`)
- **Form/Kinetics**: A system for structuring progressions over time (`at`, `formK`, `ki`, `slate`)
- **Groove**: Sub-bass and percussion integration (`subKick`, `fund`)

The `arrange` function is the primary interface between harmonic state and TidalCycles pattern, applying voicing functions to progression data and outputting MIDI-compatible note patterns.

### 5.4 Voice Leading Optimisation (Cyclic Dynamic Programming)

The `VoiceLeading` module implements **cyclic dynamic programming** for optimal voice leading across progressions. Given that a progression cycles, the algorithm finds the voicing sequence that minimises total voice movement while satisfying the constraint that the last chord must voice-lead smoothly back to the first.

Key functions:

- `voiceLeadingCost`: Semitone-distance cost between two voicings
- `cyclicCost`: Total cost including the wrap-around seam
- `solveFlow`: Optimal flow voicing (minimal movement)
- `solveRoot`: Optimal root-position voicing

### 5.5 Hindemith Dissonance Scoring

Vertical dissonance is scored using a model derived from **Hindemith's *The Craft of Musical Composition*** (1937). Interval classes are ranked by consonance:

| Rank | Interval | Quality |
|------|----------|---------|
| 1 | Unison/Octave | Most consonant |
| 2 | Perfect fifth | |
| 3 | Perfect fourth | |
| 4 | Major third/sixth | |
| 5 | Minor third/sixth | |
| 6 | Tritone | Most dissonant |

This ranking drives the **consonance drift** feature: when `dissonant` or `consonant` is applied to a `HarmonicContext`, the generation engine filters candidates so that only chords with equal or greater (or lesser) dissonance than the current chord are eligible at each step.

### 5.6 Form and Kinetics Framework

The Form/Kinetics system (`FormNode`, `Kinetics`, `at`, `formK`) enables multi-section musical forms where different progressions occupy different time regions. The `at` function places a progression at a specific position with velocity and repetition parameters; `formK` compiles a list of form nodes into continuous kinetics signals that drive pattern selection over time.

### 5.7 Entropy as Creativity Parameter (Gamma Distribution)

The generation engine uses a **gamma distribution** to sample candidate indices from a scored pool. The `entropy` parameter (shape parameter of the gamma distribution) controls the degree of randomness:

- **Low entropy** (0.1--0.3): Samples near the top of the ranked pool, producing conventional, predictable progressions
- **High entropy** (0.5--1.0): Samples more uniformly, producing unexpected harmonic turns and distant modulations

This parameter maps directly to the T component of the Creative Systems Framework---it controls *how* the system traverses the conceptual space defined by R.

### 5.8 Composer Blending (Weighted Graph Traversal)

The composer specification system enables stylistic control by weighting graph edge scores:

```haskell
seek "bach:30 debussy:70" $ gen    -- 30% Bach, 70% Debussy
seek "bach" $ gen                  -- Pure Bach style
seek "*" $ gen                     -- All composers equally
```

Edge weights from each composer are multiplied by their blend coefficient before scoring, creating a composite traversal bias that blends multiple compositional voices.

---

## 6. From Theory to Practice

### 6.1 Overtone Annotation: Bringing the Charts into Code

The V3 system includes an **overtone annotation** feature that brings the thesis's rightmost chart columns into the live coding environment. When overtones are declared in `hcOvertones` (e.g., `"E A D G"`), the `gen'` diagnostic output annotates each pitch with its possible overtone sources:

```
  3  | G maj  [G, B, D]  {G: E2/A3, B: E2/G4, D: A3/G2}
```

The reverse mapping algorithm: for each chord pitch `p` and each string fundamental `f`, if `(p - f) mod 12` matches one of the overtone offsets `[0, 7, 4, 10, 2]`, then `p` can be produced from that string at the corresponding overtone number. The "/" separator preserves the thesis notation for alternative sources.

### 6.2 The 68 Audio Examples in TidalCycles

The file `live/theHarmonicAlgorithm.tidal` encodes all 64 musical exercises from the thesis (Audio Examples 3--66) as TidalCycles-compatible data structures. Each example is a named binding that can be loaded as a `Progression`:

```haskell
s = prog ex45    -- ii-V-I cadence resolving to D major
```

This transforms the static thesis charts into a playable, composable performance library---completing the arc from manual spreadsheet to live instrument.

### 6.3 Performance: The Parting Glass

The file `live/thePartingGlass.tidal` realises the three arrangements from the Reflections score as TidalCycles performance pieces. Each 16-bar arrangement is encoded using note-name constructors, with form selection by key:

```haskell
s = stateEm    -- Em Aeolian version (default)
s = stateBm    -- Bm Aeolian version
s = stateAm    -- Am Aeolian version
```

The melody is encoded as scale-degree mininotation patterns, with the harmony providing both the chord voicing source and the scale context for melodic degrees.

### 6.4 The Live Coding Workflow

A typical V3 live coding session:

1. **Define context**: Set overtone filters, key, root constraints via `hcOvertones`, `hcKey`, `hcRoots`
2. **Generate**: Use the modifier-based API (`gen`, `gen'`, `gen''`) with entropy and composer blend
3. **Arrange**: Apply voicing paradigms (`flow`, `root`, `grid`) through the `arrange` function
4. **Form**: Structure sections with `at` and `formK`
5. **Perform**: Evaluate live in TidalCycles, modifying parameters in real time

---

## 7. Context: Electric Bass Overtone Performance

### 7.1 Jaco Pastorius

Jaco Pastorius established the vocabulary of harmonics on electric bass. His *Modern Electric Bass* (2001) demonstrates natural harmonics as a compositional and improvisational tool, most famously in "Portrait of Tracy" where harmonics form the primary melodic and harmonic material. The Harmonic Algorithm extends Pastorius's harmonic vocabulary from ad hoc discovery to systematic enumeration.

### 7.2 Michael Manring

Michael Manring's work with multiple Hipshot D-Tuners on a custom Zon Hyperbass represents the most extreme exploration of altered tunings in bass performance. His album *Soliloquy* (2005) demonstrates the musical possibilities of dynamic retuning during performance. The ecbc tuning's single D-Tuner takes direct inspiration from Manring's approach, applied to overtone rather than stopped-note technique.

### 7.3 Steve Bailey and Victor Wooten

*Bass Extremes* (Bailey & Wooten, 1993) popularised artificial harmonics technique on electric bass. Bailey's systematic approach to artificial harmonics---touching a node point while plucking with the same hand---provides the technical foundation for the "artificial harmonic toolbox" described in the Reflections document.

### 7.4 Victor Wooten: The Music Lesson

Wooten's *The Music Lesson* (2006) advocates an intuitive, holistic approach to musical practice. This philosophy influenced the thesis's emphasis on practice-led research: the algorithm exists to serve musical exploration, not to replace it. The computational system augments the performer's intuition rather than constraining it.

### 7.5 Alex McLean: TidalCycles

Alex McLean's TidalCycles (originating from *Improvising with Synthesised Vocables*, 2007) provides the live coding environment for V3. TidalCycles' pattern-based approach to music---where musical structures are defined as functions over time---maps naturally to the progression-as-sequence model of the Harmonic Algorithm. The `arrange` function bridges the harmonic generation engine with TidalCycles' pattern system.

### 7.6 Paul Hindemith: The Craft of Musical Composition

Hindemith's consonance ranking of interval classes provides the theoretical foundation for the dissonance scoring system. His hierarchy (unison > fifth > fourth > major third > minor third > tritone) is encoded directly in the `Dissonance` module and drives both the absolute dissonance score of chords and the consonance/dissonance drift feature of the generation engine.

---

## 8. Conclusion

### 8.1 From Manual Charts to Computational Live Performance

The arc of this research traces a clear progression:

1. **Physical analysis** (2016): Exhaustive mapping of what is *possible* on the instrument
2. **Practice integration** (2016): Discovery of what is *useful* and development of technique to perform it
3. **Computational automation** (2018): Diagnosis and resolution of the manual system's limitations through probabilistic traversal
4. **Live performance system** (2026): Integration with live coding infrastructure for real-time compositional use

At each stage, the core question remains the same: given the constraints of a physical instrument and the laws of tonal harmony, how can a performer navigate the vast space of harmonic possibility in a way that is both musically satisfying and practically fluent?

The Creative Systems Framework provides the vocabulary for this question. The original manual charts define a valid R but suffer from inadequate T. The 2018 Markov model transforms T while preserving R. The V3 system enriches all three components: R gains key/overtone/root filters with consonance drift; T gains entropy control and composer blending; E gains Hindemith dissonance scoring and cyclic voice leading optimisation.

### 8.2 Future Directions

The system's architecture---separating Rules, Evaluation, and Traversal into distinct layers with clean boundaries---supports continued evolution. The graph database can incorporate additional corpora beyond Bach chorales. The overtone annotation system connects the computational output back to the physical instrument. The TidalCycles integration enables performance contexts that the original thesis could not have anticipated.

The fundamental insight persists from the thesis's foreword: "Music requires a human component... analysis comes from music. The purpose of analysis is not to search for any new or innovative musical discovery but to reveal possibilities which can be used in practice, throughout the human creative process of exploration and discovery."

---

## References

### Primary Sources

- South, O. (2016). *The Harmonic Algorithm*. MA thesis, University of Chester.
- South, O. (2016). *Harmonic Algorithm Reflections*. University of Chester.
- South, O. (2018). *Data Science In The Creative Process: Composing With Functions*. Higher Diploma thesis, Dublin Business School.

### Instrument and Technique

- Pastorius, J. (2001). *Modern Electric Bass*. Manhattan Music Inc.
- Manring, M. (2005). *Soliloquy* [Album liner notes]. Manthing Music.
- Manring, M. (1998). *Michael Manring* [Instructional DVD]. Hal Leonard Corporation.
- Bailey, S. & Wooten, V. (1993). *Bass Extremes*. Alfred Publishing.
- Pear, D. (1999). *Bass Harmonics: New Concepts and Techniques*. Alfred Music.
- Wooten, V. (2006). *The Music Lesson*. Berkley Publishing Group.
- Prestia, F. R. & Schaub, A. S. D. (1993). *Fingerstyle Funk with Francis Rocco Prestia*.

### Music Theory and Composition

- Hindemith, P. (1937). *The Craft of Musical Composition*. Schott.
- Levine, M. (1995). *The Jazz Theory Book*. Sher Music.
- Rawlins, R. & Bahha, N. E. (2005). *Jazzology*. Hal Leonard Corporation.
- Bernstein, L. (1976). *The Unanswered Question*. Harvard University Press.

### Computational Creativity and Live Coding

- Wiggins, G. A. (2001). *Towards a More Precise Characterisation of Creativity in AI*. Department of Computing, City University, London.
- Wiggins, G. A. (2003). *Categorising Creative Systems*. Centre for Computational Creativity, City University, London.
- McLean, A. (2007). *Improvising with Synthesised Vocables, with Analysis Towards Computational Creativity*. Goldsmiths College, University of London.
- Candy, L. (2006). *Practice Based Research: A Guide*. CCS Report 2006-V1.0, University of Technology Sydney.

### Programming and Data Science

- Lipovaca, M. (2012). *Learn You a Haskell For Great Good!* No Starch Press.
- O'Sullivan, B., Stewart, D. & Goerzen, J. (2012). *Real World Haskell*. O'Reilly.
- Boespflug, M., Dominguez, F. & Vershilov, A. (2014). *Project H: Programming R in Haskell*.

---

## Appendix A: The Overtone Series

The harmonic series of a vibrating string produces frequencies at integer multiples of the fundamental. In equal temperament, these are approximated as:

| Partial | Ratio | ET Interval | Semitones | Deviation |
|---------|-------|-------------|-----------|-----------|
| 1 | 1:1 | Unison | 0 | 0 cents |
| 2 | 2:1 | Octave | 12 | 0 cents |
| 3 | 3:1 | Octave + P5 | 19 | +2 cents |
| 4 | 4:1 | 2 Octaves | 24 | 0 cents |
| 5 | 5:1 | 2 Oct + M3 | 28 | -14 cents |
| 6 | 6:1 | 2 Oct + P5 | 31 | +2 cents |
| 7 | 7:1 | 2 Oct + m7 | 34 | -31 cents |

The V3 system uses only the first five *unique pitch classes* (octave-reduced and deduplicated): fundamental (0), P5 (7), M3 (4), m7 (10), M2 (2). Overtone 5 (the 9th partial, producing M2) was discovered during practice as usable above the 24th fret.

## Appendix B: Tuning Charts

The complete tuning charts for EAeGB, EAeGC, and EADG are provided as separate PDF documents:

- `documents/EAeGb.pdf` --- Electric Contrabass Cittern (B position)
- `documents/EAeGC.pdf` --- Electric Contrabass Cittern (C position)
- `documents/EADG.pdf` --- Standard electric bass

## Appendix C: The Creative Systems Framework

Wiggins' Creative Systems Framework describes a creative system as a triple **\<R, T, E\>**:

| Component | Definition | In V3 |
|-----------|-----------|-------|
| **R** | Rules defining valid conceptual space | `HarmonicContext`: overtone filters, key, roots, drift, inversion spacing |
| **T** | Traversal strategy for locating concepts | Gamma-distributed sampling with composer-weighted graph traversal |
| **E** | Evaluation of concept quality | Hindemith dissonance scoring, Markov transition probabilities |
| **U** | Universe of possible concepts | All cadence states in the Neo4j graph |
| **L** | Language for expressing rules | Haskell + TidalCycles mininotation |

Wiggins identifies several failure modes:

- **Generative Uninspiration**: T cannot locate valued concepts within a sound R (the original manual algorithm's limitation)
- **Conceptual Uninspiration**: E cannot select valued concepts within the space defined by R
- **Hopeless Uninspiration**: R defines an empty conceptual space

The V3 system addresses all three: the probabilistic traversal resolves Generative Uninspiration; entropy control and composer blending refine Conceptual evaluation; and filter validation prevents empty R spaces.

## Appendix D: Pitch Class Algebra (Z12)

Musical pitch classes form the cyclic group Z12 under addition modulo 12:

```
PitchClass = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}

C=0, C#=1, D=2, D#=3, E=4, F=5, F#=6, G=7, G#=8, A=9, A#=10, B=11
```

Key operations:

- **Transposition**: `transpose n p = (p + n) mod 12`
- **Interval**: `interval p q = (q - p) mod 12`
- **Zero-form normalisation**: Translate a pitch-class set so the lowest element is 0

All cadence objects in V3 store intervals in zero-form (relative, pitch-agnostic). This is the "zero-form invariant" enforced at Neo4j query results, `toCadence` conversion, and fallback generation. Concrete pitch classes are computed at runtime by adding the current root.
