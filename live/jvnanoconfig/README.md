# JV-1010 ↔ NanoSynth level calibration

The studio orchestra (Alesis NanoSynth) and the home orchestra (Roland JV-1010)
play the same files through one rig seam (`rig` in `live/BootTidal.hs`,
`documents/NANOSYNTH.md`, `documents/ALGORITHMIC_ORCHESTRATION.md`). The notes
land; the balance does not. The nano's mix is the 2021 concert's, tuned by ear
over months and held in code; the JV's is whatever its stored Performance
carries, corrected so far by a guessed multiplier per piece. This folder is the
kit for closing that gap by measurement: a timeline that plays every shared
voice at three dynamics, a tool that measures a capture of it slot by slot, a
tool that compares two captures and names the channel to move, and the file
that moves it. The nano is the reference. The JV is brought to it.

Vice versa holds once the table matches: a piece balanced on the nano plays
on the JV with the same relative mix, and a piece balanced on the JV plays on
the nano — the pieces' nano/jv twins then differ only in routing, not level.

## What sets a level (and what does not)

Every note's velocity is one formula, the same on both rigs:

    velocity = floor(127 × mark × kDynamic × d × inline |* vel × divisi)

- `mark` is the dynamic mark through `dynTable` (`Score.hs`): ffff 1.00, fff 0.90,
  ff 0.75, f 0.65, mf 0.55, mp 0.45, p 0.40, pp 0.35, ppp 0.30, pppp 0.25.
- `kDynamic` is the form's arc (1 when flat), `d` the launcher's multiplier,
  `vel` is `amp` (`Instruments.hs`); SuperCollider sends `(amp × 127).clip(0,127)`.
- Above 1.0 the amplitude clips at velocity 127. The calibration runs `d = 1`,
  the marks alone: pppp 31, p 50, f 81. The pieces' launchers have carried
  boosts (1.5 on the nano twins, up to 1.8 on the jv ones) because the
  orchestra sat low; under a 1.5 boost `ff` and above already clip, which is
  why the top rung is `f`: it stays unclipped under any boost up to 1.5. The
  boosts themselves are a separate question, settled after the rigs agree.

Nothing in the library shapes level per rig or per instrument. The levers:

| lever | where | fixes |
|---|---|---|
| channel volume, CC7 | nano: `nanoMix` in `Devices/NanoSynth.hs`, sent by `setupRig`; JV: the Performance's part levels on the module, or `jv_levels.tidal` here | an instrument sitting too loud or too soft at every dynamic |
| expression, CC11 | pieces ramp it; the launcher opens it to 1 | nothing permanent — it must be reset before a capture |
| patch velocity sensitivity | on the module, per patch | a dynamic spread (pppp → f) wider or narrower than the nano's |
| launcher `d` | per piece | the global level only; never per-instrument balance |

Per-piece `|* vel` factors are composition, not calibration, and are the same
in both twins.

## The nano is the reference — do not retune it

- `nanoMix` holds the 2021 concert values per program (invariant, pinned by
  `NanoSynthSpec`). Nothing here changes them.
- `dynTable` is shared by both rigs and by every piece. Changing it would move
  every mark everywhere; it is not a calibration knob.
- Instrument ranges (`Orchestra.hs`) are the sampler patches' limits and are
  authoritative on both rigs; the calibration's home notes sit well inside them.

## Files

| file | role |
|---|---|
| `calibration.tidal` | the timeline: one bar per (instrument, dynamic), rig-agnostic, same code path as the pieces |
| `timeline.csv` | generated from a headless render of the file on both rigs: slot seconds, measuring window, notes, velocity, both channels. Regenerate with `tools/render_timeline.py` after any edit to the piece; the render also proves the piece (one note per slot, velocities 31/50/81, notes in range) |
| `levels_nano.csv` | the spec: the nano capture measured. Committed from the studio |
| `levels_jv.csv` | the JV capture measured, each iteration (commit the last one) |
| `jv_levels.tidal` | the knob: CC7 per JV channel, sent with `nanoTable 7`, plus a CC11 reset |
| `tools/render_timeline.py` | render → `timeline.csv` (needs `stack`, runs ~2 min) |
| `tools/measure.py` | WAV + `timeline.csv` → `levels_<rig>.csv` (standard library only) |
| `tools/compare.py` | two levels files → Markdown report: trim, per-instrument offset with suggested CC7, dynamic slope, structure |
| `captures/` | WAVs, git-ignored |

## The timeline

Crotchet = 60, so a bar is four seconds; a slot is a note held three seconds
and one second of silence. Seconds from the marker are `(bar − 1) × 4`. The key
is chromatic, so a degree is a semitone and the same MIDI note sounds on both
rigs. Marks pppp / p / f at `d = 1` are velocities 31 / 50 / 81.

| bars | what |
|---|---|
| 1 | marker: one woodblock hit (the alignment onset) |
| 2 | silence: the noise floor |
| 3–44 | fourteen pitched instruments × pppp / p / f, each on its home note (flute A5, oboe A4, clarinet G4, bassoon D3, horn G3, trombone B♭3, bass trombone F2, harp C4, timpani G2, violin 1 A4, violin 2 D4, viola G3, cello D3, contrabass A1) |
| 45–53 | pizzicato: violin 1, cello, contrabass × three (channel 12 on both rigs) |
| 54–68 | bass drum, tambourine, woodblock, triangle, tam-tam × three; the hit path carries no mark, so `|* vel 0.25 / 0.40 / 0.65` stands in for it. The tam-tam has no voice on the nano: its slots are expected silent there |
| 69 | string section chord at p (vn1 vn2 vla vc cb) |
| 70–72 | tutti chord of all fourteen at pppp / p / f |

About five minutes after the marker.

## Workflow

**Studio (NanoSynth), once:**

1. Power up, boot (banner `rig: NanoSynth`), evaluate `setupRig`.
2. Record the module's output. Any format; no normalisation, no limiter; leave
   headroom (peaks below −3 dBFS). Convert if needed:
   `afconvert -f WAVE -d LEI24 capture.m4a captures/nano.wav`
3. Run `calibration.tidal` (evaluate the file top to bottom, then the launcher).
4. `python3 live/jvnanoconfig/tools/measure.py captures/nano.wav live/jvnanoconfig/timeline.csv --rig nano`
   → `levels_nano.csv`. Every slot should read `sound` except the three tam-tam
   slots (`silent`); a `SILENT?` is a slot to re-check (patch, channel, `setupRig`).
5. Commit `levels_nano.csv`; pass the WAV alongside for listening.

**Home (JV-1010), iterate:**

1. Pull. Boot (banner `rig: JV-1010`). Fill `jv_levels.tidal` with the
   Performance's current part levels (Part → Level ÷ 127) so the first send is
   a no-op; if they cannot be read, leave 0.80 everywhere. Evaluate its two
   `once` lines.
2. Record, run `calibration.tidal`, then
   `python3 live/jvnanoconfig/tools/measure.py captures/jv.wav live/jvnanoconfig/timeline.csv --rig jv`.
3. `python3 live/jvnanoconfig/tools/compare.py live/jvnanoconfig/levels_nano.csv live/jvnanoconfig/levels_jv.csv`
4. Read the report in order: the **trim** is the two captures' gain difference
   plus the modules' master levels — ignore it or carry it in `d`. The
   **offset** column is the move: lower the channels that read positive, raise
   the negative, using the suggested value (`current × 10^(−offset/40)`) as a
   start. The **slope** column is what CC7 cannot fix: a JV patch whose spread
   differs by more than 2 dB needs its velocity sensitivity changed on the
   module (or, failing that, a `|* vel` on the lines that use it).
5. Send `jv_levels.tidal` again, re-run, re-measure, re-compare. Stop when every
   residual offset and slope difference is within ±2 dB, then play a twin piece
   from `live/local/Suites/*/jv/` and listen against the nano capture.
6. When it holds, the table belongs in the library: `jvMix` and `rigSetup` in
   `Devices/JV1010.hs`, the shape `nanoMix` has, so `setupRig` sends it on the
   JV too. Then the jv twins' launchers drop their bespoke `d` (1.8 / 1.3 / 1.0)
   to whatever the nano twins carry: one boost for both rigs, chosen once the
   balance holds, or none.

The capture gain is never compared across machines: only relative balance is.
Keep one gain per machine across its iterations so `levels_jv.csv` files are
comparable with each other.

## Structural differences the numbers will show

- **Strings.** The JV plays every bowed section on channel 16 with one patch
  (12–15 are techniques); the nano has violin 1, violin 2, cello and viola+bass
  on four patches with four CC7 values (0.40, 0.30, 0.50, 0.20). Bars 3–44 set
  each section's level; bar 69 shows whether the one-channel chord sums the way
  four channels do. Expect the JV to need its single string level set as a
  compromise, or its articulation channels used as section channels.
- **Viola and contrabass** share one patch on the nano (BiViola, ch 16): their
  balance is register alone there.
- **Harp** sits at CC7 0.10 on the nano — by far the softest channel — because
  the pieces write it loud (mf–ff) and it doubles as the continuo. The JV's
  harp will probably need to come well down.
- **Bass drum** is the orchestra hit at MIDI 12 on the nano (ch 9, Danz Hitz)
  and a rhythm-set bass drum at MIDI 36 on the JV: different sounds, so match
  by weight, not timbre.
- **Tam-tam** exists on the JV only (ch 11, note 31); the nano's trombone is
  on 11. Not compared.
- **Percussion channel.** Tambourine, woodblock and triangle are the 15ips Kit
  on channel 10 on the nano (shared with the MPC) and the rhythm set on
  channel 9 on the JV.
- **UDAGAN synth lines** go to the Moog Mother-32 on channel 6 on the studio
  chain (outside the module) and to the harp (`harp'`, channel 7) on the JV
  twins. Out of scope: the harp slot calibrates the harp, the Moog is its own
  instrument.
- **Pan and depth** differ by design (`nanoMix` pan/CC91 vs the Performance);
  the tools sum to mono and measure RMS, so they are ignored.

## Where things are

- The seam: `live/BootTidal.hs` (`rig`, `setupRig`, `localRig`), `src/Harmonic/Interface/Tidal/Orchestra.hs` (`Rig`, ranges, `instrument'`), `Devices/NanoSynth.hs` (`nanoMix`, `nanoTable`), `Devices/JV1010.hs` (channels, continuo voices, `rigSetup = []`).
- Device references: `documents/NANOSYNTH.md`, `documents/ALGORITHMIC_ORCHESTRATION.md`, `live/docs/ORCHESTRAL_CATALOGUE.tidal`, `live/docs/NANOSYNTH_CATALOGUE.tidal`.
- The twins: `live/local/Suites/{Debussy,Stravinsky,UDAGAN}/{nano,jv}/` (private composition repo).
