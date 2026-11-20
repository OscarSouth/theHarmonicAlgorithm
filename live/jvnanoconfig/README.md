# JV-1010 ↔ NanoSynth level calibration

The studio orchestra (Alesis NanoSynth) and the home orchestra (Roland JV-1010)
play the same files through one rig seam (`rig` in `live/BootTidal.hs`,
`documents/NANOSYNTH.md`, `documents/ALGORITHMIC_ORCHESTRATION.md`). This
folder brings the JV's balance and dynamic response to the nano's by
measurement, so a piece written against one transfers to the other with the
same assumptions of blend and proportion. The timbres differ and are meant to:
the nano is a symphony orchestra, the JV a chamber ensemble after Stravinsky's
*Orpheus*.

The nano is the reference. Its mix is `nanoMix` in `Devices/NanoSynth.hs`, sent
by `setupRig`. The JV's mix is the Performance stored on the module and the
user patches it plays; nothing in the library sets it (`rigSetup = []`). So the
outcome here is not code but a set of values in the module's user memory,
pre-tested, measured, and written there over SysEx. **`JV_PERFORMANCE_EDITS.md`**
is their record: what changed, the measured result, and what they do not reach.

Scope is level, dynamics, stereo placement and the auxiliary percussion on
channel 9. Reverb depth and the choice of voices are separate questions.

## What sets a level

Every note's velocity is one formula, the same on both rigs:

    velocity = floor(127 × mark × kDynamic × d × inline |* vel × divisi)

- `mark` is the dynamic mark through `dynTable` (`Score.hs`): ffff 1.00 … f 0.65
  … p 0.40 … pppp 0.25. The calibration runs `d = 1`: pppp 31, p 50, f 81.
- **The X3 rescales every velocity bound for the JV to 42–127** (`JV_VEL_FLOOR`
  in the firmware), so the JV receives 62 / 75 / 96 for those three. The nano
  receives them unchanged. The JV's patches therefore need a steeper velocity
  response than stock to give the same spread.
- `nanoMix`, `dynTable` and the instrument ranges are shared by every piece and
  are not calibration knobs.

On the JV the levers are, per part: the Performance's part level (the balance),
and inside the patch each tone's TVA velocity curve and sens (the spread),
wave gain (6 dB steps of headroom), tone level and TVA bias (balance between
registers of one patch), a second copy of a tone on a spare tone (+6 dB), and
tone pan with pan keyfollow (where a register sits). A rhythm set key has its
own wave, gain, level, velocity sens and pan.

## Files

| file | role |
|---|---|
| `calibration.tidal` | the timeline as a piece: one bar per (instrument, dynamic), rig-agnostic, the pieces' code path |
| `timeline.csv` | the same timeline as data, generated from a headless render on both rigs by `tools/render_timeline.py`: slot seconds, notes, velocity, both rigs' channels |
| `levels_nano.csv` | the spec: the nano capture measured |
| `levels_jv_baseline.csv` | the JV as it was stored, from a capture of `calibration.tidal` played in Tidal |
| `levels_jv.csv` | the JV with the edits applied |
| `jv_state_baseline.json` | the stored Performance read off the module before any edit: part levels, patches, tone parameters |
| `jv_edits.json` | the edits, in the form `jv_sysex.py apply` sends |
| `jv_state_target.json` | the module's state with the edits applied: what the stored Performance reads back as |
| `jv_stored_backup.json` | every byte of USER:01, USER:001–016 and the user rhythm set as stored before the edits; `jv_sysex.py restore` writes it back |
| `jv_stored_calibrated.json` | the same bytes with the edits in: the calibrated module, restorable the same way |
| `JV_PERFORMANCE_EDITS.md` | the edit sheet, its measured result, and what it does not reach |
| `jv_levels.tidal` | the part levels as CC7, to put them back from Tidal (channel 9 excepted: two parts share it) |
| `orchestral_board_patches.csv` | the Orchestral board's 255 patch names, read off the module: number as `orch` takes it |
| `internal_preset_patches.csv` | the 640 internal preset names (banks A–E), read off the module: number as `presetA` / `presetB` take it |
| `jv_user_slots_backup.json` | the User Patches that stood where the continuo voices now are (slot 10 empty, slot 17 Adrenaline); `jv_sysex.py restore` writes them back |
| `percussion_audition.tidal` | one line per auxiliary percussion key on channel 9, to hear each |
| `tools/measure.py` | WAV + `timeline.csv` → `levels_<rig>.csv` (numpy, scipy) |
| `tools/compare.py` | two levels files → Markdown report: level per voice grouped by JV part, spread per voice, structure |
| `tools/jv_sysex.py` | read the JV's state, apply an edits file to its temporary or its stored memory, back up and restore the stored memory, print the edit sheet |
| `tools/round.py` | one automated round: play the timeline into the JV, record, measure, compare |
| `tools/polyphony.py` | a piece's peak voice count on the JV: renders it headless on the JV rig and charges every note the voices its part's patch takes for as long as they sound (`--measure` times the voices on the module) |
| `captures/` | WAVs, per-round levels and reports, the SysEx log; git-ignored |

## How a level is measured

`measure.py` finds the marker (bar 1's hit; a file that starts on it reads 0)
and reads every slot relative to it. The compared figure, `lk_db`, is the
loudest 400 ms of the slot, K-weighted (BS.1770 momentary loudness): it reads a
held note and a struck one the same way and needs no onset alignment. A slot
opens 50 ms before its grid time: `aplaymidi` and the M4 run on different
clocks, about 80 ppm apart, so four minutes into a round an onset arrives
20 ms early, and a woodblock is all onset. `round.probe` opens its windows the
same way.

Only balance is compared, never absolute level: the two recording chains differ,
and `compare.py` removes their difference as the **trim** (the median delta
over all single-voice slots) before anything else. A capture may be normalised
or recorded at any gain; it must not pass a compressor, gate or reverb.

## The timeline

Crotchet = 60: a bar is four seconds, a slot a note held three seconds and one
of silence (hits take the sustain `hit` gives them in `Orchestra.hs`). Seconds
from the marker are `(bar − 1) × 4`.

| bars | what |
|---|---|
| 1 | marker: one woodblock hit |
| 2 | silence: the noise floor |
| 3–44 | fourteen pitched instruments × pppp / p / f, each on its home note (flute A5, oboe A4, clarinet G4, bassoon D3, horn G3, trombone B♭3, bass trombone F2, harp C4, timpani G2, violin 1 A4, violin 2 D4, viola G3, cello D3, contrabass A1) |
| 45–53 | pizzicato: violin 1, cello, contrabass × three |
| 54–68 | bass drum, tambourine, woodblock, triangle, tam-tam × three (`|* vel 0.25 / 0.40 / 0.65` stands in for the marks). The tam-tam has no voice on the nano |
| 69 | string section chord at p |
| 70–72 | tutti chord of all fourteen at pppp / p / f |

Spiccato, marcato and legato have no slot.

## Connections

- **Notes:** laptop → U2MIDI → X3 → U6 → JV, as in performance, so the velocity
  floor is in the path. `round.py` plays to `U2MIDI Pro:0`.
- **SysEx:** the X3 does not pass it. With the U6MIDI's USB port plugged into
  the laptop, `U6MIDI Pro Port 2` is a direct two-way port to the JV (the U6
  routes USB 2 ↔ DIN 2); `jv_sysex.py` uses it and finds it by name. It opens
  the port raw, which fails while SuperCollider is running (it connects to
  every MIDI port): the live-coding toolchain is closed for `jv_sysex.py` and
  for `round.py`, which reads the part levels through it.
- **Audio:** the JV arrives on M4 inputs 3+4. `round.py` records all four M4
  inputs with `pw-record` and measures 3+4. The M4 source volume is written
  into each levels file; keep it and the mixer gain fixed between rounds.

## Workflow

**Nano (studio), once.** Boot (`rig: NanoSynth`), `setupRig`, record the
module's output while `calibration.tidal` plays, then

    python3 tools/measure.py captures/nano.wav timeline.csv --rig nano

Every slot reads `sound` except the three tam-tam slots.

**JV (home), per round.**

    python3 tools/jv_sysex.py apply jv_edits.json     # the candidate edits, into temporary memory
    python3 tools/round.py NAME                        # five minutes; report in captures/compare_NAME.md

Read the report in this order. *Dynamics* first: a spread difference is the
patch's TVA velocity sens, and changing it moves the level at p. Then *Level*:
`part offset` is the part level's to correct (`suggested` applies the 40 dB per
decade the module measures), `in part` is what remains between voices sharing
the part and is corrected inside the patch. Edit `jv_edits.json`, apply, run
again. Done at ±2 dB of level and ±3 dB of spread, with the string chord and
tutti slots agreeing.

`round.probe` (import `round`) plays a handful of notes and returns their
loudness in a few seconds, for questions that do not need the whole timeline:
a velocity curve's shape, a bias slope, one tone's share of a layered patch.

**Storing.** The JV-1010 has no front-panel write; its user memory takes SysEx
directly, one value per message, and keeps it. With the Performance selected:

    python3 tools/jv_sysex.py backup > jv_stored_backup.json     # once, before the first stored write
    python3 tools/jv_sysex.py apply jv_edits.json --stored
    python3 tools/jv_sysex.py state --stored | diff - jv_state_target.json

`--stored` addresses the User Performance the selected one was loaded from
(found by name and patch assignment), the User Patches its parts play and the
user rhythm set of its rhythm part, and refuses any other address. `restore` writes a backup back block by block and
reads each block back. `sheet jv_state_baseline.json jv_state_target.json` prints the
same edits as a table for `JV_PERFORMANCE_EDITS.md`.

Without `--stored` everything goes to the temporary Performance and its parts'
temporary patches and is gone when the Performance is selected again. Every
write, to either copy, is logged with the value it replaced
(`captures/jv_sysex.log`).

After a stored write, confirm from a fresh recall (select the Performance
again, or power-cycle the module):

    python3 tools/jv_sysex.py state | diff - jv_state_target.json
    python3 tools/round.py stored

## Continuo voices

The continuo part (channel 7) is the one part of the Performance that follows
a Program Change. Its voices are User Patches outside the Performance's own
sixteen, declared under `continuo` in `jv_edits.json` by slot: built from an
Orchestral board patch, cut to the instrument's range, given the harp's
velocity response and levelled to the harp with the patch's own level (40 dB
per decade, as the part level), so the part level serves all of them. A stored
`apply` builds them; `backup 10 17` includes them.

| slot | voice | library name | bank / program |
|---|---|---|---|
| 7 | Harp 1 | `harpV` | 80 / 0 / 6 |
| 10 | Harpsichord1 | `harpsichordV` | 80 / 0 / 9 |
| 17 | Marimba | `marimbaV` | 80 / 0 / 16 |

The Orchestral board itself answers at bank 84 / 2 (patches 1–128) and 84 / 3
(129–255); bank select and Program Change pass the X3 and the U6 on channel 7.

## Structural differences between the rigs

- **Strings.** The JV plays every bowed section on channel 16 with one patch
  (12–15 are techniques); the nano has violin 1, violin 2, cello and viola+bass
  on four patches with four levels. On the JV a section's level is its
  register's, set by the patch's four key-ranged tones.
- **Trombone and bass trombone** share a patch on both rigs (JV ch 6, nano
  ch 11); their balance is register.
- **Bass drum** is the orchestra hit at MIDI 12 on the nano and `Concert BD1`
  at MIDI 36 on the JV: matched by weight, not timbre.
- **Channel 9 on the JV is two parts**, split by key with the Performance's
  Keyboard Range Switch on: part 9 (`Concert BD1`) up to B2, part 10 (the
  rhythm part, user rhythm set `OrchDrumSet`) from C3. The rhythm part listens
  on 9 because the U6 keeps channel 10 from the JV.
- **Tambourine, woodblock, triangle** are the 15ips Kit on channel 10 on the
  nano and keys 54, 76 and 81 of the rhythm set on the JV, matched in level.
- **Crash, ride, wind chimes** are the nano kit's 49, 51 and 84 and the JV
  rhythm set's 57, 59 and 84. **The snare drum** (60–64: stroke, loud stroke,
  accent, roll, side stick) exists on the JV only. None of these has a slot in
  the timeline; their levels are set against the bass drum by `round.probe`
  and by ear. The crash, snare and wind chime waves are the Orchestral board's
  (`Piatti!!`, `Snare Drum`, `ConcertSNR1`, `SNR Roll`, `Wind Chimes1`), each
  key carrying one tone of its patch.
- **Tam-tam** exists on the JV only (ch 11, note 31).
- **UDAGAN synth lines** go to the Moog Mother-32 on channel 6 in the studio
  and to the harp on the JV twins: outside this calibration.

## Where things are

- The seam: `live/BootTidal.hs` (`rig`, `setupRig`, `localRig`), `src/Harmonic/Interface/Tidal/Orchestra.hs` (`Rig`, ranges, `instrument'`, `hit`), `Devices/NanoSynth.hs` (`nanoMix`, `nanoTable`), `Devices/JV1010.hs` (channels, continuo voices).
- The X3 velocity floor and the MIDI routing: `local/docs/MIDI_SYSTEM.md`.
- JV-1010 parameter addresses: owner's manual, Parameter Address Map, pp. 74–80.
- The twins: `live/local/Suites/{Debussy,Stravinsky,UDAGAN}/{nano,jv}/`.
