# JV-1010 Performance edits — "Orchestrate"

The state of the JV-1010 orchestra against the Performance as it was first
stored: the voices chosen from the Orchestral board, every voice at the
NanoSynth's pitch, balance and dynamic response, the sections in their seats,
an auxiliary percussion section on channel 9 and continuo voices selected by
Program Change. Values were tried in the module's temporary memory, measured
against the nano capture (`levels_nano.csv`) and chosen by ear; the measured
result is `levels_jv.csv`, summarised under *Result*.

**They are stored on the module** (2026-11-16): written over SysEx into User
Performance USER:01, User Patches USER:001–016, the user rhythm set and the
continuo slots by `tools/jv_sysex.py apply jv_edits.json --stored`, and read
back equal to `jv_state_target.json`. `jv_stored_backup.json` is every byte of
the Performance's memory as it was before and `jv_user_slots_backup.json` the
continuo slots' former contents; `tools/jv_sysex.py restore FILE` puts either
back. `jv_stored_calibrated.json` is the same memory with the edits in.

Generated from `jv_state_baseline.json` (the Performance as it was stored on
2026-11-15) and `jv_state_target.json` (162 rows):

    python3 tools/jv_sysex.py sheet jv_state_baseline.json jv_state_target.json

## The voices

Numbers are the Orchestral board's own (`orchestral_board_patches.csv`). A
user patch made from a board patch is that patch with the range, tuning and
dynamics below written over it (`"board": n` in `jv_edits.json`).

| part | made from |
|---|---|
| 1 | 087 Flute, with the piccolo tone of 089 above D7 |
| 2 | 144 2 Oboes |
| 3 | 103 Clarinet 2 |
| 4 | 101 Bassoon 2 |
| 5 | 130 F.Hrn Sect3 |
| 6 | 114 Solo Tb 2 |
| 7 | 186 Harp 1 |
| 8 | 236 Timpani 1 |
| 9 | 233 Concert BD1 |

Strings (parts 12–16) and the tam-tam (11) are the patches first stored.

## Reading the table

- Part *n* of the Performance plays patch **USER:0nn** (part 1 → USER:001 …
  part 16 → USER:016); part 10 is the rhythm part and plays the user rhythm
  set `OrchDrumSet`. A patch row is a value in that user patch; a *Performance
  part* row is the part's own value in the Performance. No other User
  Performance plays these sixteen patches.
- A **Patch name** row marks a user patch remade from another board patch; its
  further rows compare the new patch's tones with the old patch's tones of the
  same number, so a **Wave** row is the sample changing with the patch.
- "tone 1, 2, 3" means the same value on each of those tones. Only tones that
  are switched on are listed.
- TVA velocity curve and sens are the level envelope's (the manual's *Level
  Envelope Velocity Curve / Sens*), not the filter's. Wave gain is the tone's
  WAVE gain. Bias direction, point and level are the tone's TVA bias. Coarse
  tune is in semitones. These are the names an editor shows, should a value
  ever be entered by hand.
- A rhythm key whose **Wave** changed was written whole: wave, pitch, filter
  and level envelope come from one tone of an Orchestral board patch (the
  bytes are the `block` entries of `jv_edits.json`); the table lists the
  values this tool decodes.
- `python3 tools/jv_sysex.py apply jv_edits.json` (no `--stored`) puts the same
  values in the temporary Performance only; selecting the Performance again
  discards them. Continuo voices are written by a stored apply alone.

## The edits

| part | where | parameter | from | to |
|---|---|---|---|---|
| — | Performance | Keyboard range switch | 0 | 1 |
| 1 | Performance part | Level | 127 | 53 |
| 1 | patch `Flute`, tone 4 | Tone switch | off | on |
| 1 | patch `Flute`, tone 1 | Key range lower | C4 (60) | C3 (48) |
| 1 | patch `Flute`, tone 1 | Key range upper | C8 (108) | D7 (98) |
| 1 | patch `Flute`, tone 1 | Coarse tune (semitones) | 12 | 0 |
| 1 | patch `Flute`, tone 1 | TVA velocity sens | 100 | 116 |
| 2 | Performance part | Level | 127 | 67 |
| 2 | Performance part | Patch name | Oboe 1 | 2 Oboes |
| 2 | patch `2 Oboes` | Patch level | 127 | 100 |
| 2 | patch `2 Oboes`, tone 1 | velocity_upper | 124 | 117 |
| 2 | patch `2 Oboes`, tone 1 | cutoff | 96 | 76 |
| 2 | patch `2 Oboes`, tone 1, 2 | filter_velocity_curve | 1 | 2 |
| 2 | patch `2 Oboes`, tone 1, 2 | filter_velocity_sens | 0 | 46 |
| 2 | patch `2 Oboes`, tone 1, 2 | TVA velocity sens | 100 | 110 |
| 2 | patch `2 Oboes`, tone 1 | Wave | EXP2:51 | EXP2:53 |
| 2 | patch `2 Oboes`, tone 2 | velocity_lower | 125 | 116 |
| 2 | patch `2 Oboes`, tone 2 | cutoff | 96 | 73 |
| 2 | patch `2 Oboes`, tone 2 | Wave | EXP2:54 | EXP2:51 |
| 3 | Performance part | Level | 127 | 92 |
| 3 | Performance part | Patch name | Clarinet 1 | Clarinet 2 |
| 3 | patch `Clarinet 2`, tone 2 | Tone switch | off | on |
| 3 | patch `Clarinet 2`, tone 1 | Wave gain (dB) | 6 | 12 |
| 3 | patch `Clarinet 2`, tone 1 | velocity_upper | 127 | 120 |
| 3 | patch `Clarinet 2`, tone 1 | cutoff | 99 | 105 |
| 3 | patch `Clarinet 2`, tone 1 | filter_velocity_curve | 2 | 1 |
| 3 | patch `Clarinet 2`, tone 1 | filter_velocity_sens | 52 | 46 |
| 3 | patch `Clarinet 2`, tone 1 | TVA velocity curve | 2 | 1 |
| 3 | patch `Clarinet 2`, tone 1 | TVA velocity sens | 66 | 136 |
| 4 | Performance part | Level | 127 | 71 |
| 4 | Performance part | Patch name | Bassoon 1 | Bassoon 2 |
| 4 | patch `Bassoon 2`, tone 1 | Wave gain (dB) | 6 | 12 |
| 4 | patch `Bassoon 2`, tone 1 | velocity_upper | 117 | 127 |
| 4 | patch `Bassoon 2`, tone 1 | Coarse tune (semitones) | -12 | 0 |
| 4 | patch `Bassoon 2`, tone 1 | cutoff | 113 | 85 |
| 4 | patch `Bassoon 2`, tone 1 | filter_velocity_curve | 2 | 1 |
| 4 | patch `Bassoon 2`, tone 1 | filter_velocity_sens | 46 | 0 |
| 4 | patch `Bassoon 2`, tone 1 | TVA velocity curve | 2 | 1 |
| 4 | patch `Bassoon 2`, tone 1 | TVA velocity sens | 44 | 114 |
| 5 | Performance part | Level | 127 | 88 |
| 5 | Performance part | Patch name | Solo F.Horn1 | F.Hrn Sect3 |
| 5 | patch `F.Hrn Sect3`, tone 2 | Tone switch | off | on |
| 5 | patch `F.Hrn Sect3`, tone 3 | Tone switch | off | on |
| 6 | Performance part | Level | 127 | 107 |
| 6 | Performance part | Patch name | Solo Tb 1 | Solo Tb 2 |
| 6 | patch `Solo Tb 2`, tone 1 | Wave gain (dB) | 6 | 12 |
| 6 | patch `Solo Tb 2`, tone 1 | Key range lower | E2 (40) | A0 (21) |
| 6 | patch `Solo Tb 2`, tone 1 | Coarse tune (semitones) | -12 | 0 |
| 6 | patch `Solo Tb 2`, tone 1 | Bias direction | ALL | UPPER |
| 6 | patch `Solo Tb 2`, tone 1 | Bias point | C4 (60) | F2 (41) |
| 6 | patch `Solo Tb 2`, tone 1 | Bias level | 0 | -20 |
| 6 | patch `Solo Tb 2`, tone 1 | TVA velocity sens | 64 | 138 |
| 6 | patch `Solo Tb 2`, tone 1 | Wave | EXP2:76 | EXP2:78 |
| 7 | Performance part | Level | 127 | 50 |
| 7 | Performance part | Receive program change | 0 | 1 |
| 7 | patch `Harp 1`, tone 1 | TVA velocity sens | 108 | 132 |
| 8 | Performance part | Pan | -18 | -8 |
| 8 | patch `Timpani 1  /`, tone 1, 2 | Wave gain (dB) | 6 | 12 |
| 8 | patch `Timpani 1  /`, tone 1, 2 | Coarse tune (semitones) | -12 | 0 |
| 9 | Performance part | Level | 127 | 93 |
| 9 | Performance part | Pan | -6 | 12 |
| 9 | Performance part | Key range upper | G9 (127) | B2 (47) |
| 9 | patch `Concert BD1`, tone 2, 3, 4 | Wave gain (dB) | 6 | 12 |
| 9 | patch `Concert BD1`, tone 2, 3, 4 | TVA velocity curve | 2 | 1 |
| 9 | patch `Concert BD1`, tone 2, 3, 4 | TVA velocity sens | 98 | 60 |
| 9 | patch `Concert BD1`, tone 2, 3, 4 | Pan keyfollow | 40 | 0 |
| 11 | Performance part | Level | 60 | 40 |
| 12 | Performance part | Level | 127 | 107 |
| 12 | patch `Pizzicato  1`, tone 1, 2, 3 | Wave gain (dB) | 6 | 12 |
| 12 | patch `Pizzicato  1`, tone 1, 2, 3 | TVA velocity sens | 100 | 142 |
| 12 | patch `Pizzicato  1`, tone 1 | Pan | 17 | 33 |
| 12 | patch `Pizzicato  1`, tone 1 | Pan keyfollow | -70 | -40 |
| 12 | patch `Pizzicato  1`, tone 2 | Pan | -22 | -6 |
| 12 | patch `Pizzicato  1`, tone 2 | Pan keyfollow | -30 | -40 |
| 12 | patch `Pizzicato  1`, tone 3 | Pan | 0 | 16 |
| 13 | Performance part | Level | 127 | 89 |
| 13 | patch `STR Spiccato`, tone 1, 2, 3 | TVA velocity sens | 100 | 124 |
| 13 | patch `STR Spiccato`, tone 1 | Pan | -12 | -3 |
| 13 | patch `STR Spiccato`, tone 2 | Pan | -21 | -12 |
| 13 | patch `STR Spiccato`, tone 2, 3 | Pan keyfollow | -10 | -30 |
| 13 | patch `STR Spiccato`, tone 3 | Pan | 5 | 14 |
| 14 | Performance part | Level | 127 | 87 |
| 14 | patch `LowSTR Attak`, tone 1, 2, 3, 4 | TVA velocity sens | 100 | 124 |
| 14 | patch `LowSTR Attak`, tone 1 | Pan | -45 | -43 |
| 14 | patch `LowSTR Attak`, tone 1, 2, 3 | Pan keyfollow | 0 | -30 |
| 14 | patch `LowSTR Attak`, tone 2 | Pan | 45 | 47 |
| 14 | patch `LowSTR Attak`, tone 3, 4 | Pan | 0 | 2 |
| 14 | patch `LowSTR Attak`, tone 4 | TVA velocity curve | 2 | 1 |
| 14 | patch `LowSTR Attak`, tone 4 | Pan keyfollow | -10 | -30 |
| 15 | Performance part | Level | 127 | 83 |
| 15 | patch `Slow Vlns 1`, tone 1, 2 | Level | 66 | 105 |
| 15 | patch `Slow Vlns 1`, tone 1, 2, 3 | TVA velocity sens | 100 | 124 |
| 15 | patch `Slow Vlns 1`, tone 1 | Pan | 36 | 53 |
| 15 | patch `Slow Vlns 1`, tone 1 | Pan keyfollow | -20 | -70 |
| 15 | patch `Slow Vlns 1`, tone 2 | Pan | -49 | -32 |
| 15 | patch `Slow Vlns 1`, tone 2 | Pan keyfollow | -10 | -70 |
| 15 | patch `Slow Vlns 1`, tone 3 | Level | 36 | 57 |
| 15 | patch `Slow Vlns 1`, tone 3 | Pan | -24 | -7 |
| 15 | patch `Slow Vlns 1`, tone 3 | Pan keyfollow | -50 | -70 |
| 16 | Performance part | Level | 127 | 56 |
| 16 | patch `VlsVasVcsCbs`, tone 1 | Level | 127 | 101 |
| 16 | patch `VlsVasVcsCbs`, tone 1 | Bias level | 30 | 0 |
| 16 | patch `VlsVasVcsCbs`, tone 1, 2, 3, 4 | TVA velocity curve | 2 | 1 |
| 16 | patch `VlsVasVcsCbs`, tone 1, 2, 3, 4 | TVA velocity sens | 86 | 124 |
| 16 | patch `VlsVasVcsCbs`, tone 1 | Pan | -34 | -30 |
| 16 | patch `VlsVasVcsCbs`, tone 1 | Pan keyfollow | -30 | -10 |
| 16 | patch `VlsVasVcsCbs`, tone 2, 4 | Wave gain (dB) | 6 | 12 |
| 16 | patch `VlsVasVcsCbs`, tone 2 | Level | 104 | 127 |
| 16 | patch `VlsVasVcsCbs`, tone 2 | Bias point | C4 (60) | G3 (55) |
| 16 | patch `VlsVasVcsCbs`, tone 2 | Bias level | 10 | -30 |
| 16 | patch `VlsVasVcsCbs`, tone 2 | Pan | 5 | 0 |
| 16 | patch `VlsVasVcsCbs`, tone 4 | Level | 103 | 127 |
| 16 | patch `VlsVasVcsCbs`, tone 4 | Bias direction | ALL | UPPER |
| 16 | patch `VlsVasVcsCbs`, tone 4 | Bias point | C4 (60) | G2 (43) |
| 16 | patch `VlsVasVcsCbs`, tone 4 | Bias level | -40 | -20 |
| 10 | Performance part (rhythm) | Receive switch | 0 | 1 |
| 10 | Performance part (rhythm) | MIDI channel | 10 | 9 |
| 10 | Performance part (rhythm) | Level | 127 | 107 |
| 10 | Performance part (rhythm) | Mix/EFX send level | 0 | 127 |
| 10 | Performance part (rhythm) | Chorus send level | 0 | 10 |
| 10 | Performance part (rhythm) | Reverb send level | 0 | 104 |
| 10 | Performance part (rhythm) | Key range lower | C-1 (0) | C3 (48) |
| 10 | rhythm set `OrchDrumSet`, key F#3 (54) | Level | 127 | 68 |
| 10 | rhythm set `OrchDrumSet`, key A3 (57) | Wave gain (dB) | 0 | 6 |
| 10 | rhythm set `OrchDrumSet`, key A3 (57) | Level | 127 | 100 |
| 10 | rhythm set `OrchDrumSet`, key A3 (57) | Pan | -11 | -10 |
| 10 | rhythm set `OrchDrumSet`, key A3 (57) | Wave | INT2:101 | EXP2:134 |
| 10 | rhythm set `OrchDrumSet`, key B3 (59) | Level | 127 | 113 |
| 10 | rhythm set `OrchDrumSet`, key B3 (59) | Pan | 19 | -18 |
| 10 | rhythm set `OrchDrumSet`, key C4 (60) | Wave gain (dB) | 6 | 12 |
| 10 | rhythm set `OrchDrumSet`, key C4 (60) | Level | 117 | 114 |
| 10 | rhythm set `OrchDrumSet`, key C4 (60) | TVA velocity sens | 96 | 100 |
| 10 | rhythm set `OrchDrumSet`, key C4 (60) | Pan | 35 | 22 |
| 10 | rhythm set `OrchDrumSet`, key C4 (60) | Wave | INT2:106 | EXP2:124 |
| 10 | rhythm set `OrchDrumSet`, key C#4 (61) | Wave gain (dB) | 6 | 12 |
| 10 | rhythm set `OrchDrumSet`, key C#4 (61) | Level | 117 | 127 |
| 10 | rhythm set `OrchDrumSet`, key C#4 (61) | TVA velocity sens | 96 | 100 |
| 10 | rhythm set `OrchDrumSet`, key C#4 (61) | Pan | 35 | 22 |
| 10 | rhythm set `OrchDrumSet`, key C#4 (61) | Wave | INT2:107 | EXP2:125 |
| 10 | rhythm set `OrchDrumSet`, key D4 (62) | Wave gain (dB) | 6 | 12 |
| 10 | rhythm set `OrchDrumSet`, key D4 (62) | Level | 127 | 101 |
| 10 | rhythm set `OrchDrumSet`, key D4 (62) | TVA velocity sens | 96 | 100 |
| 10 | rhythm set `OrchDrumSet`, key D4 (62) | Pan | -25 | 22 |
| 10 | rhythm set `OrchDrumSet`, key D4 (62) | Wave | INT2:110 | EXP2:126 |
| 10 | rhythm set `OrchDrumSet`, key Eb4 (63) | Envelope mode (1 = sustain) | 0 | 1 |
| 10 | rhythm set `OrchDrumSet`, key Eb4 (63) | Level | 127 | 76 |
| 10 | rhythm set `OrchDrumSet`, key Eb4 (63) | TVA velocity sens | 96 | 100 |
| 10 | rhythm set `OrchDrumSet`, key Eb4 (63) | Pan | -25 | 22 |
| 10 | rhythm set `OrchDrumSet`, key Eb4 (63) | Wave | INT2:108 | EXP2:127 |
| 10 | rhythm set `OrchDrumSet`, key E4 (64) | Coarse tune (key) | 60 | 56 |
| 10 | rhythm set `OrchDrumSet`, key E4 (64) | Level | 127 | 90 |
| 10 | rhythm set `OrchDrumSet`, key E4 (64) | TVA velocity sens | 96 | 100 |
| 10 | rhythm set `OrchDrumSet`, key E4 (64) | Pan | -20 | 22 |
| 10 | rhythm set `OrchDrumSet`, key E4 (64) | Wave | INT2:109 | INT2:77 |
| 10 | rhythm set `OrchDrumSet`, key E5 (76) | Level | 127 | 95 |
| 10 | rhythm set `OrchDrumSet`, key E5 (76) | TVA velocity sens | 100 | 106 |
| 10 | rhythm set `OrchDrumSet`, key A5 (81) | Wave gain (dB) | 6 | 0 |
| 10 | rhythm set `OrchDrumSet`, key A5 (81) | Level | 113 | 67 |
| 10 | rhythm set `OrchDrumSet`, key C6 (84) | Wave gain (dB) | 0 | 6 |
| 10 | rhythm set `OrchDrumSet`, key C6 (84) | Level | 127 | 73 |
| 10 | rhythm set `OrchDrumSet`, key C6 (84) | Pan | 40 | -28 |
| 10 | rhythm set `OrchDrumSet`, key C6 (84) | Wave | INT2:59 | EXP2:146 |

## Continuo voices

User Patches outside the Performance's sixteen, selected on part 7 by Program
Change (bank 80 / 0). Each has the harp's velocity response and pan
keyfollow 0, and its patch level sets it to the harp's loudness at the part's
level.

| user patch | made from | patch level |
|---|---|---|
| 10 | board 196 | 92 |
| 17 | board 204 | 123 |

Slot 10 is `Harpsichord1`, F1–F6 (its second tone is the key-release click);
slot 17 is `Marimba`, C2–C7, coarse tune −12 (the internal wave sounds an
octave above the key).

## What each kind of edit does

- **Coarse tune 0.** Every pitched tone sounds the note it is sent, as the
  NanoSynth does. The board keys its flute and piccolo an octave low and its
  bassoon, trombone and timpani an octave high; those five are corrected.
- **Key ranges.** Each voice sounds over its instrument's range and is silent
  outside it. The flute tone runs C3–D7 and the piccolo tone continues from
  Eb7 to C8; the trombone patch runs down to A0 for the bass trombone.
- **TVA velocity sens (and curve 1 everywhere it was 2).** The X3 passes the JV
  velocities 42–127, so the pieces' pppp / p / f arrive as 62 / 75 / 96. On
  curve 1 the level between those three moves 3.3 + 4.0 dB at sens +100 and
  13 + 10 dB at +140; each patch's sens is the value whose spread matches the
  nano's voice.
- **Part level.** The balance, on a law of 40 dB per decade of part level
  (measured). The timpani sits at 127 and every other part is placed under it.
  The timpani plays one tone per layer: two copies of a tone comb against each
  other, because the module starts them up to 0.25 ms apart and the gap
  changes with every note. Its level therefore comes from the rest of the
  orchestra standing 3 dB further back, and from the timpani reading about
  3 dB under its nano proportion; the mixer makes up the overall level.
- **Wave gain +12.** Headroom: +6 dB of wave gain on clarinet, bassoon, horn,
  trombone, timpani, bass drum and pizzicato.
- **Bias, direction UPPER** on the trombone patch: a downward tilt above F2
  that sets the trombone against the bass trombone inside one patch.
- **Bass drum velocity sens 60.** A narrower dynamic than the nano's hit.
- **Timpani and bass drum pan.** Timpani a little left (part pan −8), bass
  drum a little right (+12), the centre left free.
- **The arco patch (16).** Its four tones are the sections, crossfaded by
  register. Stored, its loudness climbed 16.5 dB from the low basses to the
  high violins; it now spans 8. The violin tone is flat across its range and
  4 dB down; the viola tone is raised 6 dB and fades above G3, filling the
  register around G3 where the patch was hollow and leaving the top to the
  violins; the bass tone is raised and fades above G2. The part sits 3 dB
  under the nano's string chord. A tone whose bias pushes it to full level
  ignores its level value there: gain and bias are what move it.
- **String technique patches (12–15): tone pan and pan keyfollow.** Pizzicato,
  spiccato, marcato and legato follow the arco patch's placement by register,
  basses right to violins left. Spiccato, marcato and legato sit 3 dB under
  their first calibration, with arco; pizzicato is at full level.
- **Keyboard range switch, part 9 key range, part 10.** Channel 9 is two
  parts: `Concert BD1` up to B2 and the rhythm part from C3, switched on,
  moved to channel 9 (the U6 keeps channel 10 from the JV) and given its mix
  send with the bass drum part's chorus and reverb sends.
- **Rhythm set keys.** Tambourine 54, woodblock 76 and triangle 81 are the
  set's own, at the nano kit's level. Crash 57, the snare drum 60–63 and wind
  chimes 84 are Orchestral board waves written over the set's crash, bongos,
  congas and chimes; the side stick is copied to 64 and the ride on 59 moves
  to the left. The roll on 63 sustains while held.
- **Receive program change, part 7.** The continuo part follows a Program
  Change; the other fifteen ignore one.

## Result

JV − nano in dB, the recording chains' gain difference removed
(`captures/compare_rebal2.md` has every slot). This round was taken dry with
the arco part at level 72, the viola tone at level 104 and the bass tone fading
from G1; the stored values (66, 127, G2) move the arco rows by under 2 dB, and
a dry round of the stored Performance replaces this table:

| voice | level | pppp→p | p→f |
|---|---|---|---|
| flute | +0.4 | −1.7 | +1.1 |
| oboe | −0.2 | +1.6 | +0.4 |
| clarinet | +0.7 | −0.8 | +2.0 |
| bassoon | +0.6 | −0.6 | +1.1 |
| horn | +1.0 | −0.1 | +0.3 |
| trombone | −1.5 | +0.0 | +0.2 |
| bass trombone | −2.8 | +0.1 | +1.5 |
| harp | +0.5 | −1.0 | +0.3 |
| timpani | −1.6 | −0.7 | −0.2 |
| bass drum | −0.1 | +0.4 | +0.2 |
| tambourine | +1.0 | −0.8 | −0.1 |
| woodblock | +1.0 | −0.5 | +0.4 |
| triangle | +0.9 | −0.9 | +0.1 |
| violin 1 | −0.8 | −3.5 | −0.1 |
| violin 2 | −2.7 | −1.1 | +1.3 |
| viola | −6.9 | −1.3 | +1.5 |
| cello | −1.1 | +1.7 | +1.1 |
| contrabass | −1.9 | −0.1 | +2.1 |
| violin 1 pizz | +3.4 | −1.2 | +3.4 |
| cello pizz | −0.3 | +2.4 | +2.3 |
| contrabass pizz | −4.3 | −1.7 | +6.2 |

String section chord at p −2.5; tutti −0.7 / −1.5 / −0.5 at pppp / p / f.

Since this round every part but the timpani stands 3 dB further back, so the
timpani reads about 3 dB lower than shown against the rest; the other rows
keep their relation to each other. The arco strings are meant to read about
3 dB under the nano's proportion, and the trombone, at one voice, a few dB
under.

## Voices per note

The module has 64 voices; a note takes one per sounding tone.

| part | voices per note |
|---|---|
| flute / piccolo, bassoon, trombone, harp, marimba | 1 |
| oboe, clarinet, horn, timpani, bass drum, harpsichord | 2 |
| tam-tam | 1, 2 from f |
| pizzicato, spiccato, legato | 3 |
| marcato | 2 below C2, 4 above |
| arco | 1 at the extremes to 4 around G3–C4 |

`tools/polyphony.py` renders a piece and charges every note these voices for
as long as it sounds. The scored JV pieces peak at 42 (Ocean), 47
(Arabesque), 50 (Stravinsky's Andante), 58 (Ysyakh) and 62 (Snowflake, for
half a second); the arco and legato strings make up most of every peak, the
legato patch through its 2-second release. Each part reserves 4 voices
(Performance voice reserve), which only acts once all 64 are in use.

## What the edits do not reach

- **Loudness.** The orchestra is quieter in tutti than the Performance as it
  was first stored (every part at 127). The difference is made up at the
  mixer, once.
- **The viola's register.** Around G3 the arco patch is still about 3 dB under
  its neighbours: the viola sample is weak there and the cello and bass
  samples partly cancel.
- **Violins above F6** sit 11–13 dB left; as first stored they were 21–25 dB
  left, a jump from the register below.
- **Pizzicato from p to f** rises about 11.5 dB against the nano's 5.5–9.2, the
  price of matching its 13–17 dB from pppp to p with one sens value.
- **Above f.** The calibration stops at f (velocity 96 at the JV). The response
  from there to 127 is whatever the same curve gives; it is not measured.
- **Tam-tam, crash, ride, wind chimes, snare drum** have no nano voice in the
  calibration; their levels are set by proportion and by ear.
- **Reverb and chorus sends** are as stored.

## Confirming the stored result

With the Performance selected afresh (or after a power cycle), so that what
sounds was loaded from stored memory, and with nothing between the module and
the M4's inputs but the mixer's gain:

    python3 tools/jv_sysex.py state > captures/jv_state_recalled.json
    diff captures/jv_state_recalled.json jv_state_target.json   # no output: every value is in
    python3 tools/round.py stored                                # the same table as above, from the module alone

The calibration holds for the X3's velocity floor of 42 (`JV_VEL_FLOOR`). A
different floor changes the velocities the JV receives and the sens values
with it.
