# MiDiNiNjA X3 — Harmonic Algorithm build

Firmware for the MiDiNiNjA X3, a Teensy 4.0 with three DIN inputs and three DIN outputs
that plugs into the MPC's USB host.

The X3 hardware is Matt Todman's design, and this firmware began as a fork of his
`MiDiNiNjA_X3_V1_CLEAN_MUTE` ([submodify/MiDiNiNjA](https://github.com/submodify/MiDiNiNjA)).
The DIN port instances, the LED blink state machine, the button polling and the pad-mute
idea are his. Upstream has no licence file; see [`NOTICES.md`](../../NOTICES.md).

## Ports

| DIN | carries |
|---|---|
| In A | laptop stream, all 16 channels |
| Out A | JV-1010 and laptop: In A's `TO_JV` channels, merged with the MPC's output from port 1 |
| In B | poly keyboard to be mapped, any channel |
| Out B | the mapped stream, on the input's channel (not cabled) |
| In C | unused |
| Out C | Boss RC-10R: the re-clocked Tidal clock and the ch 10 RC-10R lane from In A, plus the dkit track from USB port 3 (MPC) |

USB type **MIDIx4**:

| # | name | X3 → MPC | MPC → X3 |
|---|---|---|---|
| 1 | Main | In A's `TO_MPC` channels, 7 and 10 (the ch 10 control lanes removed, muted note-ons dropped), merged with the mapped stream and In B's MMC. In A's `TO_JV` channels, every one but 10, go straight to Out A instead, so Tidal's orchestra never crosses USB | → Out A (to the JV and laptop), except notes 0–15 (pad-mutes); every note-on leaving Out A is floored to 42–127 for the JV-1010 |
| 2 | Drums | idle: the drum lane goes straight to Out C | — |
| 3 | Looper | — | → Out C: channel messages (the dkit pads' notes 19–34 become the RC-10R kit's notes); realtime is dropped |
| 4 | Mapper | — | a second mapper input, mapped like In B |

The port names come from `name.c`. The stock Teensy 4 core (Teensyduino 1.62) doesn't attach names to USB-MIDI jacks. `patch-core` ports the Teensy 3 core's mechanism into `teensy4/usb_desc.c`, and every `make` runs it. The script regenerates the patched file from a saved pristine copy (`usb_desc.c.orig`), so it is idempotent. It stops without writing if the core's layout has changed.

**DIN transmit buffers.** A DIN write blocks the whole loop once its port's buffer is full. Each output has 128 bytes on top of the core's 40 (`addMemoryForWrite`), enough for the largest burst, the hold's 144 bytes per port, so the clock and the drum lane never wait on a DIN.

Tidal's clock is the master, but its ticks reach In A scattered by the USB-MIDI and DIN hops they share with the music. The X3 re-clocks them and sends the even ticks to port 1 (the MPC) and Out C (the RC-10R) in parallel:
- it fits the tick period over the last 96 ticks (4 beats);
- queueing only ever delays a tick, so it takes the lower edge of the arrivals as the phase;
- it eases its schedule 10% of the way to that fit on each tick;
- output stays within one tick of the input count, so no tick is lost or added;
- until the window fills, and after the input pauses for 4 periods, ticks pass straight through.

The two lanes run off one schedule:
- **The MPC** (port 1) gets the ticks on the schedule.
- **The RC-10R** (Out C) gets them a tick less 2 ms later (never under half a tick; about 27 ms at 87 bpm).
  - The pedal has no start, stop or position: its beat is set by when a start press arrives, so the lag costs it nothing.
  - The `rc10` lane's presses share the music's path and land up to ~13 ms late on busy downbeats. Each is held and released 1 ms before the pedal's next tick, so every start lands at the same offset from a tick. That admits presses up to ~25 ms late.
  - Presses go straight out when no clock is running. The hold clears any held press before its stops.

Start, Continue, Stop and Song Position go In A → port 1 untouched; the RC-10R takes Timing Clock only. The MPC's own clock on port 3 is dropped. It bunches and skips ticks, and the pedal time-stretches its loops to chase that.

| at 87 bpm | tick interval sd | beat-to-beat tempo |
|---|---|---|
| Tidal at source | 0.13 ms | 86.96–87.04 |
| arriving at In A | 2.5 ms | — |
| **re-clocked** | **0.10 ms** | **86.96–87.04** |
| the MPC's own clock | 2.7 ms | 84.5–91.6 |

Every device has exactly one clock source. The debug build (`make debug`) prints the In A clock's interval statistics every 4 beats on its serial port. The button's hold never blocks them. SysEx never crosses.

## Behaviour

**Control lanes on In A, ch 10.** All three are consumed here and never reach the MPC. Everything else on ch 10 passes: the JV-map kit (notes 0–9, 60–80), `metronome`, `count`, the MPC sub (35–48), and the 12 Step CCs.
- **State lane:**
  - notes 108–119 are the pitch classes the mapper locks to (pitch class = note mod 12);
  - note-on adds a pitch class and note-off removes it, so any number can be held at once;
  - they come from Tidal's `kmap` (`harmonicState`), held for each chord;
  - CC 120 or 123 on ch 10 (`hush`) clears the set.
- **RC-10R lane:**
  - CC 70–83 go to Out C on ch 1 (the pedal's `RX CONTROL CH`), unchanged;
  - CC 84 with value k (1–16) becomes program change k−1 on ch 2 (`RX NOTE CH`), which selects the rhythm kit;
  - they come from Tidal's `rc10` (`Devices.RC10R`), and the CC → function map lives there.
- **Drum lane:**
  - CC 33–48 is pad 0–15, and the value is the velocity;
  - each hit goes **straight to Out C** as the kit's note (`drumNote[pad]`), a note-on and an immediate note-off on ch 2 (`RX NOTE CH`). It goes DIN to DIN and never crosses USB;
  - the ch 10 pad-mute (note 9 from the MPC) and the button's hold silence it;
  - the MPC's dkit track keeps its pads: A01–A16 send notes 19–34 on port 3 (track Transpose 0), and on the way to Out C they become the kit's notes (`drumNote[]`); every other note passes unchanged. 19–34 is the free run between the ch 10 kit (0–15, `count` from 8) and the MPC sub (35–48);
  - it comes from Tidal's `dkit` block (`x3rc10`).

  | pad | sound (note) | pad | sound (note) | pad | sound (note) | pad | sound (note) |
  |---|---|---|---|---|---|---|---|
  | 13 | pedal hat (44) | 14 | ride bell (53) | 15 | floor tom (41) | 16 | kick 2 (35) |
  | 9 | ride 2 (59) | 10 | crash 2 (57) | 11 | claves (75) | 12 | snare 2 (40) |
  | 5 | ride (51) | 6 | crash (49) | 7 | side stick (37) | 8 | snare (38) |
  | 1 | kick (36) | 2 | clap (39) | 3 | hat closed (42) | 4 | hat open (46) |

**Mapper.** There are two inputs: DIN In B and USB port 4 (Mapper, from the MPC). Each source has its own key table. Nothing is sent back out on port 4, so an MPC route into it can't loop.
- Each note-on becomes the nearest note whose pitch class is in the set, on the channel it arrived on. Ties go down; velocity is kept. Keys and shared pitches are tracked per channel, so mapping on several channels at once is independent.
- With no set held, the key stays silent. During a chord change the previous set stays active for 20 ms, so note-offs that land before the next chord's note-ons don't open a silent gap.
- Each key's note-off releases the note it actually triggered, so a held key keeps sounding across a chord change.
- Two keys landing on one pitch retrigger it. The note-off waits for the last key to be released.
- CCs (pedal included), pitch bend and pressure pass through on their own channel. Poly aftertouch follows the mapped note.

**Pad-mutes.**
- A note-on 0–15 from the MPC on port 1 (any channel) mutes laptop channel 1–16 until the matching note-off.
- Only note-ons are blocked, so nothing hangs.
- Mute notes are never sent on to the laptop.

**Looper.**
- Port 3 feeds Out C. The RC-10R ignores realtime Start/Stop, so transport goes by CC.
- RC-10R settings: `SYNC CLOCK` MIDI, the START/STOP CCs (CC 1–31 or 64–95), and `RX CONTROL CH` matching the channel the MPC sends on.

**In B controls** (the JV-1010 controller, ported from its Mozaic script). Nothing from In B is forwarded raw. Notes go through the mapper, and so does every control that isn't interpreted below: sustain, other CCs, bend and pressure travel with the mapped notes to port 1 and Out B. The MPC's orchestral tracks listen on All Ports, so they play the mapped stream from port 1.

Interpreted:
- **CC 114**, on any channel except 10 and 16: steps that channel's JV program (63 / 62 = −1 / −2, 65 / 66 = +1 / +2, clamped 1–128). Bank select first if a bank key is held, then program change, to the JV on that channel via Out A. Channel 16 is the JV's Perform Ctrl CH, so it isn't stepped.
- **CC 30 / 31 / 86 / 87** (bank keys), on any channel: held at 127, each selects a bank for the next step (84/0, 84/1, 84/2, 84/3).
- **CC 115 = 127 on ch 10:** selects the JV performance, as the button does.
- **CC 20 / 21 / 22 / 24 / 27 on ch 10:** MMC to Out A and port 1. 127 sends stop / locate-zero + play / record strobe / locate-zero / punch in; CC 27 back to 0 sends punch out.

**Button (PUSH ME).**
- **Press:** selects the JV-1010's orchestral performance (USER 1: bank select 80/0, program 1) on ch 16 out of Out A. The JV's Perform Ctrl CH is 16, because the U6 filters ch 10 out of the JV's feed. One blink.
- **Hold (1 s):** panic. It clears the pad-mutes and releases the mapped notes. Then it sends all-notes-off (CC 123), sustain-off (CC 64 = 0) and expression 0 (CC 11) on all 16 channels to Out A (JV and laptop), Out B and the MPC (USB port 1), and presses the RC-10R's LOOP STOP (CC 74) and RHY STOP (CC 78) on Out C.
  - While held, only clock and transport pass (In A → port 1 and Out C), so every device stays in sync. Everything else is blocked, and the LED stays lit.
  - The harmony state keeps tracking, so the mapper is ready on release.
  - Release sends all-notes-off and expression 127 and resumes. Expression 127 is every part's default, so the performances' own levels (CC 7) are never touched. The stopped loops and rhythm stay stopped until Tidal or the pedal starts them. Two blinks.

## Build

The toolchain is arduino-cli with `teensy:avr@1.62.0` and OneButton 2.6.2 (`make core` installs both and applies `patch-core`). It also needs PJRC's `00-teensy.rules` in `/etc/udev/rules.d/`.

```
make              # performance build, usb=midi4
make flash        # build and upload (press the Teensy's button if the loader asks)
make debug        # usb=serialmidi4: also prints the state set on USB serial
make flash-debug
```

The debug build puts a serial port ahead of the MIDI ports. Read it with `arduino-cli monitor -p <port>`.

## Bench

For debugging, the laptop takes the MPC's place as USB host. Plug the X3's USB into the laptop, and plug into In A and Out A the two DIN cables that normally serve the MPC (from and to the home U6MIDI hub).

The laptop's own traffic then makes a round trip:
- **Inbound:** laptop → U2MIDI → U6MIDI → In A → USB port 1 → laptop.
- **Return:** laptop → USB port 1 → Out A → U6MIDI → U2MIDI in → laptop.

The keyboard goes into In B. Without a cable, play into USB port 4 (Mapper) from the laptop instead: it goes through the same mapper. Mapped output appears on port 1 and Out B.

- **List the ports:** `aconnect -l` lists the X3 and its four named ports.
- **Watch a port:** `aseqdump -p <client:port>`.
- **Send raw bytes:** `amidi -l` lists the X3's raw ports (`hw:N,0,0` to `hw:N,0,3` for ports 1–4). Then, for example, `amidi -p hw:N,0,0 -S '90 03 7F'` sends a pad-mute for channel 4.
