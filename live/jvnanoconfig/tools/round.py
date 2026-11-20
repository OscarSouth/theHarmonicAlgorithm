#!/usr/bin/env python3
"""Play the calibration timeline into the JV-1010, record it, measure, compare.

usage: python3 live/jvnanoconfig/tools/round.py NAME [--law 40]

One round, no hands: the timeline is rebuilt from timeline.csv as a MIDI file
(the JV's channels, notes and velocities, a note held 3 s, a hit for the
sustain the pieces' `hit` gives it) and
played with aplaymidi to the U2MIDI, so it reaches the JV through the X3 and
its velocity floor exactly as a piece does. The M4 is recorded meanwhile
(the JV arrives on inputs 3+4) and the capture goes through measure.py and
compare.py:

  captures/NAME.wav            the recording, four channels
  captures/levels_jv_NAME.csv  measured, with the M4 source volume in its header
  captures/compare_NAME.md     against levels_nano.csv

The JV is measured as it stands: whatever `jv_sysex.py apply` last put in its
temporary memory. Its part levels are read back first and passed to the report
so it can suggest the next ones.

Importable: probe(events) plays [(seconds, channel, note, velocity, hold)] and
returns each event's loudest 400 ms in dB, for quick questions that do not
need the whole timeline.
"""
import argparse
import csv
import json
import signal
import struct
import subprocess
import sys
import time
from pathlib import Path

import jv_sysex
import measure

HERE = Path(__file__).resolve().parents[1]
CAPTURES = HERE / "captures"
MIDI_OUT = "U2MIDI Pro:0"
SOURCE = "alsa_input.usb-MOTU_M4_M4AE1AEE1J-00.analog-surround-40"
JV_INPUTS = [3, 4]
TICKS_PER_S = 960        # 480 per quarter at the default 120 bpm
LEAD_S = 2.0             # silence recorded before the first event
SYNC_HIT = (2, 84, 110, 0.08)   # a probe opens with this loud oboe blip: the onset its windows are placed from,
SYNC_LEAD_S = 1.5               # so a probed note may be as quiet as the noise floor
HIT_SUSTAIN = {"bassdrum": 0.05, "tamtam": 0.5, "tambourine": 0.05, "triangle": 0.3, "woodblock": 0.05}   # `hit` in Orchestra.hs


def vlq(n):
    out = [n & 0x7F]; n >>= 7
    while n:
        out.insert(0, (n & 0x7F) | 0x80); n >>= 7
    return bytes(out)


def smf(events):
    """events: (seconds, channel 1-16, note, velocity, hold seconds)."""
    msgs = []
    for t, ch, note, vel, hold in events:
        msgs.append((LEAD_S + t, 1, bytes([0x90 | (ch - 1), note, vel])))
        msgs.append((LEAD_S + t + hold, 0, bytes([0x80 | (ch - 1), note, 0])))
    track, now = b"", 0
    for t, _, m in sorted(msgs):
        tick = int(round(t * TICKS_PER_S))
        track += vlq(tick - now) + m; now = tick
    track += vlq(TICKS_PER_S) + b"\xFF\x2F\x00"
    return b"MThd" + struct.pack(">IHHH", 6, 0, 1, 480) + b"MTrk" + struct.pack(">I", len(track)) + track


def source_volume():
    out = subprocess.run(["pactl", "get-source-volume", SOURCE], capture_output=True, text=True).stdout
    return out.split("/")[1].strip() if "/" in out else sys.exit(f"cannot read the volume of {SOURCE}")


def play_and_record(midi, wav):
    """Record the M4 while aplaymidi plays `midi`; returns when the file is closed."""
    mid = wav.with_suffix(".mid"); mid.write_bytes(midi)
    rec = subprocess.Popen(["pw-record", "--target", SOURCE, "--channels", "4", "--rate", str(measure.RATE),
                            "--format", "s24", str(wav)])
    time.sleep(1.0)
    subprocess.run(["aplaymidi", "-p", MIDI_OUT, str(mid)], check=True)
    time.sleep(1.5)
    rec.send_signal(signal.SIGINT); rec.wait()
    mid.unlink()


def program(channel, msb, lsb, number):
    """Bank select and Program Change on one channel, through the X3 as a piece sends them."""
    c = channel - 1
    track = bytes([0, 0xB0 | c, 0, msb, 10, 0xB0 | c, 32, lsb, 10, 0xC0 | c, number]) + vlq(TICKS_PER_S // 10) + b"\xFF\x2F\x00"
    mid = CAPTURES / "program.mid"; CAPTURES.mkdir(exist_ok=True)
    mid.write_bytes(b"MThd" + struct.pack(">IHHH", 6, 0, 1, 480) + b"MTrk" + struct.pack(">I", len(track)) + track)
    subprocess.run(["aplaymidi", "-p", MIDI_OUT, str(mid)], check=True)
    mid.unlink(); time.sleep(0.5)


def probe(events, name="probe", balance=False):
    """Loudness of each event; with balance, (loudness, left minus right in dB) over its loudest 400 ms."""
    CAPTURES.mkdir(exist_ok=True)
    wav = CAPTURES / f"{name}.wav"
    t0 = events[0][0] - SYNC_LEAD_S
    play_and_record(smf([(t0,) + SYNC_HIT] + list(events)), wav)
    x = measure.read_wav(wav, JV_INPUTS)
    onset = measure.find_marker((x ** 2).mean(1))
    if onset is None:
        sys.exit("probe: nothing was recorded on the JV's inputs")
    k = measure.k_cumulative(x)
    i = lambda t: int((onset + t - t0) * measure.RATE)
    out = []
    for t, _, _, _, hold in events:
        level, start = measure.loudest_window(k, i(t - measure.SLOT_HEAD_S), i(t + hold + 0.4))
        left, right = (x[start:start + int(measure.MOMENTARY_S * measure.RATE)] ** 2).mean(0)
        out.append((level, float(measure.db(left) - measure.db(right))) if balance else level)
    return out


def timeline_events():
    events = []
    for r in csv.DictReader(open(HERE / "timeline.csv", newline="")):
        if not r["ch_jv"]:
            continue
        hold = HIT_SUSTAIN.get(r["instrument"], 3.0)
        notes = r["midinotes"].split("/")[-1].split("+")     # an unpitched slot lists nano/jv notes
        for note, ch in zip(notes, r["ch_jv"].split("+")):
            events.append((float(r["start_s"]), int(ch), int(note), int(r["velocity"]), hold))
    return events


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("name")
    ap.add_argument("--law", default="40")
    a = ap.parse_args()
    CAPTURES.mkdir(exist_ok=True)
    wav = CAPTURES / f"{a.name}.wav"
    levels = CAPTURES / f"levels_jv_{a.name}.csv"
    part_levels = CAPTURES / f"part_levels_{a.name}.json"
    part_levels.write_text(json.dumps(jv_sysex.part_levels()))
    vol = source_volume()
    play_and_record(smf(timeline_events()), wav)
    tools = Path(__file__).parent
    measured = subprocess.run([sys.executable, str(tools / "measure.py"), str(wav), str(HERE / "timeline.csv"),
                               "--rig", "jv", "--channels", ",".join(map(str, JV_INPUTS)), "--out", str(levels),
                               "--meta", f"source_volume={vol}"])
    if measured.returncode:
        sys.exit("the marker slot is silent: nothing reached the JV or the M4; round not compared")
    subprocess.run([sys.executable, str(tools / "compare.py"), str(HERE / "levels_nano.csv"), str(levels),
                    "--levels", str(part_levels), "--law", a.law, "--out", str(CAPTURES / f"compare_{a.name}.md")],
                   check=True)


if __name__ == "__main__":
    main()
