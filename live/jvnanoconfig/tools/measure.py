#!/usr/bin/env python3
"""Measure a capture of calibration.tidal slot by slot.

usage: python3 live/jvnanoconfig/tools/measure.py CAPTURE.wav TIMELINE.csv --rig nano|jv
           [--out levels_<rig>.csv] [--marker SECONDS] [--floor-margin DB]

CAPTURE.wav is a PCM WAV (16-, 24- or 32-bit, or 32-bit float), mono or
stereo, recorded from the module's output without normalisation. Anything
else is converted first:  afconvert -f WAVE -d LEI24 capture.m4a capture.wav

Standard library only, so it runs on either machine as is.

The channels are summed to mono. The marker (bar 1, a woodblock hit) is found
as the first 10 ms window whose RMS rises 20 dB above the noise floor of the
first second of the file; pass --marker to place it by hand (seconds into the
file) when the capture starts late or the hit is missed. Every slot's window
in TIMELINE.csv is read relative to that onset.

Per slot: RMS dBFS over the measuring window, peak dBFS over the whole slot.
A slot whose RMS sits within --floor-margin dB (default 6) of the noise floor
is written as `silent`. The noise floor is the RMS of bar 2 (the silent bar).

Output columns:
  slot, section, instrument, mark, velocity, rms_db, peak_db, status
with `floor_db` and `marker_s` recorded in a comment line at the top.
"""
import argparse
import csv
import math
import struct
import sys
import wave
from pathlib import Path


def read_wav(path):
    """-> (rate, [float samples mono, -1..1])."""
    with wave.open(str(path), "rb") as w:
        nch, width, rate, n = w.getnchannels(), w.getsampwidth(), w.getframerate(), w.getnframes()
        raw = w.readframes(n)
    if width == 2:
        vals = struct.unpack("<%dh" % (len(raw) // 2), raw); scale = 32768.0
    elif width == 3:
        vals = [int.from_bytes(raw[i:i + 3], "little", signed=True) for i in range(0, len(raw), 3)]
        scale = 8388608.0
    elif width == 4:
        # 32-bit: the wave module cannot tell int from float; float data is
        # the common case from DAWs, int from afconvert LEI32. Guess by range.
        ints = struct.unpack("<%di" % (len(raw) // 4), raw)
        floats = struct.unpack("<%df" % (len(raw) // 4), raw)
        if all(-4.0 <= f <= 4.0 for f in floats[:: max(1, len(floats) // 2000)]) and any(abs(f) > 1e-6 for f in floats[:: max(1, len(floats) // 2000)]):
            vals, scale = floats, 1.0
        else:
            vals, scale = ints, 2147483648.0
    else:
        sys.exit(f"unsupported sample width {width * 8} bit")
    if nch == 1:
        mono = [v / scale for v in vals]
    else:
        mono = [sum(vals[i:i + nch]) / (nch * scale) for i in range(0, len(vals), nch)]
    return rate, mono


def db(x):
    return 20 * math.log10(x) if x > 0 else -120.0


def rms(samples, a, b):
    a, b = max(0, a), min(len(samples), b)
    if b <= a:
        return 0.0
    return math.sqrt(sum(s * s for s in samples[a:b]) / (b - a))


def peak(samples, a, b):
    a, b = max(0, a), min(len(samples), b)
    return max((abs(s) for s in samples[a:b]), default=0.0)


def find_marker(samples, rate, rise_db=20.0):
    win = int(rate * 0.010)
    floor = rms(samples, 0, rate)
    thresh = floor * (10 ** (rise_db / 20)) if floor > 0 else 1e-4
    thresh = max(thresh, 1e-4)
    for i in range(0, len(samples) - win, win // 2):
        if rms(samples, i, i + win) > thresh:
            return i / rate
    return None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("capture"); ap.add_argument("timeline")
    ap.add_argument("--rig", required=True, choices=["nano", "jv"])
    ap.add_argument("--out"); ap.add_argument("--marker", type=float)
    ap.add_argument("--floor-margin", type=float, default=6.0)
    a = ap.parse_args()
    rate, x = read_wav(a.capture)
    marker = a.marker if a.marker is not None else find_marker(x, rate)
    if marker is None:
        sys.exit("no marker found: pass --marker SECONDS")
    slots = list(csv.DictReader(open(a.timeline, newline="")))
    s = lambda t: int((marker + t) * rate)
    silent_bar = next(r for r in slots if r["slot"] == "1")
    floor = db(rms(x, s(float(silent_bar["end_s"]) + 0.5), s(float(silent_bar["end_s"]) + 3.5)))
    out = Path(a.out) if a.out else Path(a.timeline).parent / f"levels_{a.rig}.csv"
    rows = []
    for r in slots:
        w0, w1 = float(r["win_start_s"]), float(r["win_end_s"])
        level = db(rms(x, s(w0), s(w1)))
        pk = db(peak(x, s(float(r["start_s"])), s(float(r["end_s"]))))
        expect_silent = r["expect"] == f"silent:{a.rig}"
        if level <= floor + a.floor_margin:
            status = "silent" if expect_silent else "SILENT?"
        else:
            status = "sound" if not expect_silent else "UNEXPECTED"
        rows.append({"slot": r["slot"], "section": r["section"], "instrument": r["instrument"],
                     "mark": r["mark"], "velocity": r["velocity"],
                     "rms_db": round(level, 2), "peak_db": round(pk, 2), "status": status})
    with out.open("w", newline="") as fh:
        fh.write(f"# rig={a.rig} capture={Path(a.capture).name} rate={rate} marker_s={marker:.3f} floor_db={floor:.2f}\n")
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys())); w.writeheader(); w.writerows(rows)
    flagged = [f"{r['slot']} {r['instrument']} {r['mark']} {r['status']}" for r in rows if r["status"] in ("SILENT?", "UNEXPECTED")]
    print(f"{a.rig}: marker at {marker:.3f} s, floor {floor:.1f} dBFS, {len(rows)} slots -> {out}")
    if flagged:
        print("check these slots:\n  " + "\n  ".join(flagged))
    if peak(x, 0, len(x)) >= 0.999:
        print("WARNING: the capture clips (peak at full scale); re-record with less gain")


if __name__ == "__main__":
    main()
