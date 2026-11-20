#!/usr/bin/env python3
"""Measure a capture of the calibration timeline slot by slot.

usage: python3 live/jvnanoconfig/tools/measure.py CAPTURE.wav TIMELINE.csv --rig nano|jv
           [--out levels_<rig>.csv] [--channels 3,4] [--marker SECONDS]
           [--floor-margin DB] [--meta KEY=VALUE ...]

CAPTURE.wav is 48 kHz PCM (16-, 24- or 32-bit) recorded from the module's
output with no dynamics processing. A gain change or normalisation of the
whole file is harmless: only the balance between slots is ever compared.
--channels picks the inputs that carry the module out of a wider recording
(1-indexed); the default is all of them.

The marker (bar 1, a woodblock hit) is the first 10 ms frame that rises 20 dB
above the quietest second of the file, so a file that starts on the hit reads
marker 0. Pass --marker to place it by hand. Every slot in TIMELINE.csv is
read relative to it.

Per slot:
  lk_db    the loudest 400 ms of the slot, K-weighted (ITU-R BS.1770 momentary
           loudness, channels summed in power). The figure that is compared:
           it needs no onset alignment and reads a held note and a struck one
           the same way. The slot opens 50 ms before its grid time, so an
           onset that arrives early is inside it.
  rms_db   unweighted RMS over the timeline's measuring window, the steady
           part of a held note.
  peak_db  sample peak over the slot.
A slot whose lk_db sits within --floor-margin dB (default 6) of the noise
floor, the K-weighted level of the silent bar 2, is written as `silent`.

The header comment line records rig, capture, marker, floor and every --meta
pair (round.py stores the recording gain there).
"""
import argparse
import csv
import sys
import wave
from pathlib import Path

import numpy as np
from scipy.signal import lfilter

RATE = 48000
# BS.1770 K-weighting at 48 kHz: high shelf, then the RLB high-pass.
K_SHELF = ([1.53512485958697, -2.69169618940638, 1.19839281085285], [1.0, -1.69065929318241, 0.73248077421585])
K_HIGHPASS = ([1.0, -2.0, 1.0], [1.0, -1.99004745483398, 0.99007225036621])
MOMENTARY_S = 0.4
SLOT_HEAD_S = 0.05  # a slot is read from 50 ms before its grid time: the MIDI player's clock and the interface's
                    # differ by ~80 ppm, so five minutes in an onset lands 20 ms early, and a woodblock is all onset
SLOT_TAIL_S = 0.1   # a slot's last 100 ms is left out: it belongs to the next slot's head


def read_wav(path, channels=None):
    """-> float array (frames, channels), -1..1."""
    with wave.open(str(path), "rb") as w:
        nch, width, rate, n = w.getnchannels(), w.getsampwidth(), w.getframerate(), w.getnframes()
        raw = w.readframes(n)
    if rate != RATE:
        sys.exit(f"{path}: {rate} Hz; the K-weighting here is defined at {RATE} Hz, resample first")
    if width == 2:
        x = np.frombuffer(raw, dtype="<i2").astype(np.float64) / 32768.0
    elif width == 3:
        b = np.frombuffer(raw, dtype=np.uint8).reshape(-1, 3).astype(np.int32)
        v = b[:, 0] | (b[:, 1] << 8) | (b[:, 2] << 16)
        x = np.where(v >= 1 << 23, v - (1 << 24), v) / 8388608.0
    elif width == 4:
        x = np.frombuffer(raw, dtype="<i4").astype(np.float64) / 2147483648.0
    else:
        sys.exit(f"{path}: unsupported sample width {width * 8} bit")
    x = x.reshape(-1, nch)
    if channels:
        if max(channels) > nch:
            sys.exit(f"{path}: has {nch} channels, --channels asks for {max(channels)}")
        x = x[:, [c - 1 for c in channels]]
    return x


def db(power):
    return 10 * np.log10(np.maximum(power, 1e-12))


def k_cumulative(x):
    """Running sum of the K-weighted power, channels averaged: a window's mean is a difference of two entries."""
    k = lfilter(*K_HIGHPASS, lfilter(*K_SHELF, x, axis=0), axis=0)
    return np.concatenate([[0.0], np.cumsum((k ** 2).mean(1))])


def loudest_window(k_cum, i0, i1):
    """(dB, start sample) of the loudest 400 ms window lying inside samples i0..i1."""
    win = int(MOMENTARY_S * RATE)
    i0, i1 = max(0, i0), min(len(k_cum) - 1, i1) - win
    if i1 <= i0:
        return -120.0, i0
    starts = np.arange(i0, i1, RATE // 100)
    power = (k_cum[starts + win] - k_cum[starts]) / win
    return float(db(power.max())), int(starts[power.argmax()])


def momentary_max(k_cum, i0, i1):
    return loudest_window(k_cum, i0, i1)[0]


def find_marker(power):
    """Seconds of the first 10 ms frame 20 dB above the file's quietest second."""
    hop = RATE // 100
    frames = power[: len(power) // hop * hop].reshape(-1, hop).mean(1)
    seconds = frames[: len(frames) // 100 * 100].reshape(-1, 100).mean(1)
    above = np.nonzero(frames > max(seconds.min(), 1e-10) * 100)[0]
    return above[0] / 100 if len(above) else None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("capture"); ap.add_argument("timeline")
    ap.add_argument("--rig", required=True, choices=["nano", "jv"])
    ap.add_argument("--out"); ap.add_argument("--marker", type=float)
    ap.add_argument("--channels", type=lambda s: [int(c) for c in s.split(",")])
    ap.add_argument("--floor-margin", type=float, default=6.0)
    ap.add_argument("--meta", nargs="*", default=[])
    a = ap.parse_args()

    x = read_wav(a.capture, a.channels)
    raw_power = (x ** 2).mean(1)
    k_cum = k_cumulative(x)
    marker = a.marker if a.marker is not None else find_marker(raw_power)
    if marker is None:
        sys.exit("no marker found: pass --marker SECONDS")
    s = lambda t: min(len(x), max(0, int(round((marker + t) * RATE))))

    def mean_db(power, t0, t1):
        seg = power[s(t0):s(t1)]
        return float(db(seg.mean())) if len(seg) else -120.0

    slots = list(csv.DictReader(open(a.timeline, newline="")))
    bar1 = next(r for r in slots if r["slot"] == "1")
    floor = float(db((k_cum[s(float(bar1["end_s"]) + 3.5)] - k_cum[s(float(bar1["end_s"]) + 0.5)]) / (3 * RATE)))
    rows = []
    for r in slots:
        t0, t1 = float(r["start_s"]) - SLOT_HEAD_S, float(r["end_s"]) - SLOT_TAIL_S
        lk = momentary_max(k_cum, s(t0), s(t1))
        seg = np.abs(x[s(t0):s(t1)])
        expect_silent = r["expect"] == f"silent:{a.rig}"
        if lk <= floor + a.floor_margin:
            status = "silent" if expect_silent else "SILENT?"
        else:
            status = "UNEXPECTED" if expect_silent else "sound"
        rows.append({"slot": r["slot"], "section": r["section"], "instrument": r["instrument"],
                     "mark": r["mark"], "velocity": r["velocity"], "lk_db": round(lk, 2),
                     "rms_db": round(mean_db(raw_power, float(r["win_start_s"]), float(r["win_end_s"])), 2),
                     "peak_db": round(float(db(seg.max() ** 2)) if seg.size else -120.0, 2), "status": status})

    out = Path(a.out) if a.out else Path(a.timeline).parent / f"levels_{a.rig}.csv"
    meta = "".join(f" {m}" for m in a.meta)
    with out.open("w", newline="") as fh:
        fh.write(f"# rig={a.rig} capture={Path(a.capture).name} marker_s={marker:.3f} floor_db={floor:.2f}{meta}\n")
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys())); w.writeheader(); w.writerows(rows)
    print(f"{a.rig}: marker at {marker:.3f} s, floor {floor:.1f} dB, {len(rows)} slots -> {out}")
    flagged = [f"{r['slot']} {r['instrument']} {r['mark']} {r['status']}" for r in rows if r["status"] in ("SILENT?", "UNEXPECTED")]
    if flagged:
        print("check these slots:\n  " + "\n  ".join(flagged))
    if np.abs(x).max() >= 0.999:
        print("WARNING: the capture reaches full scale; if it was not normalised afterwards it clipped")
    return 1 if next(r for r in rows if r["slot"] == "1")["status"] != "sound" else 0


if __name__ == "__main__":
    sys.exit(main())
