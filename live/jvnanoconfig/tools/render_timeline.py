#!/usr/bin/env python3
"""Render calibration.tidal through the boot on both rigs and write timeline.csv.

usage: python3 live/jvnanoconfig/tools/render_timeline.py [--only nano|jv]

The boot is loaded headless with `p` shadowed (nothing reaches SuperDirt), the
rig forced to each module in turn, and the `calibration` pattern queried over
its whole span. Every note event (cycle, duration, channel, note, amp) is
matched to the slot map below, which mirrors the bar layout of
calibration.tidal, and the result is one CSV row per slot:

  slot, section, instrument, mark, start_s, end_s, win_start_s, win_end_s,
  midinotes, velocity, ch_nano, ch_jv, expect

Seconds are from the marker onset (bar 1), so `start_s = (bar - 1) * 4`. The
measuring window is the steady part of a held note (0.3 s in to 0.1 s before
release) or the first 0.6 s of a hit. `expect` is `sound` or `silent:<rig>`
for a slot one rig cannot voice (the tam-tam on the nano).

The render is also the proof: the script fails if a slot has the wrong number
of notes, a velocity other than 31 / 50 / 81, or a note outside its range.
Run it after any edit to calibration.tidal and commit the CSV with it.
"""
import csv
import re
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
HERE = Path(__file__).resolve().parents[1]
BOOT = "live/BootTidal.hs"
PIECE = HERE / "calibration.tidal"
OUT = HERE / "timeline.csv"
DONE = "__RENDER_DONE__"
DECL = re.compile(r"^[a-z_][\w'.]*[^=\n]*=(?!=)", re.M)

PRE_BARS = 1
BAR_S = 4.0
MARKS = [("pppp", 31), ("p", 50), ("f", 81)]
PITCHED = ["flute", "oboe", "clarinet", "bassoon", "horn", "trombone", "basstrom",
           "harp", "timpani", "violin1", "violin2", "viola", "cello", "contrabass"]
PIZZ = ["violin1", "cello", "contrabass"]
PERC = ["bassdrum", "tambourine", "woodblock", "triangle", "tamtam"]
HELD = (0.3, 2.9)
HIT = (0.0, 0.6)


def slot_map():
    """(bar, section, instrument, mark, velocity, n_notes, window, expect) per slot."""
    rows = [(1, "marker", "woodblock", "f", 81, 1, HIT, "sound")]
    bar = 3
    for name in PITCHED:
        for mark, v in MARKS:
            rows.append((bar, "pitched", name, mark, v, 1, HELD, "sound")); bar += 1
    for name in PIZZ:
        for mark, v in MARKS:
            rows.append((bar, "pizz", name, mark, v, 1, HELD, "sound")); bar += 1
    for name in PERC:
        for mark, v in MARKS:
            exp = "silent:nano" if name == "tamtam" else "sound"
            rows.append((bar, "unpitched", name, mark, v, 1, HIT, exp)); bar += 1
    rows.append((bar, "section", "strings", "p", 50, 5, HELD, "sound")); bar += 1
    for mark, v in MARKS:
        rows.append((bar, "tutti", "tutti", mark, v, len(PITCHED), HELD, "sound")); bar += 1
    return rows


def blocks(text):
    for raw in text.split("\n\n"):
        block = "\n".join(ln for ln in raw.splitlines() if not ln.lstrip().startswith("--")).strip("\n")
        if block.strip():
            yield block


def render(rig, cycles):
    decls = []
    for b in blocks(PIECE.read_text()):
        head = b.splitlines()[0]
        if head.lstrip().startswith(("do", ":", "hush", "import")):
            continue
        if DECL.search(b):
            decls.extend([":{", b, ":}"])
    boot = (ROOT / BOOT).read_text().splitlines()
    i = boot.index(":localRig")
    j = boot.index(":{", i); k = boot.index(":}", j)
    rebind = boot[j:k + 1]
    rigname = {"nano": "nanoSynth", "jv": "jv1010"}[rig]
    session = "\n".join(
        [f":script {BOOT}", f"rig = {rigname}"] + rebind + ["p _ pat = pat"] + decls +
        ["kk = preroll pre $ iK tempo form (rep key 1)",
         "pat = calibration id kk 0.99",   # d = (* 1) applied to 0.99
         'mapM_ (\\e -> putStrLn ("EV|" ++ show (fromRational (start (wholeOrPart e)) :: Double) ++ "|" ++ show (fromRational (stop (wholeOrPart e)) :: Double) ++ "|" ++ show (value e))) (filter eventHasOnset (queryArc pat (Arc 0 %d)))' % cycles,
         f'putStrLn "{DONE}"', ":quit", ""])
    with tempfile.TemporaryDirectory() as tmp:
        s = Path(tmp) / "session.ghci"; s.write_text(session)
        proc = subprocess.run(
            ["stack", "ghci", "--ghci-options=-v0 -Wno-unused-matches -Wno-unused-local-binds "
             "-Wno-unused-do-bind -Wno-name-shadowing -Wno-missing-signatures"],
            cwd=ROOT, stdin=s.open(), capture_output=True, text=True)
    o = proc.stdout + proc.stderr
    if DONE not in o:
        print(o[-4000:], file=sys.stderr); sys.exit(2)
    errs = [l for l in o.splitlines() if "error" in l.lower() and "<interactive>" in l]
    if errs:
        print("\n".join(o.splitlines()[-60:]), file=sys.stderr); sys.exit(1)
    evs = []
    for line in o.splitlines():
        if not line.startswith("EV|"):
            continue
        _, t0, t1, vm = line.split("|", 3)
        n = re.search(r"note: ([-0-9.e]+)n", vm)
        if not n:
            continue
        c = re.search(r"midichan: ([-0-9.e]+)f", vm)
        a = re.search(r"amp: ([-0-9.e]+)f", vm)
        evs.append({"t": float(t0), "d": float(t1) - float(t0),
                    "midi": int(round(float(n.group(1)))) + 60,
                    "ch": int(round(float(c.group(1)))) + 1 if c else None,
                    "vel": int(float(a.group(1)) * 127) if a else None})
    return evs


def by_bar(evs):
    out = {}
    for e in evs:
        bar = int((e["t"] - PRE_BARS * 4) // 4) + 1
        out.setdefault(bar, []).append(e)
    return out


def main():
    only = None
    if "--only" in sys.argv:
        only = sys.argv[sys.argv.index("--only") + 1]
    slots = slot_map()
    last = slots[-1][0]
    cycles = (last + PRE_BARS + 1) * 4
    renders = {}
    for rig in ("nano", "jv"):
        if only and rig != only:
            continue
        renders[rig] = by_bar(render(rig, cycles))
        print(f"{rig}: {sum(len(v) for v in renders[rig].values())} note events")
    problems = []
    rows = []
    for bar, section, name, mark, vel, n, (w0, w1), expect in slots:
        start = (bar - 1) * BAR_S
        row = {"slot": bar, "section": section, "instrument": name, "mark": mark,
               "start_s": start, "end_s": start + BAR_S,
               "win_start_s": round(start + w0, 2), "win_end_s": round(start + w1, 2),
               "midinotes": "", "velocity": vel, "ch_nano": "", "ch_jv": "", "expect": expect}
        for rig, bars in renders.items():
            evs = sorted(bars.get(bar, []), key=lambda e: e["midi"])
            if expect == f"silent:{rig}":
                if evs:
                    problems.append(f"bar {bar} {name}: {rig} should be silent, got {len(evs)} events")
                continue
            if len(evs) != n:
                problems.append(f"bar {bar} {name} {mark}: {rig} has {len(evs)} notes, expected {n}")
                continue
            bad = [e["vel"] for e in evs if e["vel"] != vel]
            if bad:
                problems.append(f"bar {bar} {name} {mark}: {rig} velocity {bad}, expected {vel}")
            for e in evs:
                if not (0 <= e["t"] - PRE_BARS * 4 - start < BAR_S):
                    problems.append(f"bar {bar} {name}: {rig} event at cycle {e['t']} outside its bar")
            notes = "+".join(str(e["midi"]) for e in evs)
            if row["midinotes"] and row["midinotes"] != notes and section != "unpitched":
                problems.append(f"bar {bar} {name}: notes differ between rigs ({row['midinotes']} vs {notes})")
            row["midinotes"] = notes if section != "unpitched" or not row["midinotes"] else row["midinotes"] + "/" + notes
            row[f"ch_{rig}"] = "+".join(str(e["ch"]) for e in evs)
        rows.append(row)
    # no bar outside the map may carry a note
    mapped = {s[0] for s in slots}
    for rig, bars in renders.items():
        stray = sorted(b for b in bars if b not in mapped)
        if stray:
            problems.append(f"{rig}: notes in unmapped bars {stray}")
    if problems:
        print("\n".join("PROBLEM " + p for p in problems), file=sys.stderr)
        sys.exit(1)
    with OUT.open("w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader(); w.writerows(rows)
    print(f"wrote {OUT.relative_to(ROOT)}: {len(rows)} slots, last bar {last}, "
          f"{(last * BAR_S) / 60:.1f} min after the marker")


if __name__ == "__main__":
    main()
