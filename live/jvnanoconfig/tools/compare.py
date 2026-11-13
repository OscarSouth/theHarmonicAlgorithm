#!/usr/bin/env python3
"""Compare two measured captures and say what to move on the JV.

usage: python3 live/jvnanoconfig/tools/compare.py levels_nano.csv levels_jv.csv
           [--anchor mean|tutti|INSTRUMENT] [--jv-levels jv_levels.tidal] [--out REPORT.md]

Both files come from measure.py. The nano is the reference: every delta is
jv − nano in dB, positive meaning the JV is louder.

Three numbers come out, because three different levers exist:

  trim      the median delta over the anchor (default: every sounding slot the
            rigs share). This is the global offset — the captures' gains and
            the module's master level — and is removed before anything else.
            It is the only thing the launcher's `d` should ever carry.
  offset    per instrument, the residual delta averaged over its marks. This
            is what channel volume (CC7) corrects: the suggested new value is
            v × 10^(−offset / 40), the GM volume law, a starting point not a
            formula. Current values are read from --jv-levels when given.
  slope     per instrument, the f − pppp spread on each rig and the difference.
            Channel volume cannot change this; it is the patch's velocity
            sensitivity on the JV (or a per-line `|* vel`).

Structural rows report the string-section chord against the sum of its parts
and name the slots only one rig can voice. Writes a Markdown report.
"""
import argparse
import csv
import math
import re
from collections import defaultdict
from datetime import date
from pathlib import Path


def load(path):
    rows = [r for r in csv.DictReader(l for l in open(path) if not l.startswith("#"))]
    return {(r["instrument"], r["mark"], r["section"]): r for r in rows}


def jv_levels(path):
    if not path or not Path(path).exists():
        return {}
    text = Path(path).read_text()
    m = re.search(r"jvLevel\s*=\s*\[(.*?)\]", text, re.S)
    if not m:
        return {}
    return {int(c): float(v) for c, v in re.findall(r"\(\s*(\d+)\s*,\s*([0-9.]+)\s*\)", m.group(1))}


def mean(xs):
    return sum(xs) / len(xs) if xs else float("nan")


def median(xs):
    xs = sorted(xs)
    if not xs:
        return float("nan")
    m = len(xs) // 2
    return xs[m] if len(xs) % 2 else (xs[m - 1] + xs[m]) / 2


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("nano"); ap.add_argument("jv")
    ap.add_argument("--anchor", default="mean")
    ap.add_argument("--jv-levels", default=str(Path(__file__).resolve().parents[1] / "jv_levels.tidal"))
    ap.add_argument("--timeline", default=str(Path(__file__).resolve().parents[1] / "timeline.csv"))
    ap.add_argument("--out")
    a = ap.parse_args()
    N, J = load(a.nano), load(a.jv)
    tl = {r["slot"]: r for r in csv.DictReader(open(a.timeline, newline=""))} if Path(a.timeline).exists() else {}
    chan = {r["instrument"]: r["ch_jv"] for r in tl.values() if r["section"] != "pizz"}
    levels = jv_levels(a.jv_levels)

    shared = [k for k in N if k in J and N[k]["status"] == "sound" and J[k]["status"] == "sound"]
    delta = {k: float(J[k]["rms_db"]) - float(N[k]["rms_db"]) for k in shared}
    if a.anchor == "mean":
        anchor = [k for k in shared if k[2] in ("pitched", "pizz", "unpitched")]
    elif a.anchor == "tutti":
        anchor = [k for k in shared if k[2] == "tutti"]
    else:
        anchor = [k for k in shared if k[0] == a.anchor]
    trim = median([delta[k] for k in anchor])

    lines = [f"# JV-1010 vs NanoSynth levels — {date.today().isoformat()}", "",
             f"Reference: `{Path(a.nano).name}`; measured: `{Path(a.jv).name}`. Deltas are jv − nano in dB.",
             f"Global trim (anchor `{a.anchor}`, {len(anchor)} slots): **{trim:+.1f} dB**. "
             "A median, so a few instruments that are off do not pull it. Remove it with the capture gain or the launcher's `d`; every figure below has it removed.", ""]

    lines += ["## Per instrument: residual offset → CC7", "",
              "| instrument | ch (jv) | pppp | p | f | offset | now | suggested |", "|---|---|---|---|---|---|---|---|"]
    by_inst = defaultdict(dict)
    for k in shared:
        if k[2] in ("pitched", "pizz", "unpitched"):
            name = k[0] + (" pizz" if k[2] == "pizz" else "")
            by_inst[name][k[1]] = delta[k] - trim
    offsets = {}
    for name, marks in by_inst.items():
        off = mean(list(marks.values())); offsets[name] = off
        base = name.replace(" pizz", "")
        ch = chan.get(base, "") if "pizz" not in name else "12"
        try:
            chn = int(ch.split("+")[0])
        except ValueError:
            chn = None
        now = levels.get(chn) if chn else None
        sugg = f"{min(1.0, now * 10 ** (-off / 40)):.2f}" if now is not None else "—"
        cell = lambda m: f"{marks[m]:+.1f}" if m in marks else "·"
        lines.append(f"| {name} | {ch} | {cell('pppp')} | {cell('p')} | {cell('f')} | **{off:+.1f}** | {now if now is not None else '—'} | {sugg} |")
    lines += ["", "Positive offset: the JV is louder there, lower its channel. Within ±2 dB is matched.", ""]

    lines += ["## Per instrument: dynamic slope (f − pppp), what CC7 cannot fix", "",
              "| instrument | nano | jv | difference |", "|---|---|---|---|"]
    for name in by_inst:
        sec = "pizz" if "pizz" in name else None
        base = name.replace(" pizz", "")
        def spread(T):
            keys = {m: (base, m, s) for (i, m, s) in T if i == base and (s == sec if sec else s != "pizz")}
            if "f" in keys and "pppp" in keys and keys["f"] in T and keys["pppp"] in T:
                return float(T[keys["f"]]["rms_db"]) - float(T[keys["pppp"]]["rms_db"])
            return None
        sn, sj = spread(N), spread(J)
        if sn is None or sj is None:
            lines.append(f"| {name} | · | · | · |"); continue
        lines.append(f"| {name} | {sn:.1f} | {sj:.1f} | {sj - sn:+.1f} |")
    lines += ["", "A difference beyond ±2 dB is the patch's velocity response on the JV; CC7 moves the whole line, not its spread.", ""]

    lines += ["## Structure", ""]
    for sec, label in (("section", "string section chord (p)"), ("tutti", "tutti")):
        for k in sorted(k for k in shared if k[2] == sec):
            lines.append(f"- {label} {k[1]}: {delta[k] - trim:+.1f} dB after trim")
    parts = [k for k in shared if k[2] == "pitched" and k[0] in ("violin1", "violin2", "viola", "cello", "contrabass") and k[1] == "p"]
    if parts:
        lines.append(f"- mean of the five string parts at p: {mean([delta[k] - trim for k in parts]):+.1f} dB "
                     "(compare with the chord row: on the JV the five share one channel and one patch)")
    only = [f"{k[0]} {k[1]}" for k in N if k not in shared or N[k]["status"] != "sound" or J[k]["status"] != "sound"]
    if only:
        lines.append("- not compared (silent or missing on one rig): " + ", ".join(sorted(set(only))))
    lines += ["", f"Worst residual: {max(abs(v) for v in offsets.values()):.1f} dB" if offsets else ""]

    report = "\n".join(lines)
    out = Path(a.out) if a.out else Path(a.jv).parent / f"compare_{date.today().isoformat()}.md"
    out.write_text(report + "\n")
    print(report); print(f"\n-> {out}")


if __name__ == "__main__":
    main()
