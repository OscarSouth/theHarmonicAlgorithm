#!/usr/bin/env python3
"""Compare two measured captures and say what to move on the JV.

usage: python3 live/jvnanoconfig/tools/compare.py levels_nano.csv levels_jv.csv
           [--levels part_levels.json] [--law 40] [--out REPORT.md]

Both files come from measure.py; the figure compared is lk_db. The nano is the
reference: every delta is jv − nano in dB, positive meaning the JV is louder.

  trim     the median delta over every single-voice slot both rigs sound. It
           is the two recording chains' gain difference and is removed first;
           nothing on the module should chase it.
  offset   per voice, the residual averaged over its marks. The part level
           corrects it. Voices that share a JV part are shown together: the
           part takes their median, and what remains per voice (`in part`) is
           a correction inside the patch or rhythm set.
  spread   per voice, pppp→p and p→f on each rig. A difference is the patch's
           velocity response; no level moves it.

--levels is a JSON object of JV channel → current part level (0–127); with it
the report suggests a new level, now × 10^(−offset / law). --law is the dB per
decade of part level; the JV-1010 measures 40.
"""
import argparse
import csv
import json
from collections import defaultdict
from datetime import date
from pathlib import Path
from statistics import mean, median

MARKS = ("pppp", "p", "f")
VOICE_SECTIONS = ("pitched", "pizz", "unpitched")


def load(path):
    rows = csv.DictReader(l for l in open(path) if not l.startswith("#"))
    return {(r["instrument"], r["mark"], r["section"]): r for r in rows}


def fmt(v):
    return "·" if v is None else f"{v:+.1f}"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("nano"); ap.add_argument("jv")
    ap.add_argument("--levels"); ap.add_argument("--law", type=float, default=40.0)
    ap.add_argument("--timeline", default=str(Path(__file__).resolve().parents[1] / "timeline.csv"))
    ap.add_argument("--out")
    a = ap.parse_args()
    N, J = load(a.nano), load(a.jv)
    chan = {(r["instrument"], r["section"]): r["ch_jv"] for r in csv.DictReader(open(a.timeline, newline=""))}
    levels = {int(c): v for c, v in json.load(open(a.levels)).items()} if a.levels else {}

    sound = lambda T, k: k in T and T[k]["status"] == "sound"
    shared = [k for k in N if sound(N, k) and sound(J, k)]
    delta = {k: float(J[k]["lk_db"]) - float(N[k]["lk_db"]) for k in shared}
    voices = [k for k in shared if k[2] in VOICE_SECTIONS]
    trim = median(delta[k] for k in voices)

    # voice = (instrument, section); rows grouped by JV part
    by_voice = defaultdict(dict)
    for inst, mark, sec in voices:
        by_voice[(inst, sec)][mark] = delta[(inst, mark, sec)] - trim
    offset = {v: mean(m.values()) for v, m in by_voice.items()}
    parts = defaultdict(list)
    for v in by_voice:
        parts[int(chan[v])].append(v)
    name = lambda v: v[0] + (" pizz" if v[1] == "pizz" else "")

    lines = [f"# JV-1010 vs NanoSynth — {date.today().isoformat()}", "",
             f"Reference `{Path(a.nano).name}`, measured `{Path(a.jv).name}`. Deltas are jv − nano in dB, "
             f"trim **{trim:+.1f} dB** ({len(voices)} slots) removed.", "",
             "## Level: offset per voice, grouped by JV part", "",
             "| part | voice | pppp | p | f | offset | part offset | in part | level now | suggested |",
             "|---|---|---|---|---|---|---|---|---|---|"]
    for ch in sorted(parts):
        part_off = median(offset[v] for v in parts[ch])
        now = levels.get(ch)
        sugg = "—" if now is None else str(max(0, min(127, round(now * 10 ** (-part_off / a.law)))))
        for i, v in enumerate(parts[ch]):
            m = by_voice[v]
            head = [str(ch), name(v)] + [fmt(m.get(x)) for x in MARKS] + [f"**{offset[v]:+.1f}**"]
            tail = [f"{part_off:+.1f}", fmt(offset[v] - part_off), "—" if now is None else str(now), sugg] if i == 0 \
                else ["", fmt(offset[v] - part_off), "", ""]
            lines.append("| " + " | ".join(head + tail) + " |")
    lines += ["", "Positive: the JV is louder, lower it. `in part` is what the part level cannot reach.", "",
              "## Dynamics: spread per voice", "",
              "| voice | nano pppp→p | jv pppp→p | diff | nano p→f | jv p→f | diff |", "|---|---|---|---|---|---|---|"]
    worst_spread = 0.0
    for v in by_voice:
        cells = [name(v)]
        for lo, hi in (("pppp", "p"), ("p", "f")):
            kl, kh = (v[0], lo, v[1]), (v[0], hi, v[1])
            if all(sound(T, k) for T in (N, J) for k in (kl, kh)):
                sn = float(N[kh]["lk_db"]) - float(N[kl]["lk_db"])
                sj = float(J[kh]["lk_db"]) - float(J[kl]["lk_db"])
                cells += [f"{sn:.1f}", f"{sj:.1f}", f"{sj - sn:+.1f}"]
                worst_spread = max(worst_spread, abs(sj - sn))
            else:
                cells += ["·", "·", "·"]
        lines.append("| " + " | ".join(cells) + " |")
    lines += ["", "Negative diff: the JV is flatter there than the nano.", "", "## Structure", ""]
    for sec, label in (("section", "string section chord"), ("tutti", "tutti")):
        for k in (k for k in shared if k[2] == sec):
            lines.append(f"- {label} {k[1]}: {delta[k] - trim:+.1f} dB")
    only = sorted({f"{k[0]} {k[1]}" for T in (N, J) for k in T if k not in shared})
    if only:
        lines.append("- not compared (silent or missing on one rig): " + ", ".join(only))
    lines += ["", f"Worst level offset {max(abs(o) for o in offset.values()):.1f} dB, "
                  f"worst spread difference {worst_spread:.1f} dB."]

    report = "\n".join(lines)
    out = Path(a.out) if a.out else Path(a.jv).parent / f"compare_{date.today().isoformat()}.md"
    out.write_text(report + "\n")
    print(report); print(f"\n-> {out}")


if __name__ == "__main__":
    main()
