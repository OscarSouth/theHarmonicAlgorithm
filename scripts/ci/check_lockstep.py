#!/usr/bin/env python3
"""USER_GUIDE.md and live/USER_GUIDE.tidal must stay section-for-section in lockstep.

The two guides are meant to be read side by side: section N of the markdown is
section N of the executable file. Nothing enforces that today, so drift is silent.
"""
import re, sys, pathlib

ROOT = pathlib.Path(__file__).resolve().parents[2]
MD   = ROOT / "USER_GUIDE.md"
TIDAL= ROOT / "live/USER_GUIDE.tidal"

def md_sections(text):
    # "## 7. Voicing strategies"
    return [(int(m.group(1)), m.group(2).strip())
            for m in re.finditer(r'^## (\d+)\.\s+(.*)$', text, re.M)]

def tidal_sections(text):
    # "-- SECTION 7 — Voicing strategies"
    return [(int(m.group(1)), m.group(2).strip())
            for m in re.finditer(r'^--\s*SECTION (\d+)\s*[—-]\s*(.*)$', text, re.M)]

def norm(s):
    s = s.lower()
    s = re.sub(r'[`*_]', '', s)                 # markdown emphasis / code ticks
    s = re.sub(r'\s*\(.*?\)\s*', ' ', s)        # parenthetical asides
    s = re.sub(r'[^a-z0-9 ]', ' ', s)
    return ' '.join(s.split())

def main():
    md, td = md_sections(MD.read_text()), tidal_sections(TIDAL.read_text())
    problems = []

    if len(md) != len(td):
        problems.append(f"section count differs: USER_GUIDE.md has {len(md)}, "
                        f"live/USER_GUIDE.tidal has {len(td)}")

    mdn, tdn = {n: t for n, t in md}, {n: t for n, t in td}
    for n in sorted(set(mdn) | set(tdn)):
        if n not in mdn:
            problems.append(f"section {n} ({tdn[n]!r}) is in the .tidal guide but not the .md")
        elif n not in tdn:
            problems.append(f"section {n} ({mdn[n]!r}) is in the .md but not the .tidal guide")
        elif norm(mdn[n]) != norm(tdn[n]):
            problems.append(f"section {n} titles diverge:\n"
                            f"      md    : {mdn[n]}\n"
                            f"      tidal : {tdn[n]}")

    if problems:
        print(f"guide lockstep: {len(problems)} problem(s)")
        for p in problems: print(f"  - {p}")
        return 1
    print(f"guide lockstep: OK — {len(md)} sections aligned")
    return 0

if __name__ == "__main__":
    sys.exit(main())
