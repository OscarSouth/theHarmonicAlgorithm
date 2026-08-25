#!/usr/bin/env python3
"""Type-check every executable block in live/USER_GUIDE.tidal against the real library.

The guide IS this project's API surface, so a renamed function or changed signature
breaks the guide before it breaks anything else — and no unit test can see it.

How it works: every code block in the guide is spliced into one `do` block (so bindings
flow between sections exactly as they would in a live session) and bound, never forced.
GHC type-checks it; nothing runs. That matters because the guide is graph-first — its
examples use `seek "*"`, so executing them would need Neo4j.

Tidal boots headlessly here: `midi = s "thru"` is an OSC control name, not a hardware
binding, so no SuperCollider, MIDI device or audio hardware is involved.

Exit 0 = clean, 1 = type errors (reported at their real USER_GUIDE.tidal line), 2 = the
harness itself failed to run.
"""
import re, subprocess, sys, pathlib, tempfile, shutil, argparse

ROOT  = pathlib.Path(__file__).resolve().parents[2]
GUIDE = "live/USER_GUIDE.tidal"
BOOT  = "live/BootTidal.hs"
DONE  = "HARMONIC_GUIDE_TYPECHECK_DONE"

# GHCi reports the line AFTER the ':{' that opens the block: measured +1, constant
# across the payload (verified at both ends). payload_line = interactive_line - 1.
GHCI_LINE_OFFSET = 1

ERR = re.compile(r'^<interactive>:(\d+):(\d+)(?:-\d+)?:\s*(.*)$')

def sections(guide_text):
    """line number -> section heading, for human-readable error context."""
    out = {}
    for m in re.finditer(r'^--\s*SECTION (\d+)\s*[—-]\s*(.*)$', guide_text, re.M):
        out[guide_text[:m.start()].count('\n') + 1] = f"SECTION {m.group(1)} — {m.group(2).strip()}"
    return out

def section_for(line, secs):
    best = None
    for start, name in sorted(secs.items()):
        if start <= line:
            best = name
        else:
            break
    return best or "(preamble)"

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--keep", action="store_true", help="keep the generated payload for inspection")
    a = ap.parse_args()

    here = pathlib.Path(__file__).parent
    tmp  = pathlib.Path(tempfile.mkdtemp(prefix="guide-typecheck-"))
    try:
        return _run(here, tmp, a)
    finally:
        if a.keep:
            print(f"  payload kept in {tmp}")
        else:
            shutil.rmtree(tmp, ignore_errors=True)

def _run(here, tmp, a):
    payload, linemap = tmp / "payload.ghci", tmp / "payload.map"

    gen = subprocess.run(
        [sys.executable, str(here / "extract_guide_blocks.py"), GUIDE, "--map", str(linemap)],
        cwd=ROOT, capture_output=True, text=True)
    if gen.returncode != 0:
        print("guide typecheck: extractor failed\n" + gen.stderr, file=sys.stderr)
        return 2
    payload.write_text(gen.stdout)

    mapping = {}
    for row in linemap.read_text().splitlines():
        gen_ln, src_ln = row.split("\t")
        mapping[int(gen_ln)] = int(src_ln)

    session = tmp / "session.ghci"
    session.write_text(f":script {BOOT}\n{gen.stdout}\nputStrLn \"{DONE}\"\n:quit\n")

    # The guide deliberately binds results the reader is invited to play
    # with (pcNN <- ...), so unused-binding warnings are pedagogical noise
    # here, not defects — the gate's job is type errors.
    proc = subprocess.run(["stack", "ghci",
                           "--ghci-options=-v0 -Wno-unused-matches -Wno-unused-local-binds -Wno-unused-do-bind"],
                          cwd=ROOT, stdin=session.open(), capture_output=True, text=True)
    out = proc.stdout + proc.stderr

    if DONE not in out:
        print("guide typecheck: harness did not complete — GHCi failed to start or the boot "
              "file errored.\n", file=sys.stderr)
        print(out[-3000:], file=sys.stderr)
        return 2

    guide_text = (ROOT / GUIDE).read_text()
    secs = sections(guide_text)
    guide_lines = guide_text.splitlines()

    problems, seen = [], set()
    lines = out.splitlines()
    for i, line in enumerate(lines):
        m = ERR.match(line)
        if not m:
            continue
        inter_ln, col, head = int(m.group(1)), int(m.group(2)), m.group(3)
        src = mapping.get(inter_ln - GHCI_LINE_OFFSET, 0)
        if (src, head) in seen:
            continue
        seen.add((src, head))
        detail = [l for l in lines[i+1:i+6] if l.strip() and not ERR.match(l)]
        problems.append((src, col, head, detail))

    if not problems:
        n = sum(1 for l in guide_lines if l.strip() and not l.lstrip().startswith('--'))
        print(f"guide typecheck: OK — {len(secs)} sections, {n} code lines type-check "
              f"against the library")
        return 0

    print(f"guide typecheck: {len(problems)} error(s) in {GUIDE}")
    for src, col, head, detail in problems:
        where = f"{GUIDE}:{src}:{col}" if src else f"{GUIDE}:(unmapped):{col}"
        print(f"\n  {where}  [{section_for(src, secs)}]")
        if src and 0 < src <= len(guide_lines):
            print(f"    | {guide_lines[src-1].strip()}")
        print(f"    {head}")
        for d in detail:
            print(f"    {d.strip()}")
    return 1

if __name__ == "__main__":
    sys.exit(main())
