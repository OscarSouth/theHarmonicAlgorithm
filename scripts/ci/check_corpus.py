#!/usr/bin/env python3
"""Type-check real performance files against the library.

The guide gate covers `live/USER_GUIDE.tidal`. It is not enough on its own:
the guide is written to the API as documented, while the arrangement files are
written to the API as it actually behaves, and the two drifted apart unnoticed
(`oct` was declared `Int -> Pattern ValueMap` in the library while every
performance file called it with a mini-notation pattern).

Blocks are never evaluated. Expression blocks are bound to a throwaway name so
GHCi type-checks them without running them; declaration blocks are entered as
declarations, which also does not run anything. Nothing here reaches an audio
target, and nothing needs Neo4j.
"""
import re
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
BOOT = "live/BootTidal.hs"
DONE = "__CORPUS_CHECK_COMPLETE__"

# The files that exercise the live surface hardest: a full performance state,
# a launcher with every instrument family, a walking-bass score, and a drum
# part. Add to this list rather than widening it to a glob — the point is a
# fast gate on representative code, not a whole-corpus sweep.
# Order matters: files are fed to one session the way a performer evaluates
# them, arrangement before launcher. `live/local` is git-ignored, so those
# entries are checked on the performer's machine and skipped in CI.
DEFAULT_TARGETS = [
    "live/local/older/misc/2026-05-01_office.tidal",
    "live/local/perform/state.tidal",
    "live/local/older/concert/06-rosslynCastle.tidal",
    "live/docs/JAZZ_DIAGNOSTIC.tidal",
    "live/docs/ORCHESTRAL_CATALOGUE.tidal",
    "live/drumpats/afro-cuban/bolero.tidal",
    "live/drumpats/dub/rockers.tidal",
]

# A binding at column 0 followed by `=` is a declaration group; anything else
# that is not a bind or a GHCi command is an expression we can name and drop.
DECL = re.compile(r"^[a-z_][\w'.]*[^=\n]*=(?!=)", re.M)
# `s <- seek "*" $ gen` becomes a lazy `let`, so the name carries its real type
# into later blocks without the generation ever running.
BIND = re.compile(r"^([a-z_][\w']*)\s*<-\s*(.*)$")
ERR = re.compile(r"^<interactive>:(\d+):(\d+):\s*error:\s*(.*)$")


def blocks(text):
    """Split on blank lines, the way the editor does."""
    for raw in text.split("\n\n"):
        block = "\n".join(
            ln for ln in raw.splitlines() if not ln.lstrip().startswith("--")
        ).strip("\n")
        if block.strip():
            yield block


def payload(targets):
    """GHCi lines, plus a map from GHCi line number back to (file, block)."""
    lines, origins = [], {}
    for target in targets:
        text = (ROOT / target).read_text()
        for n, block in enumerate(blocks(text)):
            if block.lstrip().startswith(":"):
                continue
            head, *rest = block.splitlines()
            # A file may pull in a module of its own; imports are entered as
            # imports, never wrapped.
            if head.startswith("import"):
                lines.extend(block.splitlines())
                continue
            bind = BIND.match(head)
            if bind:
                name, first = bind.group(1), bind.group(2)
                body = f"let {name} = unsafePerformIO (\n" + "\n".join(
                    ["      " + first] + ["      " + ln for ln in rest]
                ) + "\n      )"
            elif DECL.search(block):
                body = block
            else:
                body = "let _chk =\n" + "\n".join(
                    "      " + ln for ln in block.splitlines()
                )
            origins[len(lines)] = (target, block.splitlines()[0][:70])
            lines.extend([":{", body, ":}"])
    return lines, origins


def main():
    explicit = bool(sys.argv[1:])
    targets = sys.argv[1:] or DEFAULT_TARGETS
    missing = [t for t in targets if not (ROOT / t).exists()]
    if missing and explicit:
        print(f"corpus typecheck: missing {', '.join(missing)}", file=sys.stderr)
        return 2
    targets = [t for t in targets if t not in missing]
    if missing:
        print(f"corpus typecheck: skipping {len(missing)} file(s) not present "
              f"({', '.join(missing)})")
    if not targets:
        print("corpus typecheck: no target files present — nothing to check")
        return 0

    lines, origins = payload(targets)

    # Same reset as the guide gate: `:script` then `:m` plus the boot file's
    # own imports, so the session sees exactly the surface Pulsar sees.
    boot_imports = "\n".join(
        ln.rstrip() for ln in (ROOT / BOOT).read_text().splitlines()
        if ln.startswith("import ")
    )

    with tempfile.TemporaryDirectory() as tmp:
        session = Path(tmp) / "session.ghci"
        session.write_text(
            f":script {BOOT}\n:m\n{boot_imports}\n"
            "import System.IO.Unsafe (unsafePerformIO)\n"
            + "\n".join(lines)
            + f'\nputStrLn "{DONE}"\n:quit\n'
        )
        proc = subprocess.run(
            ["stack", "ghci",
             "--ghci-options=-v0 -Wno-unused-matches -Wno-unused-local-binds "
             "-Wno-unused-do-bind -Wno-name-shadowing -Wno-missing-signatures"],
            cwd=ROOT, stdin=session.open(), capture_output=True, text=True)

    out = proc.stdout + proc.stderr
    if DONE not in out:
        print("corpus typecheck: harness did not complete — GHCi failed to "
              "start or the boot file errored.\n", file=sys.stderr)
        print(out[-3000:], file=sys.stderr)
        return 2

    seen, problems, scope = set(), [], []
    all_lines = out.splitlines()
    for i, line in enumerate(all_lines):
        m = ERR.match(line)
        if not m:
            continue
        head = m.group(3)
        detail = [l for l in all_lines[i + 1:i + 6]
                  if l.strip() and not ERR.match(l)]
        key = (head, tuple(detail[:2]))
        if key in seen:
            continue
        seen.add(key)
        # A performance file is one voice in a session of several, so a name it
        # expects from a sibling file is missing context, not drift. Scope
        # errors are reported and tolerated; a type mismatch is the signal this
        # gate exists for, and fails.
        blob = " ".join([head] + detail)
        (scope if "not in scope" in blob else problems).append((head, detail))

    def show(label, items, stream):
        print(f"corpus typecheck: {len(items)} {label}", file=stream)
        for head, detail in items:
            print(f"  {head}", file=stream)
            for d in detail:
                print(f"    {d}", file=stream)

    if scope:
        show("name(s) expected from a sibling file", scope, sys.stdout)
    if problems:
        show("type problem(s)", problems, sys.stderr)
        return 1

    checked = len(lines) // 3
    print(f"corpus typecheck: OK — {checked} blocks across "
          f"{len(targets)} performance files type-check against the library")
    return 0


if __name__ == "__main__":
    sys.exit(main())
