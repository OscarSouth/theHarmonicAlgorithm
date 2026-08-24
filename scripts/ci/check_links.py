#!/usr/bin/env python3
"""Relative links in the public docs must resolve to TRACKED files.

A link can resolve fine on the author's machine and 404 for everyone else, because
the target is present locally but gitignored. That has bitten this repo before
(data/artefacts, live/local, notes), so the check tests against `git ls-files`
rather than the filesystem.
"""
import re, subprocess, sys, pathlib

ROOT = pathlib.Path(__file__).resolve().parents[2]

DOCS = [ROOT / "README.md", ROOT / "USER_GUIDE.md", ROOT / "CHANGELOG.md"]
DOCS += sorted((ROOT / "documents").glob("*.md"))

LINK = re.compile(r'\[[^\]]*\]\(([^)]+)\)')

def tracked():
    out = subprocess.run(["git", "ls-files"], cwd=ROOT, capture_output=True,
                         text=True, check=True).stdout
    return set(out.split("\n"))

def main():
    files = tracked()
    problems = []
    checked = 0

    for doc in DOCS:
        if not doc.exists():
            continue
        rel_doc = doc.relative_to(ROOT)
        for lineno, line in enumerate(doc.read_text().splitlines(), 1):
            for target in LINK.findall(line):
                target = target.strip()
                # external, anchors, mailto — not our problem
                if re.match(r'^(https?:|mailto:|#)', target):
                    continue
                path = target.split('#', 1)[0].split('?', 1)[0]
                if not path:
                    continue
                checked += 1
                resolved = (doc.parent / path).resolve()
                try:
                    rel = resolved.relative_to(ROOT).as_posix()
                except ValueError:
                    problems.append(f"{rel_doc}:{lineno}: {target!r} escapes the repository")
                    continue
                # a directory is fine if git tracks anything beneath it
                if rel in files or any(f.startswith(rel + "/") for f in files):
                    continue
                hint = " (exists locally but is NOT tracked by git)" if resolved.exists() else ""
                problems.append(f"{rel_doc}:{lineno}: {target!r} does not resolve to a tracked file{hint}")

    if problems:
        print(f"doc links: {len(problems)} problem(s) across {checked} relative link(s)")
        for p in problems: print(f"  - {p}")
        return 1
    print(f"doc links: OK — {checked} relative links all resolve to tracked files")
    return 0

if __name__ == "__main__":
    sys.exit(main())
