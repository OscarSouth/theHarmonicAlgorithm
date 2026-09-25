#!/usr/bin/env python3
"""The snippet prefix index in USER_GUIDE.md must match live/system/pulsar/snippets.cson.

Snippets are the primary live-performance interface, and the guide's prefix index
is how a reader discovers them. A snippet added or renamed without updating the
table leaves the guide quietly lying.
"""
import re, sys, pathlib

ROOT  = pathlib.Path(__file__).resolve().parents[2]
CSON  = ROOT / "live/system/pulsar/snippets.cson"
MD    = ROOT / "USER_GUIDE.md"

def cson_prefixes(text):
    return sorted({m.group(1) for m in re.finditer(r"^\s*'prefix':\s*'([^']+)'", text, re.M)})

def md_prefixes(text):
    """Prefixes listed in the 'prefix index' table only — not prose mentions."""
    m = re.search(r'\*\*The prefix index:\*\*(.*?)(?:\n\n(?!\|))', text, re.S)
    if not m:
        return None
    table = m.group(1)
    found = set()
    for line in table.splitlines():
        if not line.strip().startswith('|'):
            continue
        cells = line.split('|')
        if len(cells) < 3:
            continue
        # column 2 holds the prefixes; skip the header/separator rows
        found |= set(re.findall(r'`([a-z0-9\',]+)`', cells[2]))
    return sorted(found)

def main():
    cson = cson_prefixes(CSON.read_text())
    md   = md_prefixes(MD.read_text())

    if md is None:
        print("snippet index: FAILED to locate '**The prefix index:**' table in USER_GUIDE.md")
        return 1

    missing = sorted(set(cson) - set(md))   # in the library, undocumented
    extra   = sorted(set(md) - set(cson))   # documented, but no such snippet

    if missing or extra:
        print(f"snippet index: mismatch ({len(cson)} in snippets.cson, {len(md)} in the guide table)")
        for p in missing: print(f"  - {p!r} exists in snippets.cson but is missing from the guide table")
        for p in extra:   print(f"  - {p!r} is listed in the guide table but has no snippet")
        return 1

    print(f"snippet index: OK — {len(cson)} prefixes reconciled")
    return 0

if __name__ == "__main__":
    sys.exit(main())
