#!/usr/bin/env python3
"""Print the CHANGELOG.md section for a given version, for use as release notes.

Headings in this changelog are prose, not Keep-a-Changelog ("## Version 3.0.0 is
here! (2026)"), so match on the version number rather than an exact heading.
"""
import re, sys, pathlib, argparse

ROOT = pathlib.Path(__file__).resolve().parents[2]

def slice_notes(text, version):
    heads = list(re.finditer(r'^## .*?(\d+\.\d+(?:\.\d+)*)', text, re.M))
    for i, h in enumerate(heads):
        if h.group(1) == version:
            start = h.end() - len(h.group(0)) + h.group(0).index('##')
            start = h.start()
            end = heads[i + 1].start() if i + 1 < len(heads) else len(text)
            return text[start:end].strip()
    return None

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("version", help="e.g. 3.0.0 (a leading 'v' and any -rcN suffix are stripped)")
    a = ap.parse_args()

    version = re.sub(r'^v', '', a.version)
    version = re.sub(r'-.*$', '', version)          # v3.0.0-rc1 -> 3.0.0

    notes = slice_notes((ROOT / "CHANGELOG.md").read_text(), version)
    if notes is None:
        print(f"No CHANGELOG.md section found for version {version}", file=sys.stderr)
        return 1
    print(notes)
    return 0

if __name__ == "__main__":
    sys.exit(main())
