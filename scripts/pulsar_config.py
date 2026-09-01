#!/usr/bin/env python3
"""Ensure Pulsar's tidalcycles settings, idempotently.

Pulsar stores settings in ~/.pulsar/config.cson. CSON is CoffeeScript object
notation -- indentation-structured, not JSON -- so this edits the
`tidalcycles:` block by indentation rather than parsing the whole file. Keys
outside that block are copied through byte-for-byte.

Settings and their reasons (see documents/LIVE_ENVIRONMENT.md):
  interpreter/ghciPath   the plugin spawns its interpreter with no arguments, so
                         the -O2 flags must ride on a wrapper script
  bootTidalPath empty    resolution falls to the project root, live/. Set
                         elsewhere, the session boots cleanly with none of this
                         project's definitions and prints no error
  showErrorNotifications ordinary build chatter on stderr raises modal
                         notifications when enabled
  eventHighlighting      requires plugin >= 4.1.3, which ships
                         event-highlighter.js

Usage:
  pulsar_config.py --ghci-path /abs/path/to/live/system/bin/ghci [--config FILE] [--check]

--check reports what would change and exits 1 if anything would; it writes
nothing. Exit 0 means already correct.
"""
import argparse, os, sys

BLOCK = "tidalcycles"


def parse_block(lines, start):
    """Return (keys, end) for the block whose header is at `start`.

    keys maps name -> str (scalar line remainder) or list[str] (nested lines).
    """
    header_indent = len(lines[start]) - len(lines[start].lstrip())
    body_indent = header_indent + 2
    keys, i = {}, start + 1
    while i < len(lines):
        line = lines[i]
        if line.strip() == "":
            i += 1
            continue
        indent = len(line) - len(line.lstrip())
        if indent <= header_indent:
            break
        if indent == body_indent:
            name, _, rest = line.strip().partition(":")
            if rest.strip():
                keys[name] = rest.strip()
                i += 1
            else:                                   # nested sub-block
                sub, i = [], i + 1
                while i < len(lines):
                    if lines[i].strip() == "":
                        i += 1
                        continue
                    ind = len(lines[i]) - len(lines[i].lstrip())
                    if ind <= body_indent:
                        break
                    sub.append(lines[i].strip())
                    i += 1
                keys[name] = sub
        else:
            i += 1
    return keys, i


def emit(keys, indent):
    out = []
    for name in sorted(keys):
        v = keys[name]
        if isinstance(v, list):
            out.append("%s%s:" % (" " * indent, name))
            out += ["%s%s" % (" " * (indent + 2), s) for s in v]
        else:
            out.append("%s%s: %s" % (" " * indent, name, v))
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--ghci-path", required=True)
    ap.add_argument("--config", default=os.path.expanduser("~/.pulsar/config.cson"))
    ap.add_argument("--check", action="store_true")
    a = ap.parse_args()

    want = {
        "ghciPath": '"%s"' % a.ghci_path,
        "interpreter": '"default"',
        "showErrorNotifications": "false",
        "eventHighlighting": ["enable: true", "fps: 20"],
        "superDirt": ["autostart: true"],
    }

    if not os.path.exists(a.config):
        print("  config.cson not found at %s -- start Pulsar once first" % a.config)
        return 1

    src = open(a.config).read()
    lines = src.split("\n")

    hdr = next((i for i, l in enumerate(lines)
                if l.strip() == BLOCK + ":" and l.startswith("  ")), None)
    if hdr is None:
        print("  no `tidalcycles:` block in config.cson -- open Pulsar with the")
        print("  package installed once so it writes its defaults, then re-run")
        return 1

    indent = len(lines[hdr]) - len(lines[hdr].lstrip()) + 2
    keys, end = parse_block(lines, hdr)

    changes = []
    for k, v in want.items():
        if keys.get(k) != v:
            changes.append("%s: %r -> %r" % (k, keys.get(k), v))
            keys[k] = v
    # bootTidalPath absent or empty: resolution falls to the project root
    if keys.get("bootTidalPath") not in (None, '""'):
        changes.append("bootTidalPath: %r -> removed" % keys["bootTidalPath"])
        del keys["bootTidalPath"]

    if not changes:
        print("  Pulsar settings: already correct")
        return 0

    for c in changes:
        print("  Pulsar settings: " + c)
    if a.check:
        return 1

    lines[hdr + 1:end] = emit(keys, indent)
    open(a.config, "w").write("\n".join(lines))
    print("  Pulsar settings: written")
    return 0


if __name__ == "__main__":
    sys.exit(main())
