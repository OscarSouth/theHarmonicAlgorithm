#!/usr/bin/env python3
"""Turn live/USER_GUIDE.tidal into a single GHCi type-check payload.

Why one big `do` block rather than block-by-block: the guide is cumulative — later
sections use bindings from earlier ones (`s`, `start`, `tempo`, `form`). Splicing every
statement into one `do` block makes types flow exactly as they would in a real session,
so a renamed function or a changed signature anywhere shows up as a type error.

Nothing is executed. The block is *bound*, never forced, so GHC type-checks it and stops.
That matters because the guide is graph-first — running it would need Neo4j.

Emits the payload on stdout and, with --map, a "generated-line -> guide-line" TSV so CI
can point at the real file.
"""
import re, sys, pathlib, argparse

BINDING = re.compile(r"""^
    (?: \( [^)]* \)            # tuple binding: (rhythm, contour, motif) = ...
      | [a-z_][A-Za-z0-9_']*   # plain or function binding: form = ... / p19 f k d = ...
        (?:\s+[A-Za-z0-9_'\[\](),]+)*
    )
    \s* = (?!=)                # '=' but not '=='
""", re.X)

FN_NAME = "_harmonicGuideTypecheck"

def is_comment(line):
    return line.lstrip().startswith('--')

def blocks(lines):
    """Group into blocks separated by BLANK lines only.

    Comment lines must not split a block: the guide routinely puts explanatory
    comments *inside* a multi-line expression, e.g.

        $ stack [silence
          -- Chord pad throughout
          , arrange (0, 1) k ...

    Splitting there would orphan the continuation and produce a parse error.
    GHC's layout algorithm ignores comments entirely, so they are kept in place.
    Blocks holding no code at all are dropped by the caller.
    """
    cur, out = [], []
    for i, raw in enumerate(lines, 1):
        if not raw.strip():
            if cur: out.append(cur); cur = []
            continue
        cur.append((i, raw.rstrip('\n')))
    if cur: out.append(cur)
    return out

def build(path):
    lines = pathlib.Path(path).read_text().splitlines(True)
    payload, linemap = [], []

    def emit(text, src=0):
        payload.append(text)
        linemap.append(src)

    emit(":{")
    emit(f"{FN_NAME} = do")

    for blk in blocks(lines):
        code = [(ln, t) for ln, t in blk if not is_comment(t)]
        if not code:
            continue                       # prose-only block
        first = code[0][1]
        if '<-' in first.split('--')[0] or not BINDING.match(first):
            # a statement: hush, print s, once $ ..., do ..., x <- action
            for ln, text in blk:
                emit("  " + text, ln)
        else:
            # a definition: needs `let` inside do-notation. Its own `let` line keeps
            # the original relative indentation valid under the layout rule.
            emit("  let", code[0][0])
            for ln, text in blk:
                emit("    " + text, ln)

    emit("  pure ()")
    emit(":}")
    return payload, linemap

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("guide", nargs="?", default="live/USER_GUIDE.tidal")
    ap.add_argument("--map", help="write generated-line -> guide-line TSV here")
    a = ap.parse_args()

    payload, linemap = build(a.guide)
    sys.stdout.write("\n".join(payload) + "\n")
    if a.map:
        with open(a.map, "w") as f:
            for gen, src in enumerate(linemap, 1):
                f.write(f"{gen}\t{src}\n")
    return 0

if __name__ == "__main__":
    sys.exit(main())
