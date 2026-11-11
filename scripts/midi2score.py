#!/usr/bin/env python3
"""Harvest a Standard MIDI File into score-mode text.

For transcriptions whose notation exists as MIDI (a LilyPond or MuseScore render of
an edition): every track becomes one or more `harp' M 1 "p" k (staff beats [...])`
lines, one string per bar, so the piece can be placed with `bars` in a score-mode
piece and checked bar by bar against the sheet music.

    python3 -I scripts/midi2score.py piece.mid --blocks 1-16,17-46 [--instrument harp'] [--mark pp]

Rules:
- Degrees are chromatic on C at octave 1 (`key = inKey C "chromatic"`), so a degree
  is Tidal's own note number (0 = C4, 12 = C5, -12 = C3).
- A track's notes are split into monophonic layers: a note joins the first layer whose
  previous note has ended by the note's onset; notes with the same onset and length
  become a chord `[a,b]`. Each layer is one instrument line.
- A block (a range of bars) is one weighted sequence per layer, in units of the block's
  tick GCD, written one bar per string: a note held across a bar line is one step whose
  weight spans the line, so it is not re-attacked; the following bar's string then
  holds only that bar's remaining events.
- Time-signature changes are honoured per bar (a 2/4 bar is two beats of the block).
- A note that another track already sounds at the same onset and pitch is dropped
  (LilyPond editions double a note with hidden noteheads to carry a slur across
  staves); each drop is reported.
- Dynamics are not read (LilyPond renders drop them when told to); marks are written
  by hand from the score. Tempo meta events are reported, not emitted.

Pure Python, no dependencies.
"""
import argparse
import math
import sys
from collections import defaultdict

NOTE_NAMES = ["c", "cs", "d", "ds", "e", "f", "fs", "g", "gs", "a", "as", "b"]


# --------------------------------------------------------------------------- SMF
def read_vlq(data, i):
    v = 0
    while True:
        b = data[i]
        i += 1
        v = (v << 7) | (b & 0x7F)
        if not b & 0x80:
            return v, i


def parse_smf(data):
    assert data[:4] == b"MThd", "not a Standard MIDI File"
    hlen = int.from_bytes(data[4:8], "big")
    fmt = int.from_bytes(data[8:10], "big")
    ntrk = int.from_bytes(data[10:12], "big")
    division = int.from_bytes(data[12:14], "big")
    assert division & 0x8000 == 0, "SMPTE time division not supported"
    i = 8 + hlen
    tracks = []
    for _ in range(ntrk):
        assert data[i:i + 4] == b"MTrk", "track chunk expected"
        tlen = int.from_bytes(data[i + 4:i + 8], "big")
        i += 8
        end = i + tlen
        t = 0
        status = None
        events = []  # (tick, kind, payload)
        while i < end:
            delta, i = read_vlq(data, i)
            t += delta
            b = data[i]
            if b == 0xFF:
                meta = data[i + 1]
                ln, j = read_vlq(data, i + 2)
                payload = data[j:j + ln]
                i = j + ln
                events.append((t, "meta", (meta, payload)))
                continue
            if b in (0xF0, 0xF7):
                ln, j = read_vlq(data, i + 1)
                i = j + ln
                continue
            if b & 0x80:
                status = b
                i += 1
            if status is None:
                raise ValueError("running status without a status byte")
            kind = status & 0xF0
            chan = status & 0x0F
            if kind in (0x80, 0x90, 0xA0, 0xB0, 0xE0):
                d1, d2 = data[i], data[i + 1]
                i += 2
                if kind == 0x90 and d2 > 0:
                    events.append((t, "on", (chan, d1, d2)))
                elif kind in (0x80, 0x90):
                    events.append((t, "off", (chan, d1)))
            elif kind in (0xC0, 0xD0):
                i += 1
        tracks.append(events)
        i = end
    return fmt, division, tracks


def notes_of(events):
    """(start, end, pitch, velocity, channel) per note; a note-on that is never
    released ends at the track's last tick."""
    open_notes = {}
    notes = []
    last = 0
    for t, kind, p in events:
        last = max(last, t)
        if kind == "on":
            chan, pitch, vel = p
            open_notes.setdefault((chan, pitch), []).append((t, vel))
        elif kind == "off":
            chan, pitch = p
            stack = open_notes.get((chan, pitch))
            if stack:
                t0, vel = stack.pop(0)
                if t > t0:
                    notes.append((t0, t, pitch, vel, chan))
    for (chan, pitch), stack in open_notes.items():
        for t0, vel in stack:
            if last > t0:
                notes.append((t0, last, pitch, vel, chan))
    return sorted(notes)


def meta_of(tracks):
    tempos, timesigs, names = [], [], []
    for ti, ev in enumerate(tracks):
        for t, kind, p in ev:
            if kind != "meta":
                continue
            m, payload = p
            if m == 0x51:
                us = int.from_bytes(payload, "big")
                tempos.append((t, 60_000_000 / us))
            elif m == 0x58:
                timesigs.append((t, payload[0], 2 ** payload[1]))
            elif m == 0x03:
                names.append((ti, payload.decode("latin-1")))
    return sorted(tempos), sorted(timesigs), names


# --------------------------------------------------------------------------- bars
def bar_table(timesigs, division, last_tick):
    """[(bar_no, start_tick, length_ticks, beats)] from tick 0 to past last_tick."""
    sigs = timesigs or [(0, 4, 4)]
    if sigs[0][0] != 0:
        sigs = [(0, 4, 4)] + sigs
    bars, t, n, si = [], 0, 1, 0
    while t <= last_tick:
        while si + 1 < len(sigs) and sigs[si + 1][0] <= t:
            si += 1
        _, num, den = sigs[si]
        beats = num * 4 / den
        length = int(division * beats)
        bars.append((n, t, length, beats))
        t += length
        n += 1
    return bars


# --------------------------------------------------------------------------- layers
def layers_of(notes):
    """Monophonic layers: chords (same start and end) stay together; a chord joins the
    first layer that is free at its onset."""
    groups = defaultdict(list)
    for s, e, p, v, c in notes:
        groups[(s, e)].append(p)
    chords = sorted((s, e, sorted(ps)) for (s, e), ps in groups.items())
    layers = []  # each: list of (start, end, [pitches])
    ends = []
    for s, e, ps in chords:
        for li, le in enumerate(ends):
            if le <= s:
                layers[li].append((s, e, ps))
                ends[li] = e
                break
        else:
            layers.append([(s, e, ps)])
            ends.append(e)
    return layers


def tidal_note(p):
    return str(p - 60)


def note_name(p):
    return f"{NOTE_NAMES[p % 12]}{p // 12 - 1}"


def encode_block(layer, bars, b1, b2):
    """One weighted sequence per bar string for bars b1..b2 of a layer, or None
    when the layer is silent there. Returns (strings, beats, comments, ties)."""
    sel = [b for b in bars if b1 <= b[0] <= b2]
    start = sel[0][1]
    end = sel[-1][1] + sel[-1][2]
    beats = int(sum(b[3] for b in sel))
    evs = [(s, e, ps) for s, e, ps in layer if s < end and e > start]
    if not evs:
        return None
    # clip to the block; collect boundaries for the unit
    evs = [(max(s, start), min(e, end), ps) for s, e, ps in evs]
    bounds = {start, end}
    for b in sel:
        bounds.add(b[1])
    for s, e, _ in evs:
        bounds.update((s, e))
    unit = 0
    for x in bounds:
        unit = math.gcd(unit, x - start)
    unit = unit or 1
    # steps: notes and the rests between them, in block order; a rest is cut at
    # every bar line (only a note is held across one)
    steps = []  # (start, end, pitches or None)
    bar_starts = [b[1] for b in sel]

    def rest(a, b):
        for bs in bar_starts:
            if a < bs < b:
                steps.append((a, bs, None))
                a = bs
        steps.append((a, b, None))

    cursor = start
    for s, e, ps in evs:
        if s > cursor:
            rest(cursor, s)
        steps.append((s, e, ps))
        cursor = e
    if cursor < end:
        rest(cursor, end)
    # one string per bar: a step belongs to the bar it starts in
    strings, comments, ties = [], [], []
    for n, bs, bl, _ in sel:
        be = bs + bl
        toks, names = [], []
        for s, e, ps in steps:
            if not (bs <= s < be):
                continue
            w = (e - s) // unit
            if ps is None:
                tok = "~"
            elif len(ps) == 1:
                tok = tidal_note(ps[0])
                names.append(note_name(ps[0]))
            else:
                tok = "[" + ",".join(tidal_note(p) for p in ps) + "]"
                names.append("[" + " ".join(note_name(p) for p in ps) + "]")
            if w != 1:
                tok += f"@{w}"
            toks.append(tok)
            if ps is not None and e > be:
                ties.append((n, ps, e))
        strings.append(" ".join(toks))
        comments.append(f"b.{n}" + (": " + " ".join(names) if names else ""))
    return strings, beats, comments, ties


# --------------------------------------------------------------------------- main
def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("midi")
    ap.add_argument("--blocks", required=True, help="bar ranges, e.g. 1-16,17-46,47-62")
    ap.add_argument("--instrument", default="harp'", help="primed instrument name (default harp')")
    ap.add_argument("--mark", default="p", help="dynamic mark to write on every line (default p)")
    ap.add_argument("--indent", type=int, default=2)
    a = ap.parse_args()

    data = open(a.midi, "rb").read()
    fmt, division, tracks = parse_smf(data)
    tempos, timesigs, names = meta_of(tracks)
    all_notes = [(ti, notes_of(ev)) for ti, ev in enumerate(tracks)]
    all_notes = [(ti, ns) for ti, ns in all_notes if ns]
    sounding = set()
    for i, (ti, ns) in enumerate(all_notes):
        kept = []
        for n in ns:
            if (n[0], n[2]) in sounding:
                print(f"-- dropped track {ti} {note_name(n[2])} at tick {n[0]}: another track sounds it", file=sys.stderr)
            else:
                kept.append(n)
        sounding.update((n[0], n[2]) for n in kept)
        all_notes[i] = (ti, kept)
    last_tick = max(e for _, ns in all_notes for _, e, *_ in ns)
    bars = bar_table(timesigs, division, last_tick)
    nbars = max(b[0] for b in bars if b[1] < last_tick)

    print(f"-- {a.midi}: format {fmt}, division {division}, {len(tracks)} tracks, {nbars} bars", file=sys.stderr)
    print(f"-- time signatures: {[(t, f'{n}/{d}') for t, n, d in timesigs]}", file=sys.stderr)
    print(f"-- tempos (tick, bpm): {[(t, round(b, 1)) for t, b in tempos]}", file=sys.stderr)
    lo = min(p for _, ns in all_notes for _, _, p, *_ in ns)
    hi = max(p for _, ns in all_notes for _, _, p, *_ in ns)
    print(f"-- range: {note_name(lo)} ({lo}) .. {note_name(hi)} ({hi})", file=sys.stderr)
    for ti, nm in names:
        print(f"-- track {ti}: {nm!r}", file=sys.stderr)

    ind = " " * a.indent
    for rng in a.blocks.split(","):
        b1, b2 = (int(x) for x in rng.split("-"))
        print(f"{ind},bars {b1} {b2} $ stack [silence")
        for ti, ns in all_notes:
            for li, layer in enumerate(layers_of(ns)):
                enc = encode_block(layer, bars, b1, b2)
                if enc is None:
                    continue  # silent layer in this block
                strings, beats, comments, ties = enc
                print(f"{ind}   ,{a.instrument} M 1 \"{a.mark}\" k (staff {beats}   -- track {ti} layer {li}")
                for s, c in zip(strings, comments):
                    print(f"{ind}      [\"{s}\"" if s is strings[0] else f"{ind}      ,\"{s}\"", end="")
                    print(f"{' ' * max(1, 40 - len(s))}-- {c}")
                print(f"{ind}      ])")
                for n, ps, e in ties:
                    print(f"-- tie: bar {n} {' '.join(note_name(p) for p in ps)} held into the next bar", file=sys.stderr)
        print(f"{ind}   ]")


if __name__ == "__main__":
    main()
