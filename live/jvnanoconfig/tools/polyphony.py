#!/usr/bin/env python3
"""How many JV-1010 voices a piece uses, moment by moment.

usage: python3 live/jvnanoconfig/tools/polyphony.py PIECE.tidal [PIECE.tidal ...]
       python3 live/jvnanoconfig/tools/polyphony.py --measure     (with the JV connected: tones and lifetimes)
       python3 live/jvnanoconfig/tools/polyphony.py --tones       (tones only, after a patch edit)

A piece is rendered through the boot headless on the JV rig (`p` shadowed, so
nothing is sent): its own definitions are loaded, the blocks of its launcher
are queried over the whole score, and every note event (start, length,
channel, pitch, velocity) is collected with the tempo track. Each note is then
charged the voices it takes on the module:

  voices   the tones of the part's patch that answer the note's key and the
           velocity the JV receives (the X3 maps 1-127 to 42-127), read from
           the module into captures/voice_tones.json by --measure
  lifetime from the onset until the tone dies: a sustained tone lives until
           note-off plus its release; a tone that decays while held dies on
           its own. Both are measured on the module by --measure (time to
           50 dB under the note's peak, effect sends off) into
           captures/voice_lifetimes.json

The report gives each piece's peak voice count, where it occurs and which
parts make it, against the module's 64 and the Performance's per-part voice
reserve. Channel 10 is the MPC's lane and does not reach the JV; the continuo
part is taken to play the voice the piece selects (the harp when it selects
none).
"""
import json
import re
import subprocess
import sys
import tempfile
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
HERE = Path(__file__).resolve().parents[1]
CAPTURES = HERE / "captures"
TONES = CAPTURES / "voice_tones.json"
LIFETIMES = CAPTURES / "voice_lifetimes.json"
BOOT = "live/BootTidal.hs"
DONE = "__RENDER_DONE__"
DECL = re.compile(r"^[a-z_][\w'.]*[^=\n]*=(?!=)", re.M)
IO_ITEMS = re.compile(r"^(hush|resetCycles|setbpm|display|setContinuo|once|setCC|allNotesOff)\b")
JV_VELOCITY = lambda v: 42 + round(v * 85 / 127)
VOICES = 64
CONTINUO = {"harpV": 7, "harpsichordV": 10, "marimbaV": 17}     # continuo voice -> User Patch


def blocks(text):
    for raw in text.split("\n\n"):
        block = "\n".join(ln for ln in raw.splitlines() if not ln.lstrip().startswith("--")).strip("\n")
        if block.strip():
            yield block


def launcher(text):
    """(let bindings, pattern items, continuo voice) of the piece's `do` launcher."""
    body = text[text.rindex("\ndo\n"):]
    binds = re.findall(r"^\s+(?:let\s+)?([fdk])\s*=\s*(.+?)\s*(?:--.*)?$", body, re.M)
    items = body[body.index("mapM_ id [") + len("mapM_ id ["):]
    depth, cur, out = 0, "", []
    for ch in items:
        if ch in "([":
            depth += 1
        elif ch in ")]":
            if depth == 0:
                break
            depth -= 1
        if ch == "," and depth == 0:
            out.append(cur); cur = ""
        else:
            cur += ch
    out.append(cur)
    out = [re.sub(r"--.*", "", i).strip() for i in out]
    voice = next((m.group(1) for i in out for m in [re.match(r"setContinuo\s+(\w+)", i)] if m), "harpV")
    return binds, [i for i in out if i and not IO_ITEMS.match(i)], voice


def last_cycle(text):
    bars = [float(b) for b in re.findall(r"\bbars\s+[\d.]+\s+\(?([\d.]+)", text)]
    pre = re.search(r"^pre\s*=\s*([\d.]+)", text, re.M)
    return int((max(bars) + (float(pre.group(1)) if pre else 0) + 2) * 4)


def render(piece):
    text = piece.read_text()
    decls = []
    for b in blocks(text):
        head = b.splitlines()[0]
        if head.lstrip().startswith(("do", ":", "hush", "import")):
            continue
        if DECL.search(b):
            decls.extend([":{", b, ":}"])
    binds, items, voice = launcher(text)
    boot = (ROOT / BOOT).read_text().splitlines()
    i = boot.index(":localRig")
    j = boot.index(":{", i); k = boot.index(":}", j)
    end = last_cycle(text)
    out = "putStrLn (\"{0}|\" ++ show (fromRational (start (wholeOrPart e)) :: Double) ++ \"|\" ++ show (fromRational (stop (wholeOrPart e)) :: Double) ++ \"|\" ++ show (value e))"
    session = "\n".join(
        [f":script {BOOT}", "rig = jv1010"] + boot[j:k + 1] + ["p _ pat = pat"] + decls
        + [f"{n} = {v}" for n, v in binds]
        + ["piecePat = stack [" + ", ".join(items) + "]",
           "mapM_ (\\e -> %s) (filter eventHasOnset (queryArc piecePat (Arc 0 %d)))" % (out.format("EV"), end),
           "mapM_ (\\e -> %s) (filter eventHasOnset (queryArc (segment 4 $ filterValues (Map.member \"cps\") piecePat) (Arc 0 %d)))" % (out.format("CPS"), end),
           f'putStrLn "{DONE}"', ":quit", ""])
    with tempfile.TemporaryDirectory() as tmp:
        s = Path(tmp) / "session.ghci"; s.write_text(session)
        proc = subprocess.run(
            ["stack", "ghci", "--ghci-options=-v0 -Wno-unused-matches -Wno-unused-local-binds "
             "-Wno-unused-do-bind -Wno-name-shadowing -Wno-missing-signatures -Wno-type-defaults"],
            cwd=ROOT, stdin=s.open(), capture_output=True, text=True)
    o = proc.stdout + proc.stderr
    errs = [l for l in o.splitlines() if "error" in l.lower()]
    if DONE not in o or errs:
        print("\n".join(o.splitlines()[-40:]), file=sys.stderr)
        sys.exit(f"{piece}: render failed")
    notes, cps = [], []
    for line in o.splitlines():
        kind, _, rest = line.partition("|")
        if kind not in ("EV", "CPS"):
            continue
        t0, t1, vm = rest.split("|", 2)
        if not re.fullmatch(r"[-0-9.e]+", t0) or not re.fullmatch(r"[-0-9.e]+", t1):
            continue                                           # a line ghci interleaved with its own output
        num = lambda key: (lambda m: float(m.group(1)) if m else None)(re.search(key + r": ([-0-9.e]+)", vm))
        if kind == "CPS":
            cps.append((float(t0), num("cps")))
            continue
        n, c = num("note"), num("midichan")
        if n is None or c is None:
            continue
        notes.append({"t": float(t0), "d": float(t1) - float(t0), "midi": int(round(n)) + 60, "ch": int(round(c)) + 1,
                      "vel": int((num("amp") if num("amp") is not None else 0.4) * 127),
                      "sustain": num("sustain"), "legato": num("legato")})
    return notes, sorted(cps), voice


def clock(cps, tempo):
    """cycle -> seconds, integrating the piece's tempo track (cycles per second)."""
    pts = sorted(set(cps)) or [(0.0, tempo / 60)]
    if pts[0][0] > 0:
        pts.insert(0, (0.0, pts[0][1]))
    acc, table = 0.0, []
    for (c0, r0), (c1, _) in zip(pts, pts[1:] + [(1e9, None)]):
        table.append((c0, acc, r0)); acc += (c1 - c0) / r0
    def seconds(c):
        c0, a, r = max((row for row in table if row[0] <= c), key=lambda row: row[0])
        return a + (c - c0) / r
    return seconds


def part_of(ch, midi):
    if ch == 10:
        return None                                            # the MPC's lane: filtered from the JV by the U6
    if ch == 9:
        return "10" if midi >= 48 else "9"                     # channel 9 is split by key at C3
    return str(ch)


def tones_for(table, part, midi, vel, voice):
    """(lifetime table, entry) for each voice the note takes."""
    if part == "10":
        return [("rhythm", str(midi))] if str(midi) in table["rhythm"] else []
    patch = table["continuo"][voice] if part == "7" else table["parts"][part]
    v = JV_VELOCITY(vel)
    owner = ("continuo", voice) if part == "7" else ("parts", part)
    return [owner for t, (kl, ku, vl, vu) in patch["tones"].items()
            if kl <= midi <= ku and (not patch["velocity_switch"] or vl <= v <= vu)]


def rendered(piece):
    """The render, cached in captures/ against the piece's modification time."""
    cache = CAPTURES / ("render_" + re.sub(r"[^\w]+", "_", str(piece.relative_to(ROOT))) + ".json")
    if cache.exists() and json.loads(cache.read_text())["mtime"] == piece.stat().st_mtime:
        r = json.loads(cache.read_text()); return r["notes"], [tuple(c) for c in r["cps"]], r["voice"]
    notes, cps, voice = render(piece)
    cache.write_text(json.dumps({"mtime": piece.stat().st_mtime, "notes": notes, "cps": cps, "voice": voice}))
    return notes, cps, voice


def analyse(piece, table, life):
    notes, cps, voice = rendered(piece)
    tempo = eval(re.search(r"^tempo\s*=\s*([\d.*/ ]+)", piece.read_text(), re.M).group(1))   # e.g. `66 * 4 / 6`
    sec = clock(cps, tempo)
    spans = []                                                 # (start, end, part, voices)
    for e in notes:
        part = part_of(e["ch"], e["midi"])
        if part is None:
            continue
        t0 = sec(e["t"])
        hold = e["sustain"] if e["sustain"] is not None else (sec(e["t"] + e["d"]) - t0) * (e["legato"] or 1)
        for group, entry in tones_for(table, part, e["midi"], e["vel"], voice):
            decay, release = life[group][entry]
            spans.append((t0, t0 + min(decay, hold + release), part))
    marks = sorted([(s, 1, p) for s, _, p in spans] + [(e, -1, p) for _, e, p in spans])
    now, by_part, peak, at, mix = 0, defaultdict(int), 0, 0.0, {}
    over = {48: 0.0, 56: 0.0, 64: 0.0}                         # seconds spent above each count
    last = marks[0][0] if marks else 0.0
    for t, step, part in marks:
        for n in over:
            if now > n:
                over[n] += t - last
        last = t
        now += step; by_part[part] += step
        if now > peak:
            peak, at, mix = now, t, {q: n for q, n in by_part.items() if n}
    return {"piece": str(piece.relative_to(ROOT)), "notes": len(notes), "voices": len(spans), "peak": peak,
            "at_s": round(at, 1), "mix": dict(sorted(mix.items(), key=lambda kv: -kv[1])), "continuo": voice,
            "seconds_over": {n: round(v, 1) for n, v in over.items()}, "length_s": round(marks[-1][0], 1) if marks else 0}


MEASURE_NOTE = {"1": 72, "2": 69, "3": 62, "4": 50, "5": 60, "6": 52, "7": 60, "8": 45, "9": 36, "11": 31,
                "12": 55, "13": 60, "14": 55, "15": 64, "16": 55}
LONG_S, SHORT_S = 4.0, 0.15


def read_tones():
    """Every part's sounding tones, the continuo voices' and the per-part voice reserve; writes TONES."""
    import jv_sysex as J

    def tones(read):
        common = read(0, J.PATCH_COMMON_SIZE)
        out = {}
        for t in range(1, 5):
            r = read(0x10 + 2 * (t - 1), J.TONE_SIZE)
            if r[0]:
                out[str(t)] = (r[0x0E], r[0x0F], r[0x0C], r[0x0D])
        return {"velocity_switch": common[0x40], "tones": out}

    table = {"parts": {str(p): tones(lambda third, size, p=p: J.rq1(J.patch_addr(p)[:2] + [third, 0], size))
                       for p in J.PATCH_PARTS},
             "continuo": {name: tones(lambda third, size, n=n: J.rq1([0x11, n - 1, third, 0], size))
                          for name, n in CONTINUO.items()},
             "rhythm": [str(k) for k in J.RHYTHM_KEYS],
             "reserve": list(J.rq1([0x01, 0, 0, 0x30], 16))}
    TONES.write_text(json.dumps(table, indent=1))
    return table


def measure_voices():
    """Read the tones and time each voice's life on the module; writes TONES and LIFETIMES."""
    import numpy as np
    import jv_sysex as J
    import measure as M
    import round as R

    table = read_tones()

    def life(channel, note):
        """(decay, release) in seconds: how long the voice lives while held, and after note-off."""
        ev = [(0.0, channel, note, 81, LONG_S), (LONG_S + 9.0, channel, note, 81, SHORT_S),
              (LONG_S + 18.0, channel, note, 1, 0.01)]                     # a last, inaudible event keeps the recorder running
        R.probe(ev)
        x = M.read_wav(CAPTURES / "probe.wav", R.JV_INPUTS)
        onset = M.find_marker((x ** 2).mean(1)); t0 = ev[0][0] - R.SYNC_LEAD_S
        def frames(a, b):
            power = (x[a:min(b, len(x))] ** 2).mean(1)
            return 10 * np.log10(np.maximum(power[: len(power) // 480 * 480].reshape(-1, 480).mean(1), 1e-14))
        floor = np.percentile(frames(0, len(x)), 10)          # the file is mostly silence
        ends = []
        for t, _, _, _, hold in ev[:2]:
            a = int((onset + t - t0) * 48000)
            db = np.convolve(frames(a, a + int(8.8 * 48000)), np.ones(5) / 5, "same")   # 50 ms smoothing
            quiet = db < max(db.max() - 50, floor + 10)
            # the voice ends where the level falls under the threshold and stays there for 0.3 s
            end = next((i for i in range(int(db.argmax()), len(db) - 30) if quiet[i:i + 30].all()), len(db))
            ends.append(end / 100)
        long_end, short_end = ends
        if long_end < LONG_S - 0.05:
            return long_end, max(short_end - SHORT_S, 0.0)
        return 1e9, max(long_end - LONG_S, 0.0)

    sends = {p: list(J.rq1(J.part_addr(p, 0x0C), 2)) for p in range(1, 17)}
    lifetimes = {"parts": {}, "continuo": {}, "rhythm": {}}
    try:
        for p in range(1, 17):
            J.dt1(J.part_addr(p, 0x0C), [0, 0])
        for part, note in MEASURE_NOTE.items():
            lifetimes["parts"][part] = life(int(part), note)
            print("part", part, lifetimes["parts"][part], flush=True)
        for name, n in CONTINUO.items():
            R.program(7, 80, 0, n - 1)
            lifetimes["continuo"][name] = life(7, 60)
            print("continuo", name, lifetimes["continuo"][name], flush=True)
        R.program(7, 80, 0, CONTINUO["harpV"] - 1)
        for key in table["rhythm"]:
            lifetimes["rhythm"][key] = life(9, int(key))
            print("rhythm key", key, lifetimes["rhythm"][key], flush=True)
    finally:
        for p, v in sends.items():
            J.dt1(J.part_addr(p, 0x0C), v)
    LIFETIMES.write_text(json.dumps(lifetimes, indent=1))


def main():
    sys.path.insert(0, str(Path(__file__).parent))
    if "--measure" in sys.argv:
        return measure_voices()
    if "--tones" in sys.argv:
        return read_tones()
    table, life = json.loads(TONES.read_text()), json.loads(LIFETIMES.read_text())
    for path in sys.argv[1:]:
        r = analyse(Path(path).resolve(), table, life)
        print(json.dumps(r))


if __name__ == "__main__":
    main()
