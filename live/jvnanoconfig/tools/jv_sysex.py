#!/usr/bin/env python3
"""Read and write JV-1010 parameters over the U6MIDI's USB port.

usage: python3 live/jvnanoconfig/tools/jv_sysex.py state [--stored | --from backup.json]  > state.json
       python3 live/jvnanoconfig/tools/jv_sysex.py apply jv_edits.json [--stored]
       python3 live/jvnanoconfig/tools/jv_sysex.py backup [USER PATCH ...] > backup.json
       python3 live/jvnanoconfig/tools/jv_sysex.py restore backup.json
       python3 live/jvnanoconfig/tools/jv_sysex.py sheet before.json after.json
       python3 live/jvnanoconfig/tools/jv_sysex.py read  ADDR SIZE
       python3 live/jvnanoconfig/tools/jv_sysex.py write ADDR BYTE [BYTE ...]

The JV answers on `U6MIDI Pro Port 2`: the U6 routes USB 2 to the JV's MIDI IN
and the JV's MIDI OUT back to USB 2. SysEx does not pass the X3, so this port
is the only two-way path; notes stay on the U2MIDI. The port is opened raw, so
nothing else may hold it: SuperCollider connects to every MIDI port and must
be closed first.

The module keeps two copies of the selected Performance. The temporary one is
what sounds; it is loaded from the stored one (a User Performance, the User
Patches its parts play and the User Rhythm Set of its rhythm part) whenever
the Performance is selected, and is lost at power-off. Without --stored every
command works on the temporary copy.

  state   one copy decoded: each part's routing, level and pan; each patch's
          level and, per sounding tone, what sets its loudness, its response
          to velocity and its place in the stereo field; the rhythm part and
          the rhythm keys the rig plays. --from decodes a backup file instead
          of the module.
  apply   send an edits file, values as `state` prints them:
          {"key_range_switch": 0 or 1,                     whether the parts' key ranges apply
           "parts":   {PART: {PARAM: value}},              part 10 is the rhythm part
           "patches": {PART: {"board": n,                   the user patch becomes Orchestral board patch n, first
                              "level": n,
                              "copy": {TONE: SOURCE TONE},  the whole tone, switch included
                              "tones": {TONE or "all": {"block": hex, PARAM: value}}}},  "block" is a whole tone, written first
           "rhythm":  {KEY: {"block": hex, PARAM: value}},  "block" is a whole rhythm key, written first
           "continuo": {USER PATCH: {"board": n, "level": n, "off": [TONE], "tones": {...}}}}
                                                            a continuo voice: a User Patch outside the Performance,
                                                            selected on the continuo part by Program Change;
                                                            written with --stored only
          Copies are made first, then "all" means every tone that is switched
          on. With --stored the values go into user memory and are permanent:
          take a `backup` first.
  backup  every byte of the stored copy.
  restore write a backup back, block by block where it differs, and read each
          block back to confirm it.
  sheet   two `state` files as a Markdown table of what changed, from -> to.
  read / write   raw access to the temporary copy. ADDR is the four-byte
          Roland address as hex, SIZE a byte count, BYTEs hex.

Every write reads the address first and appends old and new values to
captures/jv_sysex.log. The only addresses ever written are the temporary
Performance, its patches and rhythm set (01 00 00 00 to 02 0F 7F 7F) and, with
--stored, the stored copy of exactly those.

Offsets and block sizes are the owner's manual's Parameter Address Map
(pp. 74-80).
Importable: rq1, dt1, state, apply, backup, restore, part_addr, patch_addr,
tone_addr, rhythm_addr.
"""
import functools
import json
import subprocess
import sys
import time
from pathlib import Path

PORT_NAME = "U6MIDI Pro Port 2"
MODEL, DEVICE = 0x6A, 0x10
LOG = Path(__file__).resolve().parents[1] / "captures" / "jv_sysex.log"


# ── transport ────────────────────────────────────────────

def port():
    for line in subprocess.run(["amidi", "-l"], capture_output=True, text=True).stdout.splitlines():
        if PORT_NAME in line:
            return line.split()[1]
    sys.exit(f"no '{PORT_NAME}': plug the U6MIDI's USB port into this machine")


def checksum(body):
    return (128 - sum(body) % 128) % 128


def message(command, addr, payload):
    body = list(addr) + list(payload)
    return [0xF0, 0x41, DEVICE, MODEL, command] + body + [checksum(body), 0xF7]


def amidi(msg, listen=0.0):
    cmd = ["amidi", "-p", port(), "-S", " ".join(f"{b:02X}" for b in msg)]
    if listen:
        cmd += ["-d", "-t", str(listen)]
    out = subprocess.run(cmd, capture_output=True, text=True)
    if out.returncode:
        busy = "busy" in out.stderr
        sys.exit(f"{PORT_NAME} is held by another program; close SuperCollider (it connects to every MIDI port)"
                 if busy else out.stderr.strip())
    return [int(b, 16) for b in out.stdout.split()]


def rq1(addr, size):
    """Data at addr, `size` bytes, gathered across however many DT1s the JV splits it into."""
    reply = amidi(message(0x11, addr, [(size >> 21) & 0x7F, (size >> 14) & 0x7F, (size >> 7) & 0x7F, size & 0x7F]),
                  listen=0.4 + size / 400)
    data, i = [], 0
    while i < len(reply):
        j = reply.index(0xF7, i)
        pkt = reply[i:j + 1]; i = j + 1
        if pkt[:5] == [0xF0, 0x41, DEVICE, MODEL, 0x12]:
            if checksum(pkt[5:-2]) != pkt[-2]:
                sys.exit(f"checksum mismatch in the reply for {fmt(addr)}")
            data += pkt[9:-2]
    if len(data) != size:
        sys.exit(f"{fmt(addr)}: asked for {size} bytes, the JV returned {len(data)}")
    return bytes(data)


def dt1(addr, data, stored=False):
    temporary = [0x01, 0, 0, 0] <= list(addr) <= [0x02, 0x0F, 0x7F, 0x7F]
    declared = tuple(addr[:2]) in {(0x11, n - 1) for n in continuo_slots}
    if not (temporary or stored and (declared or tuple(addr[:2]) in stored_blocks())):
        sys.exit(f"{fmt(addr)} is neither the temporary Performance, its stored copy nor a declared continuo voice; not written")
    old = rq1(addr, len(data))
    amidi(message(0x12, addr, data))
    time.sleep(0.05)
    LOG.parent.mkdir(exist_ok=True)
    with LOG.open("a") as fh:
        fh.write(f"{time.strftime('%Y-%m-%d %H:%M:%S')} {fmt(addr)} old {fmt(old)} new {fmt(data)}\n")
    return old


def write_block(addr, data, stored=False):
    """A whole block, in packets of at most 128 bytes; an address byte holds 7 bits, so 128 on is the next third byte."""
    for i in range(0, len(data), 128):
        dt1(addr[:2] + [addr[2] + i // 128, 0], list(data[i:i + 128]), stored)


def fmt(bs):
    return " ".join(f"{b:02X}" for b in bs)


def parse(hexes):
    s = "".join(hexes).replace(" ", "")
    return [int(s[i:i + 2], 16) for i in range(0, len(s), 2)]


# ── parameters: name -> (offset, (decode, encode)), values as the module displays them ──

STEP15 = [-100, -70, -50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50, 70, 100]
plain = (lambda b: b, lambda v: v)
centred = (lambda b: b - 64, lambda v: v + 64)             # pan: L64 … 0 … 63R
sens = (lambda b: 2 * b - 100, lambda v: (v + 100) // 2)   # -100 … +150 in steps of 2
table = lambda xs: (lambda b: xs[b], lambda v: xs.index(v))
GAIN = table([-6, 0, 6, 12])

PART = {
    "receive": (0x00, plain), "channel": (0x01, (lambda b: b + 1, lambda v: v - 1)),
    "level": (0x06, plain), "pan": (0x07, centred),
    "mix_send": (0x0B, plain), "chorus_send": (0x0C, plain), "reverb_send": (0x0D, plain),
    "receive_program": (0x0E, plain),                     # off, the part ignores Program Change and keeps its patch
    "key_lower": (0x11, plain), "key_upper": (0x12, plain),
}
KEY_RANGE_SWITCH = 0x2F                                  # in the Performance's common block: off, every part's key range is ignored
TONE = {
    "wave_gain": (0x05, GAIN),
    "velocity_lower": (0x0C, plain), "velocity_upper": (0x0D, plain),
    "key_lower": (0x0E, plain), "key_upper": (0x0F, plain),
    "coarse_tune": (0x3D, (lambda b: b - 48, lambda v: v + 48)),   # semitones: the tone sounds this far from the note sent
    "cutoff": (0x51, plain),
    "filter_velocity_curve": (0x56, (lambda b: b + 1, lambda v: v - 1)),
    "filter_velocity_sens": (0x57, sens),
    "level": (0x65, plain),
    "bias_direction": (0x66, table(["LOWER", "UPPER", "LOWER&UPPER", "ALL"])),
    "bias_position": (0x67, plain),
    "bias_level": (0x68, table(STEP15)),
    "velocity_curve": (0x69, (lambda b: b + 1, lambda v: v - 1)),
    "velocity_sens": (0x6A, sens),
    "pan": (0x77, centred),
    "pan_keyfollow": (0x78, table(STEP15)),
}
RHYTHM = {
    "wave_gain": (0x05, GAIN), "sustain": (0x08, plain), "tune": (0x0C, plain),
    "level": (0x29, plain), "velocity_sens": (0x2A, sens), "pan": (0x33, centred),
}
PATCH_LEVEL = 0x2E
PATCH_REF = slice(2, 6)                                  # in a part: sound group type, group id, number (two nibbles)
USER_GROUP = (0, 1)
PERF_COMMON_SIZE, PART_SIZE, PATCH_COMMON_SIZE, TONE_SIZE, RHYTHM_KEY_SIZE = 0x42, 0x19, 0x4A, 0x81, 0x3A
RHYTHM_PART = 10
CONTINUO_PART = 7                                        # its channel takes Program Changes: the continuo voices are User Patches
continuo_slots = set()                                   # User Patch numbers (1-128) an edits file declares as continuo voices
PATCH_PARTS = [p for p in range(1, 17) if p != RHYTHM_PART]
RHYTHM_KEYS = (54, 57, 59, 60, 61, 62, 63, 64, 76, 81, 84)   # the keys the rig's unpitched voices play
ALL_RHYTHM_KEYS = range(35, 99)


def wave(raw):
    return f"{['INT', 'PCM', 'EXP'][raw[1]]}{raw[2]}:{raw[3] * 16 + raw[4] + 1}"


# ── the two copies' addresses ────────────────────────────

def user_number(part):
    """The User Patch (for part 10, User Rhythm Set) number the temporary Performance's part plays."""
    kind, group, hi, lo = rq1([0x01, 0, 0x10 + part - 1, PATCH_REF.start], 4)
    if (kind, group) != USER_GROUP:
        sys.exit(f"part {part} plays a preset or card sound, which has no stored copy to write")
    return hi * 16 + lo


@functools.lru_cache(None)
def stored_performance():
    """The User Performance (0-31) the temporary one was loaded from: same name, same sound on every part."""
    name = rq1([0x01, 0, 0, 0], 12)
    refs = [rq1([0x01, 0, 0x10 + p, PATCH_REF.start], 4) for p in range(16)]
    found = [n for n in range(32) if rq1([0x10, n, 0, 0], 12) == name
             and [rq1([0x10, n, 0x10 + p, PATCH_REF.start], 4) for p in range(16)] == refs]
    if len(found) != 1:
        sys.exit(f"{len(found)} User Performances match the selected one ({bytes(name).decode().strip()}); "
                 "select the Performance to work on")
    return found[0]


@functools.lru_cache(None)
def stored_prefix(part):
    return [0x10, 0x40 + user_number(part)] if part == RHYTHM_PART else [0x11, user_number(part)]


def stored_blocks():
    return {(0x10, stored_performance())} | {tuple(stored_prefix(p)) for p in range(1, 17)}


def part_addr(part, offset=0, stored=False):
    return ([0x10, stored_performance()] if stored else [0x01, 0]) + [0x10 + part - 1, offset]


def patch_addr(part, offset=0, stored=False):
    return (stored_prefix(part) if stored else [0x02, part - 1]) + [0, offset]


def tone_addr(part, tone, offset=0, stored=False):
    return patch_addr(part, 0, stored)[:2] + [0x10 + 2 * (tone - 1), offset]


def rhythm_addr(key, offset=0, stored=False):
    return patch_addr(RHYTHM_PART, 0, stored)[:2] + [key, offset]


# ── decoded state ────────────────────────────────────────

def decode(table_, raw):
    return {name: dec(raw[off]) for name, (off, (dec, _)) in table_.items()}


def state(stored=False, saved=None):
    """One copy decoded: the module's temporary or stored one, or a backup file's."""
    if saved:
        blocks = {tuple(parse([a])): parse([d]) for a, d in saved["blocks"].items()}
        perf = next(list(a[:2]) for a in blocks if a[0] == 0x10 and a[1] < 0x40)
        read = lambda addr, size: bytes(blocks[tuple(addr[:3]) + (0,)][addr[3]:addr[3] + size])
        part_at = lambda part: perf + [0x10 + part - 1, 0]

        def sound_at(part, third=0):
            ref = blocks[tuple(part_at(part))]
            number = ref[4] * 16 + ref[5]
            return ([0x10, 0x40 + number] if part == RHYTHM_PART else [0x11, number]) + [third, 0]
    else:
        read = rq1
        perf = part_addr(1, 0, stored)[:2]
        part_at = lambda part: part_addr(part, 0, stored)
        sound_at = lambda part, third=0: patch_addr(part, 0, stored)[:2] + [third, 0]
    out = {"performance": bytes(read(perf + [0, 0], 12)).decode().strip(),
           "key_range_switch": read(perf + [0, KEY_RANGE_SWITCH], 1)[0], "parts": {}}
    for part in PATCH_PARTS:
        common = read(sound_at(part), PATCH_COMMON_SIZE)
        tones = {}
        for t in range(1, 5):
            raw = read(sound_at(part, 0x10 + 2 * (t - 1)), TONE_SIZE)
            if raw[0]:
                tones[str(t)] = decode(TONE, raw) | {"wave": wave(raw)}
        out["parts"][str(part)] = decode(PART, read(part_at(part), PART_SIZE)) | {
            "patch": bytes(common[:12]).decode().strip(), "patch_level": common[PATCH_LEVEL], "tones": tones}
    out["rhythm_part"] = decode(PART, read(part_at(RHYTHM_PART), PART_SIZE)) | {
        "set": bytes(read(sound_at(RHYTHM_PART), 12)).decode().strip()}
    out["rhythm"] = {}
    for key in RHYTHM_KEYS:
        raw = read(sound_at(RHYTHM_PART, key), RHYTHM_KEY_SIZE)
        out["rhythm"][str(key)] = decode(RHYTHM, raw) | {"wave": wave(raw)}
    return out


def part_levels():
    return {part: rq1(part_addr(part, PART["level"][0]), 1)[0] for part in PATCH_PARTS}


# ── writing ──────────────────────────────────────────────

def apply(edits, stored=False):
    put = lambda addr, table_, name, v: dt1(addr[:3] + [table_[name][0]], [table_[name][1][1](v)], stored)
    if "key_range_switch" in edits:
        dt1(part_addr(1, 0, stored)[:2] + [0, KEY_RANGE_SWITCH], [edits["key_range_switch"]], stored)
    for part, params in edits.get("parts", {}).items():
        for name, v in params.items():
            put(part_addr(int(part), 0, stored), PART, name, v)
    for part, e in edits.get("patches", {}).items():
        part = int(part)
        if "board" in e:
            board_patch(part, e["board"], stored)
        if "level" in e:
            dt1(patch_addr(part, PATCH_LEVEL, stored), [e["level"]], stored)
        for dest, source in e.get("copy", {}).items():
            write_block(tone_addr(part, int(dest), 0, stored), rq1(tone_addr(part, int(source), 0, stored), TONE_SIZE), stored)
        on = [t for t in range(1, 5) if rq1(tone_addr(part, t, 0, stored), 1)[0]]
        for key, params in e.get("tones", {}).items():
            for t in (on if key == "all" else [int(key)]):
                if "block" in params:
                    write_block(tone_addr(part, t, 0, stored), parse([params["block"]]), stored)
                for name, v in params.items():
                    if name != "block":
                        put(tone_addr(part, t, 0, stored), TONE, name, v)
    for slot, e in edits.get("continuo", {}).items():
        if stored:
            continuo_voice(int(slot), e)
    for key, params in edits.get("rhythm", {}).items():
        if "block" in params:
            write_block(rhythm_addr(int(key), 0, stored), parse([params["block"]]), stored)
        for name, v in params.items():
            if name != "block":
                put(rhythm_addr(int(key), 0, stored), RHYTHM, name, v)


def board_patch(part, number, stored=False):
    """Make the part's user patch a copy of Orchestral board patch `number` (1-255, the board's patch list).

    A preset is readable only as a temporary patch, so the part plays it for a moment. Putting the part's
    own patch reference back reloads the temporary patch from the stored one, so a stored write puts the
    temporary patch back as it found it: either copy is written without disturbing the other."""
    blocks = [(patch_addr, (part,), PATCH_COMMON_SIZE)] + [(tone_addr, (part, t), TONE_SIZE) for t in range(1, 5)]
    read = lambda: [rq1(at(*args), size) for at, args, size in blocks]
    ref = part_addr(part, PATCH_REF.start)
    own, temporary = list(rq1(ref, 4)), read()
    dt1(ref, [2, 2, (number - 1) >> 4, (number - 1) & 15]); time.sleep(0.4)
    preset = read()
    dt1(ref, own); time.sleep(0.4)
    for (at, args, _), was, new in zip(blocks, temporary, preset):
        if stored:
            write_block(at(*args), was)
        write_block(at(*args, 0, stored), new, stored)


def continuo_voice(slot, e):
    """Build a continuo voice in User Patch `slot`: {"board": n, "level": patch level, "off": [tones],
    "tones": {TONE or "all": {PARAM: value}}}.

    The voice is assembled where it can be heard, in the continuo part's temporary patch, and copied to the
    slot from there; the part is then given its own patch back. It exists only in user memory, so it is
    written by a stored apply alone, and the slot is whatever the edits file names."""
    continuo_slots.add(slot)
    part = CONTINUO_PART
    ref = part_addr(part, PATCH_REF.start)
    own = list(rq1(ref, 4))
    dt1(ref, [2, 2, (e["board"] - 1) >> 4, (e["board"] - 1) & 15]); time.sleep(0.4)
    for t in e.get("off", []):
        dt1(tone_addr(part, t), [0])
    if "level" in e:
        dt1(patch_addr(part, PATCH_LEVEL), [e["level"]])
    on = [t for t in range(1, 5) if rq1(tone_addr(part, t), 1)[0]]
    for key, params in e.get("tones", {}).items():
        for t in (on if key == "all" else [int(key)]):
            for name, v in params.items():
                dt1(tone_addr(part, t)[:3] + [TONE[name][0]], [TONE[name][1][1](v)])
    write_block([0x11, slot - 1, 0, 0], rq1(patch_addr(part), PATCH_COMMON_SIZE), True)
    for t in range(1, 5):
        write_block([0x11, slot - 1, 0x10 + 2 * (t - 1), 0], rq1(tone_addr(part, t), TONE_SIZE), True)
    dt1(ref, own); time.sleep(0.4)


def backup(slots=()):
    """{"performance": name, "blocks": {address: bytes}} of the whole stored copy, as hex; `slots` adds those User Patches."""
    perf = [0x10, stored_performance()]
    blocks = [(perf + [0, 0], PERF_COMMON_SIZE)] + [(perf + [0x10 + p, 0], PART_SIZE) for p in range(16)]
    for part in PATCH_PARTS:
        blocks.append((patch_addr(part, 0, True), PATCH_COMMON_SIZE))
        blocks += [(tone_addr(part, t, 0, True), TONE_SIZE) for t in range(1, 5)]
    blocks.append((rhythm_addr(0, 0, True), 12))
    blocks += [(rhythm_addr(key, 0, True), RHYTHM_KEY_SIZE) for key in ALL_RHYTHM_KEYS]
    for n in slots:
        blocks.append(([0x11, n - 1, 0, 0], PATCH_COMMON_SIZE))
        blocks += [([0x11, n - 1, 0x10 + 2 * t, 0], TONE_SIZE) for t in range(4)]
    return {"performance": bytes(rq1(perf + [0, 0], 12)).decode().strip(),
            "blocks": {fmt(addr): fmt(rq1(addr, size)) for addr, size in blocks}}


def restore(saved):
    """Write back every block that differs and confirm each by reading it. A User Patch the file holds is its to write."""
    continuo_slots.update(parse([a])[1] + 1 for a in saved["blocks"] if parse([a])[0] == 0x11)
    for addr, data in ((parse([a]), parse([d])) for a, d in saved["blocks"].items()):
        if list(rq1(addr, len(data))) == data:
            continue
        write_block(addr, data, stored=True)
        if list(rq1(addr, len(data))) != data:
            sys.exit(f"{fmt(addr)}: the block read back differs from the backup")
        print(f"restored {fmt(addr)}", file=sys.stderr)


# ── the sheet ────────────────────────────────────────────

LABEL = {"receive": "Receive switch", "channel": "MIDI channel", "level": "Level", "pan": "Pan",
         "key_lower": "Key range lower", "key_upper": "Key range upper", "patch_level": "Patch level",
         "mix_send": "Mix/EFX send level", "chorus_send": "Chorus send level", "reverb_send": "Reverb send level",
         "key_range_switch": "Keyboard range switch",
         "velocity_curve": "TVA velocity curve", "velocity_sens": "TVA velocity sens", "wave_gain": "Wave gain (dB)",
         "bias_direction": "Bias direction", "bias_position": "Bias point", "bias_level": "Bias level",
         "pan_keyfollow": "Pan keyfollow", "sustain": "Envelope mode (1 = sustain)", "coarse_tune": "Coarse tune (semitones)", "receive_program": "Receive program change", "patch": "Patch name", "tune": "Coarse tune (key)",
         "wave": "Wave"}
NOTE = ["C", "C#", "D", "Eb", "E", "F", "F#", "G", "Ab", "A", "Bb", "B"]


def shown(name, v):
    return f"{NOTE[v % 12]}{v // 12 - 1} ({v})" if name in ("bias_position", "key_lower", "key_upper") else str(v)


def changed(before, after):
    return [(LABEL.get(k, k), shown(k, before[k]), shown(k, after[k])) for k in after
            if not isinstance(after[k], dict) and before.get(k) != after[k]]


def sheet(before, after):
    """Markdown rows of every value that differs between two states, in part order."""
    rows = [("—", "Performance", *row) for row in changed(before, after) if row[0] != "performance"]
    for part in sorted(after["parts"], key=int):
        b, a = before["parts"][part], after["parts"][part]
        rows += [(part, f"patch `{a['patch']}`" if name == "Patch level" else "Performance part", name, was, now)
                 for name, was, now in changed(b, a)]
        same = {}                                   # (parameter, from, to) -> the tones it applies to
        for t in sorted(a["tones"]):
            if t not in b["tones"]:
                twin = next((s for s in sorted(b["tones"]) if a["tones"].get(s) == a["tones"][t]), None)
                rows.append((part, f"patch `{a['patch']}`, tone {t}", "Tone switch", "off",
                             f"on, a copy of tone {twin}" if twin else "on"))
                continue
            for row in changed(b["tones"][t], a["tones"][t]):
                same.setdefault(row, []).append(t)
        rows += [(part, f"patch `{a['patch']}`, tone {', '.join(tones)}", *row) for row, tones in same.items()]
    rows += [("10", "Performance part (rhythm)", *row) for row in changed(before["rhythm_part"], after["rhythm_part"])]
    for key in after["rhythm"]:
        rows += [("10", f"rhythm set `{after['rhythm_part']['set']}`, key {shown('key_lower', int(key))}", *row)
                 for row in changed(before["rhythm"][key], after["rhythm"][key])]
    print("| part | where | parameter | from | to |\n|---|---|---|---|---|")
    for r in rows:
        print("| " + " | ".join(str(c) for c in r) + " |")


if __name__ == "__main__":
    cmd = sys.argv[1] if len(sys.argv) > 1 else ""
    stored = "--stored" in sys.argv
    if stored:
        sys.argv.remove("--stored")
    load = lambda path: json.load(open(path))
    if cmd == "state" and "--from" in sys.argv:
        json.dump(state(saved=load(sys.argv[sys.argv.index("--from") + 1])), sys.stdout, indent=1); print()
    elif cmd == "state":
        json.dump(state(stored), sys.stdout, indent=1); print()
    elif cmd == "apply" and len(sys.argv) == 3:
        apply(load(sys.argv[2]), stored)
    elif cmd == "backup":
        json.dump(backup([int(n) for n in sys.argv[2:]]), sys.stdout, indent=1); print()
    elif cmd == "restore" and len(sys.argv) == 3:
        restore(load(sys.argv[2]))
    elif cmd == "sheet" and len(sys.argv) == 4:
        sheet(load(sys.argv[2]), load(sys.argv[3]))
    elif cmd == "read" and len(sys.argv) >= 4:
        print(fmt(rq1(parse(sys.argv[2:-1]), int(sys.argv[-1]))))
    elif cmd == "write" and len(sys.argv) >= 4:
        b = parse(sys.argv[2:])
        print("was", fmt(dt1(b[:4], b[4:])))
    else:
        sys.exit(__doc__)
