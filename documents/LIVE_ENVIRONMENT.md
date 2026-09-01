# The Live Environment

This is the rig the project is written and performed on, written down. It is not a
requirement and not a canonical setup — the library is ordinary TidalCycles code and will
run from any editor, any boot file, any sound source. What genuinely has to be installed
is short, and it lives in [README.md](../README.md) under `## Installation`.

What earns a document is everything around that. A handful of these choices are
load-bearing in ways that stay invisible until they break, one of them fails by *looking
like it worked*, and until now the whole arrangement existed only as source comments and
a shell function on one machine. So: a worked example, with the reasoning attached.

---

## The signal chain

```
Pulsar ──spawns──> ghci ──> TidalCycles ──OSC──> SuperDirt ──MIDI──> Roland JV-1010
   │                          │                     │
   │                          │                     ├──> 12 Step LED display (CC 20+, 50-53)
   │                          │                     └──> AIRA S-1 / P-6
   │                          │
   │                          └──cF──> Q-Link controller (CC 100-110, via OSC 6010)
   │
   └──/editor/highlights (6013)──> event highlighting in the buffer
```

Tidal never touches audio hardware here. Every sound is MIDI, sent through SuperDirt's
`thru` device to the JV-1010 and the two AIRA units. SuperCollider is doing MIDI routing
and LED state, not synthesis. See
[ALGORITHMIC_ORCHESTRATION.md](ALGORITHMIC_ORCHESTRATION.md) for the instrument and
channel map.

---

## Pulsar

The editor is [Pulsar](https://pulsar-edit.dev/) with the `tidalcycles` package. Install
it from Settings → Install, or `pulsar -p install tidalcycles`.

### Open `live/` as the project folder — not the repository root

This is the one thing to get right. The plugin resolves both of its important files
against the **first project root directory**:

| File | Resolution order |
|---|---|
| `BootTidal.hs` | `bootTidalPath` setting → **project root** → tidal package data-dir → plugin's own bundled copy |
| `superdirt_startup.scd` | **project root** → plugin's own bundled copy |

Both live in `live/`, so `live/` is what gets opened. The launcher does this for you
(`cd live && pulsar .`).

Open the repository root instead and neither file is found. The plugin falls back to
stock copies, and **the session boots cleanly**: SuperCollider starts, Tidal answers,
`d1 $ s "bd"` works. None of this project's definitions exist, no error is printed, and
the failure looks like a Haskell problem rather than a path problem. That is why
`live/system/bin/ghc-pkg` exists — the tidal-data-dir fallback shells out to `ghc-pkg`, and
without a working one the chain skips straight to the wrong boot file.

The layout follows from that resolution rule. Only the files the plugin resolves by name,
plus the guide you open first, sit at the top level; everything else is one level down:

```
live/
  BootTidal.hs            resolved by name at the project root
  superdirt_startup.scd   resolved by name at the project root
  USER_GUIDE.tidal        the entry point
  docs/                   further interactive guides
  system/                 the rig's machinery — see below
    bin/                  ghci, ghc-pkg, livecode
    pulsar/               config, keymap and snippet reference copies
  drumpats/               the pattern corpus
  local/                  per-machine tools, git-ignored
```

Nothing under `system/` or `docs/` is resolved by name, so those are free to move; the two
files at the top are not.

### Settings

Settings → Packages → tidalcycles → Settings. `live/system/pulsar/config.cson` holds an
annotated copy of the resulting block.

| Setting | Value | Why |
|---|---|---|
| **Interpreter** | `Default (ghc installed through cabal)` | `Stack` hardcodes a bare `stack ghci` with no arguments. The plugin spawns its interpreter with no arguments and quotes the whole command, so there is nowhere to put a flag — the flags have to ride on the executable instead. |
| **Haskell (ghci) path** | `<repo>/live/system/bin/ghci` | The wrapper. See below. |
| **Boot Tidal Path** | *(empty)* | Resolution by project root, as above. |
| **Event highlighting** | on, 20 fps | Highlights each event as it sounds. fps must stay under the denominator of the boot file's `cFrameTimespan`, which is `1/30`. |
| **Show error notifications** | off | Otherwise every line stack or GHC writes to stderr raises a modal notification, including ordinary build chatter. Errors still appear in the plugin console. |
| **SuperDirt → autostart** | on | Starts SuperCollider with `superdirt_startup.scd` from the project root. |

> **No plugin source edit is required.** Earlier setups patched `stackPrefix` in the
> package's `lib/ghc.js`. That constant is reachable only through `case 'stack':` — with
> Interpreter `default` and a `ghciPath`, which the `-O2` wrapper needs anyway, the branch
> never runs. Nothing in the tidalcycles package needs modifying for this project.

Pulsar omits any setting equal to its schema default when it rewrites `config.cson`, and
`default` *is* the default for Interpreter — so that line may disappear on its own.
Absence means the same thing.

### Event highlighting has a type consequence

With highlighting on, the plugin rewrites **every double-quoted literal** on an evaluated
line into `(deltaContext offset id "…")` before sending it. Tidal's `Stringy` class has
instances for `String` and `Pattern a` but **not for `Text`**. So a library function
taking a `Text` argument fails to typecheck at the prompt, with an error that names
`deltaContext` and not your code:

```
No instance for 'Stringy Text' arising from a use of 'deltaContext'
```

Every user-facing function in this project takes `String` for that reason. It is not a
style preference.

Two smaller consequences of the same rewriting: the regex is naive (`/"([^"]*)"/g`), so
escaped quotes inside a pattern mis-pair; and its exception rule tests the text before
the quote for `p` followed by a space, which anything containing `chop `, `up ` or
`jux ` matches by accident — those strings silently lose their highlight.

### Snippets

`live/system/pulsar/snippets.cson` is the snippet library — type a prefix, press Tab, fill the
tab-stops. Pulsar reads `~/.pulsar/snippets.cson`, so the two must be connected:

```sh
mv ~/.pulsar/snippets.cson ~/.pulsar/snippets.cson.bak
ln -s "$PWD/live/system/pulsar/snippets.cson" ~/.pulsar/snippets.cson
```

A symlink rather than a copy, because a copy drifts. This one had: the installed copy was
two prefixes and two weeks behind the repository, with nothing in the editor to reveal
it — a snippet simply failed to expand. Pulsar reads the link at startup; edits made
while it is running may need a restart to appear.

### Keys

The plugin ships no keymap, so without bindings evaluation is only reachable from the
Packages menu. `live/system/pulsar/keymap.cson` holds the ones used here — `cmd-enter`,
`ctrl-enter` and `ctrl-cmd-enter` all bound to `eval-multi-line`, `cmd-shift-enter` to
`eval-whole-editor`.

---

## The compiled session

`live/system/bin/ghci` is a wrapper that loads the library as **`-O2` object code** instead of
bytecode. It is the only reason the interpreter setting is `default`.

Why it matters: the cyclic-DP voice-leading solve is the one piece of library code on the
evaluation path slow enough to be heard. Measured on five-tone bars, a cold solve:

| bars | bytecode | `-O2` object code |
|---|---|---|
| 4 | 0.317 s | 0.0065 s |
| 8 | 0.958 s | 0.0212 s |
| 16 | 2.390 s | 0.0528 s |

That is the difference between a launcher that interrupts the music and one that does
not. Voicings are byte-identical either way — this buys speed and changes nothing
musical.

Two details the wrapper exists to get right:

- **The object directory is pinned** to `.stack-work/live-odir`, private to the wrapper.
  Left unset, GHCi writes `.o` and `.hi` **beside each source file**, and every later
  session — plain `stack ghci`, CI, anything — links those instead of compiling. After a
  source edit that is an ABI mismatch, and the errors point anywhere but the cause.
- **`GHCRTS="-N2 -qg"`.** GHCi's RTS is threaded but runs one capability by default, and
  Tidal's clock is a single thread that queries every orbit in one call per tick. Sharing
  a capability with the REPL means a heavy evaluation starves the clock and *all* audio
  stops. `-N2` gives the clock its own; `-qg` drops the parallel GC's stop-the-world
  pauses.

### What a healthy boot looks like

```
Choose ghc base path
 * custom path configured
Ghci command: /Users/…/live/system/bin/ghci
Ghc-pkg command: /Users/…/live/system/bin/ghc-pkg
Choose BootTidal.hs path
 > no custom path configured
 * found in the current directory
 * load BootTidal.hs from /Users/…/live/BootTidal.hs
Listening for external osc messages on 127.0.0.1:6013
Ok, 51 modules added.
[TidalCycles version 1.10.1]
Listening for external controls on 127.0.0.1:6010
Connected to SuperDirt.
theHarmonicAlgorithm V3 boot complete.
```

Four things to read out of that:

- `* custom path configured` and the two paths — the settings took. Without them the
  first line reads `> no custom path configured` and the commands are `stack ghci` /
  `stack ghc-pkg`.
- `* found in the current directory` — the right boot file. `* use the default contained
  in the plugin` means the wrong folder is open.
- **No `Compiling` lines at all.** With a warm object cache GHCi links what is already
  built and says nothing. If the cache is cold or `src/` has changed you get one line per
  module ending in `…/live-odir/Harmonic/Config.o`; **if those lines end in
  `interpreted`, the wrapper is not being used.**
- `Ok, 51 modules` rather than 52 — the wrapper targets the library alone, where a bare
  `stack ghci` also loads `app/Main.hs`. Nothing in the live surface uses it, and dropping
  it removes two spurious warnings (`-threaded` incompatible, duplicate `Paths_` module).

The banner reading `1.10.1` against an installed 1.10.3 is a cosmetic bug in Tidal
itself, not a version mismatch.

### After changing `src/`

The first boot after a source change recompiles at `-O2` — roughly 30 seconds for a full
rebuild — and every boot after that only links. Get it over with in a terminal rather
than at the start of a set:

```sh
live/system/bin/ghci <<< ':quit'
```

`stack build` does **not** warm this cache; it writes to its own output directory.

### Rollback

Set Interpreter back to `Stack` and clear the ghci path. That is the whole revert — the
objects are private to the wrapper, so nothing else has ever seen them.

---

## SuperCollider

Pulsar starts SuperCollider itself. [`live/superdirt_startup.scd`](../live/superdirt_startup.scd)
is what it runs, and it is worth reading — it is the only place several rig facts are
recorded. It boots the server (44100 Hz, no input channels, enlarged buffers), starts
`SuperDirt(2, s)` on **port 57120**, loads Dirt-Samples, then:

- **Binds MIDI by priority.** It walks a list — `U2MIDI Pro`, `iConnectAUDIO4+ DIN`,
  `P-6 MIDI IN`, `S-1 MIDI IN`, `IAC Driver Bus 1` — and takes the first one the system
  actually offers, registering it as SuperDirt's `\thru` sound. Change this list, not the
  patterns, when the interface changes.
- **Wraps it in `SafeMIDIOut`.** SuperDirt schedules each event's Note Off independently,
  so two overlapping events on the same note cut each other short. The wrapper
  reference-counts note-offs and adds a precautionary one. It is a **class**, so it must
  be installed separately — the source is in the tail of the startup file, and it goes in
  `~/Library/Application Support/SuperCollider/Extensions/SafeMIDIOut.sc`. Without it the
  file will not compile.
- **Opens an OSC backdoor** — `OSCdef('/run-code')` on **port 57121**, which interprets
  whatever string it is sent. That is how the Q-Link bridge gets injected into a running
  server.
- **Loads [`live/system/led-coordinator.scd`](../live/system/led-coordinator.scd)**, a passive
  pitch-class LED state machine that observes the MIDI already flowing through
  `SafeMIDIOut` and drives the 12 Step's ring and 4-character display.
- **Writes an `sc_ready` stamp** into the SuperCollider support directory, as its last
  act. The launcher waits on that file.

On a fresh machine **one** value needs changing: the `s.options.device` line naming the
audio interface (and the MIDI priority list, if your interfaces differ). The
`executeFile` call to `led-coordinator.scd` is now self-locating — it resolves against
`thisProcess.nowExecutingPath`, captured at file scope because it is nil inside the
async `s.reboot` closure.

On Linux `s.options.device` should be left **unset**: it names a JACK *server* there,
not an audio device, and `ServerOptions.devices` raises `Primitive '_ListAudioDevices'
failed` because enumeration is CoreAudio/PortAudio only.

---

## The launcher

`live/system/bin/livecode` starts the rig in the order the parts depend on each other:

1. Remove any stale `sc_ready`, so a previous session's stamp cannot be mistaken for this
   one.
2. Launch Pulsar with `live/` as the project folder; it starts SuperCollider.
3. Wait for `sc_ready`.
4. Inject [`live/system/qlink-bridge.scd`](../live/system/qlink-bridge.scd) through the OSC backdoor,
   using [`live/system/osc-send.py`](../live/system/osc-send.py) (stdlib only, no python-osc needed).
5. Paint the bridge's status line in place, so the terminal doubles as a controller
   read-out.

Injecting the bridge rather than loading it at startup means it can be re-injected
mid-session without restarting the server — `livecode --attach` does exactly that against
a SuperCollider that is already up. Injecting *early*, before `sc_ready`, fails silently:
the OSC lands nowhere and the encoders simply do nothing.

Ctrl-C stops the session and takes Pulsar with it.

---

## Ports

| Port | Direction | Carries |
|---|---|---|
| 57120 | Tidal → SC | SuperDirt; every note and control |
| 57121 | shell → SC | the `/run-code` backdoor |
| 6010 | SC → Tidal | Q-Link controller values, read with `cF` |
| 6013 | Tidal → Pulsar | `/editor/highlights` event highlighting |

---

## When it goes wrong

**Everything boots, nothing is defined.** The wrong folder is open. Check the plugin
console for `* found in the current directory` — if it says `* use the default contained
in the plugin`, it is running stock TidalCycles with none of this library.

**`No instance for 'Stringy Text'`.** Event highlighting rewriting a string literal into
`deltaContext`. The function being called needs a `String` parameter, not `Text`.

**The boot file is mangled, or `:{` blocks error.** The plugin only uses `:script` when
the boot path contains **no spaces**. With a space it falls back to feeding the file
block-by-block, split on blank lines, stripping only the first `:{`/`:}` per block. Keep
the repository somewhere without spaces in the path.

**Linker errors in a plain `stack ghci` session.** A stale object directory. Only the
wrapper should ever be given `-fobject-code`, and only with `-odir` pinned; if objects
have leaked into `src/`, `find src -name '*.o' -o -name '*.hi' -delete` clears them.

**`renameFile … .o.tmp … does not exist` mid-boot, then `Failed, N modules added` and
hidden-package errors.** Two interpreters compiled into `live-odir` at once. The plugin
itself spawns the wrapper twice on every boot — the REPL plus a second instance it uses
to `:browse` for autocomplete — and after a source edit both would recompile the same
dirtied modules into the shared odir, racing each other's temp-file renames. The wrapper
now serialises this with a PID lock inside the odir: the first instance owns the object
cache, and any concurrent instance (the autocomplete browser, or a reboot over a live
session) silently falls back to a plain bytecode GHCi, which writes nothing. Recover
from an already-poisoned odir with `rm -rf .stack-work/live-odir` (this clears the lock
too) and reboot — the next boot recompiles everything once.

**The controller is dead and nothing is wrong.** An orphaned ghci from a previous session
is still holding port 6010, so `cF` values never update and every knob reads its default.
`lsof -nP -iUDP:6010` names the process.

**A snippet prefix does not expand.** `~/.pulsar/snippets.cson` is a copy rather than a
symlink, and has drifted.

---

## Rebuilding on a fresh machine

1. GHC 9.10.3 and Stack — `stack.yaml` sets `system-ghc: true`, so GHC must be on the
   PATH. `stack build`.
2. SuperCollider, plus the SuperDirt and Dirt-Samples quarks.
3. `SafeMIDIOut.sc` into `~/Library/Application Support/SuperCollider/Extensions/` —
   source is in the tail of `live/superdirt_startup.scd`.
4. Fix `s.options.device` in `live/superdirt_startup.scd`, and the MIDI priority list
   if the interface differs.
5. Pulsar, plus the `tidalcycles` package.
6. Symlink `~/.pulsar/snippets.cson` to `live/system/pulsar/snippets.cson`; copy the bindings from
   `live/system/pulsar/keymap.cson` into `~/.pulsar/keymap.cson`.
7. Set Interpreter and the ghci path per the settings table, using this machine's own
   absolute path to `live/system/bin/ghci`.
8. Warm the object cache: `live/system/bin/ghci <<< ':quit'`.
9. Docker, for the composer graph — `docker compose up -d neo4j`, then `stack run` once
   to populate it. Only needed for `seek`; the library generates without it.
10. `live/system/bin/livecode`.

Sound check: [USER_GUIDE.md](../USER_GUIDE.md) §0 walks the three moving parts — MIDI
out, library in scope, graph reachable — in that order.
