# Porting notes — macOS rig → Linux

**What this is.** theHarmonicAlgorithm 3.1.0 was developed and performed on one macOS
machine. This records every divergence found while standing it up on a second, different
environment — what the published guidance says, what the other platform actually needed,
and whether each is a documentation fix or a code fix.

**Why it is in the repository.** It is the input to the *next* port. Read it before
writing a setup script for another platform: most of what is here fails **silently**, so
these are findings you would otherwise rediscover one confusing symptom at a time.

**Where it came from.** Ubuntu 24.04.4, kernel 6.8 lowlatency, Legion Pro 7 16IRX9H
(i9-14900HX / RTX 4080), MOTU M4 over USB, PipeWire serving JACK, Pulsar with the
`tidalcycles` package, SuperCollider 3.13.0. 2026-08-31 → 2026-09-01.

A few sections cite this machine's measurements (thermal, power, refresh rate). They are
kept as the evidence behind a decision, not as recommendations for another machine.

Machine: Ubuntu 24.04.4, kernel 6.8 lowlatency, i9-14900HX + RTX 4080 Mobile,
MOTU M4 over USB, PipeWire serving JACK, Pulsar 1.123.0, SuperCollider 3.13.0.
Date: 2026-08-31. Repo at `~/theHarmonicAlgorithm`, with `~/.stack/global-project`
symlinked to it.

**Paths.** `live/` was reorganised after these notes were written: `bin/` and `pulsar/`
moved under `live/system/`, along with `SafeMIDIOut.sc`, `led-coordinator.scd`,
`qlink-bridge.scd`, `osc-send.py` and `snippets.cson` (the last into `system/pulsar/`);
`JAZZ_DIAGNOSTIC.tidal` and `ORCHESTRAL_CATALOGUE.tidal` moved to `live/docs/`.
`BootTidal.hs` and `superdirt_startup.scd` stay at the root of `live/`, because the Pulsar
plugin resolves them there by exact name. The runnable commands below use the current
paths; the findings quote the paths as they were at the time.

Legend: **[code]** = a repo change would fix it · **[doc]** = documentation only ·
**[ok]** = checked and genuinely fine, recorded so it is not re-raised.

---

## 1. `SafeMIDIOut.sc` install path is macOS-only — **[doc]**

`live/superdirt_startup.scd` and `LIVE_ENVIRONMENT.md` both say:

    ~/Library/Application Support/SuperCollider/Extensions/SafeMIDIOut.sc

On Linux the SuperCollider user extension directory is:

    ~/.local/share/SuperCollider/Extensions/SafeMIDIOut.sc

Verified by `sclang` reporting `Compiling directory
'/home/os/.local/share/SuperCollider/Extensions'` at class-library build, and
`SafeMIDIOut.findRespondingMethodFor(\noteOn)` resolving afterwards.

Suggested wording: give `Platform.userExtensionDir` as the authority rather than a
literal path — it is correct on every platform and is already what SuperCollider uses.

---

## 2. `s.options.device` — wrong *kind* of value on Linux, not just a wrong name — **[code]**

Shipped value: `s.options.device = "MacBook Pro Speakers";`

This is not merely a per-machine string. On Linux, scsynth runs under JACK and
`s.options.device` names a **JACK server**, not an audio device. There is no device
list to choose from: `ServerOptions.devices` raises

    ERROR: Primitive '_ListAudioDevices' failed.

because enumeration is a CoreAudio/PortAudio-only primitive. The correct Linux
configuration is to **leave it unset**, which connects to the default JACK server;
routing is then done in the patchbay, not in this file.

Worth documenting explicitly — the primitive failure looks alarming and reads like a
broken install rather than a platform difference.

Applied here: commented out, with the macOS value kept alongside.

---

## 3. `s.options.sampleRate = 44100` fails the boot on this rig — **[code]**

On Linux the JACK server dictates the rate and scsynth must match. PipeWire here is
force-clocked:

    clock.force-rate = 48000
    clock.allowed-rates = [ 48000 ]

so a 44100 request cannot be satisfied. Changed to 48000.

Suggested: either leave it unset on Linux (inherit the server rate) or note that this
value must match the JACK graph. The comment in the file — "the simplest way to fix the
sample rate mismatch error" next to `numInputBusChannels = 0` — is macOS reasoning and
misleads on Linux, where the mismatch is with the server, not the input channel count.

---

## 4. Hardcoded absolute path to `led-coordinator.scd` — **[code, upstreamable]**

Shipped:

    this.executeFile("/Users/oscarsouth/.stack/global-project/live/led-coordinator.scd");

`LIVE_ENVIRONMENT.md` calls this out as one of "two absolute paths [that] need changing"
on a fresh machine. It does not have to be either.

Fix applied — capture the script's own directory **at file scope**:

    ~thaLiveDir = thisProcess.nowExecutingPath !? { |pth| pth.dirname } ?? { "<fallback>" };
    ...
    this.executeFile(~thaLiveDir +/+ "led-coordinator.scd");

Subtlety worth keeping in the comment: `nowExecutingPath` is **nil inside the async
`s.reboot { }` closure**, so it must be captured before it, not where it is used. Doing
this removes one of the two documented per-machine edits entirely.

---

## 5. MIDI priority list has no Linux entry — **[code]**

The list ends at `["IAC Driver", "Bus 1"]`, which is the macOS IAC virtual bus and never
exists on Linux. If none of the five hardware devices are attached, nothing binds and
`~midiFound` stays false — which also silently skips the LED coordinator block.

Added two Linux fallbacks at the end of the list:

    ["M4", "M4 MIDI 1"],                      // MOTU M4 DIN out, ALSA card 2
    ["Midi Through", "Midi Through Port-0"]   // kernel virtual port, always present

`Midi Through` is the Linux analogue of IAC: always available, so it guarantees a bind.

---

## 6. Pulsar keymap is unusable as shipped on Linux — **[code/doc]**

`live/pulsar/keymap.cson` binds:

    'ctrl-cmd-enter' | 'ctrl-enter' | 'cmd-enter'  -> eval-multi-line
    'cmd-shift-enter'                              -> eval-whole-editor

On Linux, Pulsar's `cmd` does not bind, so **only `ctrl-enter` survives and
`eval-whole-editor` becomes unreachable from the keyboard** — the exact situation the
file's own header warns about ("without bindings, evaluation is only reachable from the
Packages menu").

Bindings actually in use on this machine, which work and are worth publishing as the
Linux column of that table:

    'alt-enter'       -> tidalcycles:eval-multi-line
    'ctrl-alt-enter'  -> tidalcycles:eval-multi-line
    'alt-shift-enter' -> tidalcycles:eval-whole-editor

---

## 7. `live/pulsar/config.cson` hardcodes a macOS home — **[doc]**

    ghciPath: "/Users/oscarsouth/.stack/global-project/live/bin/ghci"

The file is labelled a reference copy, so this is minor, but a reader on Linux copying
the block gets a broken interpreter path with no error until boot. Worth a placeholder
like `<repo>/live/bin/ghci` and a one-line "use your own absolute path".

Same for `LIVE_ENVIRONMENT.md`'s "Settings (Cmd+, → Packages → ...)" — `Ctrl+,` on Linux.

---

## 8. The healthy-boot signature in the doc is stale — **[doc]**

`LIVE_ENVIRONMENT.md` §"What a healthy boot looks like" says:

    Ok, 47 modules added.

and explains 47-rather-than-48 as the wrapper targeting the library alone. On 3.1.0 the
actual count through `live/bin/ghci` is:

    Ok, 51 modules added.

Since that section is explicitly a checklist for "is my boot healthy", a stale number
sends someone hunting a non-existent problem. Either update it or phrase it as "the same
count every time" rather than a literal.

Everything else in that checklist reproduced exactly: object-code compile lines ending in
`live-odir/….o` on the cold boot, and a warm boot with **no `Compiling` lines at all**
(measured: 21.5 s cold, 1.0 s warm).

---

## 9. The symlinked global-project gives the repo two absolute paths — **[doc]**

`~/.stack/global-project` → `~/theHarmonicAlgorithm` is convenient and preserves
`stack ghci` from anywhere. But GHC records source paths inside `.hi` files, so
alternating between the two spellings can invalidate the `-O2` object cache and force a
full ~21 s recompile at the worst possible moment.

Rule adopted here: the **real** path everywhere that is scripted or configured (Pulsar
`ghciPath`, launchers); the symlink only for casual interactive use.

Also worth recording: 3.1.0 **no longer needs** the global-project location at all —
`live/bin/ghci` resolves its own repo root and passes `--stack-yaml` explicitly. That was
a 2.0.0 constraint (Pulsar's `Stack` interpreter mode runs a bare `stack ghci`). The
symlink is now compatibility, not requirement.

---

## 10. `SafeMIDIOut` in the repo is an older revision than `led-coordinator.scd` needs — **[code, BLOCKING]**

The class source commented into the tail of `superdirt_startup.scd` defines only
`midiOut` and `noteOnCounts`. But `led-coordinator.scd`'s own "HOW IT IS WIRED" header
(lines 36-38) requires three settable callback slots:

    ~midiOut.onNoteOn  = { |c, n, v| ~ledCoordinator.onNoteOn(c, n, v) }
    ~midiOut.onNoteOff = { |c, n, v| ~ledCoordinator.onNoteOff(c, n, v) }
    ~midiOut.onControl = { |c, ct, v| ~ledCoordinator.onControl(c, ct, v) }

With the shipped source, `onNoteOn_` falls through `doesNotUnderstand` to the raw
`MIDIOut`, which does not understand it either:

    ERROR: Message 'onNoteOn_' not understood.
    RECEIVER: Instance of MIDIOut

**This aborts the startup file before it writes the `sc_ready` stamp**, so
`live/bin/livecode` waits forever at step 3. That makes it a boot-blocking bug, and it
is *not* Linux-specific — a fresh install from the commented source fails identically on
macOS. The author's installed `Extensions/SafeMIDIOut.sc` has evidently drifted ahead of
the copy pasted into the comment block: **exactly the copy-drifts failure the docs warn
about for `snippets.cson`, and the argument for making this a real file in the repo
rather than a comment to be pasted.**

Fixed here by adding `var <>onNoteOn, <>onNoteOff, <>onControl;` and firing them from
`noteOn` / `noteOff` / `control`. One semantic decision worth carrying upstream:
**`onNoteOff` fires only on a TRUE release** — when the reference count reaches zero and
a real Note Off is actually emitted — so the LED extinguishes when the note stops
sounding, not when a suppressed duplicate arrives. Firing on every call would reintroduce
the very bug the class exists to prevent, on the LED ring instead of the synth.

---

## 11. MIDI matcher requires an exact (device, port) pair — silently binds the wrong device — **[code, SERIOUS]**

    var isAvailable = MIDIClient.destinations.any { |endpoint|
      (endpoint.device == devName) && (endpoint.name == portName) };

Port names are platform-specific. ALSA appends `" MIDI n"`, and the AIRA units are named
differently again:

| Device | macOS CoreMIDI port | Linux ALSA port |
|---|---|---|
| U2MIDI Pro | `U2MIDI Pro` | `U2MIDI Pro MIDI 1` |
| P-6 / S-1 | `MIDI IN` | `P-6 MIDI 1` / `S-1 MIDI 1` |

So **every macOS entry fails to match on Linux** and the walk falls through. Measured on
this machine with a U2MIDI Pro physically connected:

    OLD matcher -> binds to  M4 / M4 MIDI 1        (priority 6 — wrong device)
    NEW matcher -> binds to  U2MIDI Pro / ...      (priority 1 — correct)

It reports `SUCCESS` either way, so nothing looks wrong; the MIDI just goes somewhere
else. On this rig that would have sent the whole arrangement to the audio interface's
DIN port instead of the JV-1010.

Fixed by keeping the exact match first (macOS behaviour unchanged) and falling back to
the first destination on the **same device**. Device identity is what matters; the port
name is only a disambiguator.

---

## 12. Dirt-Samples path is a literal macOS path — loads zero samples on Linux — **[code]**

    ~dirt.loadSoundFiles("~/Library/Application Support/SuperCollider/downloaded-quarks/Dirt-Samples/*");

On Linux this produces:

    WARNING: no folders found in paths: '~/Library/Application Support/.../Dirt-Samples/*'

which reads like a missing quark rather than a wrong path — the quark was installed and
had 220 sample directories all along. Replaced with `Platform.userAppSupportDir +/+
"downloaded-quarks/Dirt-Samples/*"`, correct on both platforms. After the fix:
**217 sample banks, 444 MB loaded.**

Same root cause as §1 (the `SafeMIDIOut` Extensions path) — `Platform.userAppSupportDir`
and `Platform.userExtensionDir` exist precisely for this and should be used throughout.

---

## 13. `sc3-plugins` is not installed — **[env, non-blocking]**

    ---- SC3-Plugins not found. This is not a problem.
    WARNING: Dirt could not load some synths from default-synths.scd

SuperDirt degrades gracefully (a comb delay substitutes for `SwitchDelay`). Irrelevant to
a MIDI-only rig, but worth installing for sample-based work. Ubuntu ships
`supercollider-sc3-plugins`.

---

## 14. `live/bin/livecode` cannot run on Linux at all — **[code, BLOCKING]**

    #!/bin/zsh
    live=${0:A:h:h}
    PULSAR=${PULSAR:-/Applications/Pulsar.app/Contents/MacOS/Pulsar}
    ready_file="$HOME/Library/Application Support/SuperCollider/sc_ready"

Four macOS assumptions, and the first is fatal: zsh is **not installed by default on
Ubuntu**, so the launcher fails with

    timeout: failed to run command './livecode': No such file or directory

which reads as "the script is missing" rather than "the interpreter is missing" — one of
the more misleading errors in Unix.

Ported to POSIX `sh`, which works unchanged on both platforms:

| macOS-ism | replacement |
|---|---|
| `#!/bin/zsh` | `#!/bin/sh` |
| `${0:A:h:h}` (zsh-only expansion) | `live=$(cd "$(dirname "$0")/.." && pwd)` |
| `/Applications/Pulsar.app/...` | `$PULSAR`, then `command -v pulsar`, then the bundle |
| `~/Library/Application Support/SuperCollider` | detected: that dir if present, else `${XDG_DATA_HOME:-$HOME/.local/share}/SuperCollider` |
| `echo "\nStopped."` | `printf` (only zsh's echo interprets the escape) |

Original preserved at `live/local/livecode.upstream-zsh`. Dropping the zsh dependency is
worth doing upstream regardless of platform.

---

## 15. Sample loading is unconditional, and this is a MIDI-only rig — **[code, perf]**

`~dirt.loadSoundFiles(...)` runs on every boot. On a rig where "Tidal never touches audio
hardware — every sound is MIDI" (LIVE_ENVIRONMENT.md, §The signal chain), that is
**444 MB of RAM and ~40 s of boot time for 217 sample banks that are never played.**

Made opt-in behind `~thaLoadSamples` (default `false`). Boot with samples skipped is
clean and immediate. Worth a flag upstream rather than a code edit, since the same repo
serves both sample-based and MIDI-only use.

---

## 16. `~/qlink-bridge.scd` in `$HOME` had drifted from the repo copy — **[env]**

The Linux `.bashrc` had a `start_qlinks()` that read `$HOME/qlink-bridge.scd` and pushed
it with the `oscsend` binary. The macOS `.zshrc` had already moved on: it runs
`python3 osc-send.py localhost 57121 /run-code s --file qlink-bridge.scd` **from the
repository's `live/` directory**.

Timestamps confirm the drift: `~/qlink-bridge.scd` 2025-08-04 vs `live/qlink-bridge.scd`
2026-08-31, and the files differ. Same failure mode as `snippets.cson` — a copy in `$HOME`
that nothing keeps in sync.

`start_qlinks` now delegates to `live/bin/livecode --attach`, which is exactly this
sequence against an already-running SuperCollider, using the repo's files.

---

## 17. SuperCollider version gap between the two machines — **[env, accepted]**

    Ubuntu 24.04 noble/universe : 3.13.0  (the only candidate; no PPA)
    upstream latest             : 3.14.1

Staying on 3.13.0: it is what Ubuntu Studio ships and tests against, `sc3-plugins 3.9.1`
in the archive is built for it, and SuperDirt supports both. Building 3.14.1 from source
would decouple the rig from the distro's audio stack for no feature this rig uses. Worth
re-checking if the macOS side moves to a 3.14-only SuperDirt feature.

---

## 18. Load-bearing state that lives OUTSIDE the repository — **[code, structural]**

The recurring failure of this port was not any single bug. It was **files the rig
depends on that live in `$HOME`, are not version controlled, and drift silently.**
Three of them had already drifted before we started, and one of those blocked the boot.

| File | What it does | How it failed / would fail | Status |
|---|---|---|---|
| `Platform.userExtensionDir/SafeMIDIOut.sc` | note-off refcounting + the LED coordinator's `onNoteOn`/`onNoteOff`/`onControl` hooks | **Boot-blocking.** Exists in the repo only as a commented block inside `superdirt_startup.scd`. The Mac's real file had moved ahead of it; installing from the comment aborted the boot before `sc_ready`, so `livecode` waited forever | ⚠ **still unversioned** — see recommendation below |
| tidalcycles `lib/console-view.js` patch | POSTs each console line to the tidal-monitor | **Silently wiped by every `ppm install`.** The bridge just stops receiving; no error, because the fetch swallows its own failures by design | ✅ `scripts/pulsar-apply-patches` (idempotent, ESM-aware syntax check) |
| the console listener | receives that console stream | was `scripts/tidal-monitor/listener.py`: untracked, machine-local, bound to `0.0.0.0` (console output readable by anything on a venue network), and duplicated in three places that had already diverged | ✅ `scripts/tidal-monitor/`, bound to `127.0.0.1`; the `$HOME` copy is deleted and the launcher has **no fallback** — one source of truth |
| `~/qlink-bridge.scd` | Q-Link encoder bridge | `$HOME` copy was 2025-08-04, repo copy 2026-08-31, **and they differed**. The Linux `start_qlinks` pushed the stale one | ✅ `start_qlinks` now delegates to `live/bin/livecode --attach`, which uses the repo file |
| `~/.pulsar/snippets.cson` | snippet library | the doc already records a two-week silent drift here | ✅ symlinked to `live/snippets.cson` |
| `~/.pulsar/config.cson` | editor settings | repo's `live/pulsar/config.cson` is a reference copy and is **already stale** — it names `/Users/oscarsouth/.stack/global-project` | ⚠ reference only, by design; keep it current |
| `~/.pulsar/keymap.cson` | eval bindings | repo's copy binds `cmd-*`, which does not bind on Linux at all | ⚠ needs a Linux column (see §6) |
| `~/.pulsar/styles.less`, `init.js` | editor appearance/behaviour | not in the repo; a machine rebuild loses them | ⚠ unversioned |
| shell functions (`liveCode`, `start_qlinks`) in `.bashrc` / `.zshrc` | launchers | duplicated per machine and already divergent between them | ✅ both now delegate to `live/bin/livecode` |
| root `CLAUDE.md` | imports `.github/CLAUDE.md` | **gitignored, so a fresh clone does not have it** and none of the repo's agent guidance loads. The clone looks complete | ✅ documented in setup below |
| `live/local/*` (thermal, display, offload, affinity) | machine-specific tuning | genuinely per-machine — correct to stay gitignored | ✅ by design |

### Recommendation: make `SafeMIDIOut.sc` a real file

It is the only remaining **boot-blocking** item, and the comment-block approach is what
caused the drift. Suggested:

1. `live/SafeMIDIOut.sc` — the class as a tracked file.
2. Replace the commented copy in `superdirt_startup.scd` with a one-line pointer.
3. Install by **symlink**, not copy, for the same reason `snippets.cson` is symlinked:

```sh
# sclang has no -e flag (checked on 3.13.0), so ask it via a script file:
printf 'Platform.userExtensionDir.postln; 0.exit;\n' > /tmp/extdir.scd
EXT=$(sclang /tmp/extdir.scd 2>/dev/null | grep '^/' | tail -1)
mkdir -p "$EXT" && ln -sf "$PWD/live/system/SafeMIDIOut.sc" "$EXT/SafeMIDIOut.sc"

# or just use the literal path — there are only two:
#   macOS  ~/Library/Application Support/SuperCollider/Extensions
#   Linux  ~/.local/share/SuperCollider/Extensions
```

A symlink cannot drift. A copy already did.

---

## 19. Fresh-machine setup process

> **Now automated on Linux** by `scripts/install-linux.sh` — idempotent, `--check` reports
> what is outstanding without writing anything. The ordered steps below are what it does
> and why; they are the specification for an equivalent script on another platform.

Ordered, with the reasoning attached. Steps marked **⚠** are the ones whose omission
fails *silently*.

```sh
# 1. Repo
git clone https://github.com/OscarSouth/theHarmonicAlgorithm ~/theHarmonicAlgorithm
cd ~/theHarmonicAlgorithm

# 2. ⚠ Load the repo's agent guidance (gitignored, so the clone lacks it)
echo '@.github/CLAUDE.md' > CLAUDE.md

# 3. Toolchain — GHC 9.10.3 exactly; stack.yaml sets system-ghc: true
ghcup upgrade                      # an old ghcup cannot install 9.10.3 cleanly
ghcup install ghc 9.10.3 && ghcup set ghc 9.10.3
ghcup install stack && ghcup install hls
stack build && stack test          # expect 1130 examples, 0 failures

# 4. Graph (optional; `seek "none"` works without it)
docker compose up -d neo4j
#    then load the corpus-v3 dump per README.md §2

# 5. SuperCollider: SuperDirt + Dirt-Samples quarks, then ⚠ SafeMIDIOut
#    Extensions dir is Platform.userExtensionDir:
#      macOS  ~/Library/Application Support/SuperCollider/Extensions/
#      Linux  ~/.local/share/SuperCollider/Extensions/
#    Without SafeMIDIOut the startup file aborts before writing sc_ready
#    and the launcher hangs with no error.

# 6. ⚠ Fix the two machine-specific values in live/superdirt_startup.scd
#      s.options.sampleRate   -> must match the JACK/CoreAudio graph
#      the MIDI priority list -> add this machine's interfaces
#    (the led-coordinator path is now self-locating; no longer needed)

# 7. Pulsar + the tidalcycles package (4.1.3+ for event highlighting)
pulsar -p install tidalcycles
ln -sf "$PWD/live/system/pulsar/snippets.cson" ~/.pulsar/snippets.cson  # ⚠ symlink, never copy
#    Settings: interpreter=default, ghciPath=<repo>/live/system/bin/ghci,
#    bootTidalPath empty, showErrorNotifications off, superDirt autostart on
#    Keymap: macOS cmd-* do NOT bind on Linux — use alt-enter / alt-shift-enter

# 8. ⚠ Re-apply local plugin patches (ppm wipes them on every upgrade)
./scripts/pulsar-apply-patches

# 9. Console bridge
pip install flask                  # scripts/tidal-monitor/listener.py

# 10. Warm the -O2 object cache before you need it (~21 s cold, ~1 s after)
live/system/bin/ghci <<< ':quit'

# 11. Go
live/system/bin/livecode
```

**Verify the boot, don't assume it.** A wrong-folder boot looks healthy: SuperCollider
starts, Tidal answers, `d1 $ s "bd"` works, and none of this project's definitions exist.
The console must say `* custom path configured`, `* found in the current directory`,
**no `Compiling` lines**, and `Ok, 51 modules added` (the doc still says 47 — stale).

---

## 20. `live/bin/livecode` tracked the wrong PID — three bugs, one cause — **[code, BLOCKING]**

`/usr/bin/pulsar` (and ppm's `pulsar` on macOS) is a **bash wrapper**, not the editor:

```sh
( nohup "$PULSAR_EXECUTABLE" --executed-from="$(pwd)" --pid=$$ "$@" --no-sandbox ... ) &
```

It backgrounds the real Electron binary and exits at once, so `pulsar_pid=$!` captured a
process that was already dead. Every reported symptom followed from that:

| Symptom | Mechanism |
|---|---|
| `Waiting for SuperCollider… / Pulsar exited.` | `kill -0` failed on the dead wrapper → `exit 1` before the monitor started |
| Ctrl-C never closed Pulsar | cleanup signalled a PID that was already gone |
| tidalListener window showed `Terminated` | `livecode` exiting 1 fired the caller's EXIT trap |

Fixed by branching on **what the target is**, not what is on `PATH`: the macOS `.app`
binary stays in the foreground and `$!` is correct; a shell wrapper is detected by its
shebang and the real process found via the `--executed-from=` argument it passes.

Three traps worth carrying to any similar code:

- **`pgrep -f` matches the shell running it.** The script searching for Pulsar has the
  search string in its own command line, so `cleanup` would have signalled *itself*. Every
  match is now confirmed with `ps -o comm=`. This bit three separate times in one session —
  once on `listener.py` (matched the `sh -c` wrapper, would have orphaned the listener),
  once here, and once on a `pkill -f livecode-perform` that killed the calling shell.
- **`wait` only works on your own children.** A PID found by `pgrep` is not one, so cleanup
  polls `kill -0` and escalates to `-9`.
- **An INT trap does not exit.** Without `trap 'cleanup; exit 130' INT TERM` the handler ran,
  fell back into the wait loop, saw the Pulsar it had just killed, and printed
  `Pulsar exited.` before the EXIT trap cleaned up a second time — three lines of noise for
  one Ctrl-C. A `cleaned` guard makes cleanup run exactly once.

---

## 21. `dirname "$0"` is wrong for a symlinked entry point — **[code]**

Installing `~/.local/bin/livecode` as a symlink broke the launcher immediately:

```
python3: can't open file '/home/os/.local/osc-send.py'
```

`$0` is the **symlink's** path, so `dirname "$0"/..` resolved to `~/.local`. `readlink -f`
would fix it in one line, but **BSD/macOS `readlink` has no `-f`** — it would have worked
on Linux and silently failed on the machine this has to stay compatible with. The launcher
now walks the symlink chain in POSIX sh.

---

## 22. Two conventions were obsolete and could simply be deleted — **[code, simplification]**

Both were load-bearing when the project built as Stack's implicit global project. Neither
survives the switch to a pre-compiled `-O2` wrapper.

**The tidalcycles plugin needed no patch.** In `lib/ghc.js`, `stackPrefix` is reachable
only through `case 'stack':`. With Interpreter `default` and a `ghciPath` — which the
wrapper requires anyway — that branch never runs. The `stackPrefix` edit found on this
machine was dead code.

**`~/.stack/global-project` was not needed.** `live/bin/ghci` derives its own repo root and
passes `--stack-yaml` explicitly, so Stack's implicit project is never consulted. The
symlink is gone and nothing changed.

One design decision retired both workarounds; the repo had simply never gone back to
remove them. Also corrected while there: the healthy-boot signature said `Ok, 47 modules`
(3.1.0 gives **51**, and a bare `stack ghci` gives 52 — both measured), and the "two
absolute paths need changing" instruction is now one.

---

## 23. Idempotence has to be tested from *broken* states, not just clean ones — **[process]**

`scripts/install-linux.sh` and `scripts/pulsar-apply-patches` both passed
"run twice, nothing changes" while still being unable to repair a **half-applied** state.
The patcher tested `grep -q sendToListener`, which matched a leftover *call site* in a
file whose *method* had been removed: it concluded the patch was present, skipped the
edit, and left verification to fail — which under `set -e` killed the installer at step 7,
silently, with steps 8–11 never running.

Two fixes, both worth generalising:

- **Normalise, then apply.** The patcher now strips every remnant before applying, so it
  converges from any prior state. Verified against six: pristine, correct, no-method,
  no-call, doubled-call, fully-reverted — all produce a byte-identical file. Strip and
  apply must be exactly symmetric, or repeated runs drift the file one blank line at a time
  (they did).
- **Report where you aborted.** The installer traps EXIT and names the step it failed on,
  because `set -e` otherwise leaves a half-configured machine with no indication that later
  steps never ran.

---

## 24. MIDI: bind the virtual port on Linux, the device on macOS — **[code]**

The two platforms want opposite strategies, and the shipped list only expressed one.

On Linux the PipeWire MIDI node name **embeds the USB bus path**:

    Midi-Bridge:CME Pro U2MIDI Pro at usb-0000:00:14-0-3-3- full speed:(playback_0) U2MIDI Pro MIDI 1

so it changes every time the device enumerates on a different socket. Binding a device by
name therefore cannot hold. The RaySession patch file this replaced had accumulated **five
names for the one U2MIDI Pro** — `U2MIDI Pro 1/2/3` plus two `usb-…` variants — and none
of them matched the device as plugged in on the day. Each replug had quietly minted
another dead rule.

The fix is to bind the kernel's always-present `Midi Through` and let the patchbay route
onward, which is also more flexible: swap or replug an interface and nothing in the rig
changes. macOS has no equivalent unless the IAC Driver is enabled, so it keeps the device
list. Selected with:

```supercollider
if (thisProcess.platform.name == \linux) { ... } { ... }
```

**Not `Platform.name`** — that returns the string `"Platform"` (the class name) and
compares false against `\linux`. Verified both ways before use.

Consequence worth stating in the file itself: on Linux MIDI now goes **nowhere** unless
something routes Midi Through. That is the design, not an oversight — hence §25.

---

## 25. Replacing a session manager with four lines of `pw-link` — **[env]**

`~/defaultSession/default/raysession.xml` launched exactly one client, `ray-jackpatch`.
A GUI plus four Python processes, autostarted at login, existed solely to restore
connections — and restored them by exact port name, which is the thing that cannot work
here (§24).

Replaced by `live/local/midi-autoconnect`: rules are `out-substring|in-substring`, so
`U2MIDI Pro MIDI 1` matches whatever node prefix the device enumerates as. `pw-link -m`
streams `=` existing / `+` added / `-` removed (confirmed empirically with `pw-loopback`),
so reacting to `+` gives hotplug re-linking for free. Started by the session, not at login.

**Coexistence with RaySession is safe, and the reason is specific.** `ray-jackpatch`
disconnects a link only when it is absent from `saved_connections` **and** both port names
were in the saved `<graph>` **and** `Glob.open_done_once` is true — i.e. on a *second*
session open (`main_loop.py:261-270`). Its own header concedes the behaviour "is
(probably) not suitable if we start the ray-jackpatch client once the session is already
loaded". Opening RaySession on an **empty** session removes even that: no
`saved_connections`, no `<graph>`, so the condition can never be satisfied. `live/local/patchbay`
opens exactly that, and the third process it starts — `ray-jackpatch_to_osc` — only reads
the graph to draw the canvas.

**A generic hook, not machine specifics in the repo.** `live/bin/livecode` gained:

```sh
HOOK="$repo/live/local/session-hook"
[ -x "$HOOK" ] && "$HOOK" start      # before Pulsar
[ -x "$HOOK" ] && "$HOOK" stop       # in cleanup
```

The repo carries the mechanism; device names stay in gitignored `live/local/`. A machine
without the hook is unaffected, and macOS can supply its own or none.

---

## 26. A dangling symlink in the Extensions directory hangs `sclang` — **[code]**

`sclang` compiles the whole class library at startup, including every file under
`Platform.userExtensionDir`. An unreadable entry there aborts the compile:

    ERROR: Could not read .../Extensions/SafeMIDIOut.sc: basic_ios::clear: iostream error.
    Library has not been compiled successfully.

It then **does not exit**. A script passed on the command line never runs, so a trailing
`0.exit` never executes and the process waits indefinitely.

Since `SafeMIDIOut.sc` is installed as a symlink into the repository, anything that moves
or renames the target leaves that symlink dangling — and the failure is not "SuperCollider
reports a missing class", it is "every `sclang` invocation hangs".

`scripts/install-linux.sh` step 5 asked `sclang` for `userExtensionDir` before repairing
the symlink, so a broken link blocked its own fix. The probe now runs under
`timeout 30` and falls back to the platform default path, which is enough to complete the
repair. Worth carrying into any macOS installer: never call `sclang` unbounded from a
setup script.

## Checked and fine — do not re-raise

- **[ok]** `shasum -a 256 -c` in README §2 — `shasum` **is** present on Ubuntu 24.04
  (`/usr/bin/shasum`, from perl). The corpus dump verified first try. No change needed.
- **[ok]** `Platform.userAppSupportDir` for the `sc_ready` stamp — correctly portable,
  unlike the hand-written Extensions path in §1.
- **[ok]** `docker compose` v2 syntax, the `127.0.0.1:`-bound port mappings, and the
  `neo4j-admin database load --from-stdin` recipe all worked verbatim. Graph loaded
  405 MiB / 38 files; 660 `:Cadence` + 994 `:Change` nodes, 246,959 `NEXT` rels.
- **[ok]** `system-ghc: true` with ghcup-managed GHC 9.10.3 — clean `stack build` and
  `stack test` (1130 examples, 0 failures) with no `extra-deps` needed.
- **[ok]** `seek "none"` (offline fallback), `seek "bach"` (graph) and
  `seek "bach:30 debussy:70"` (blend, portmanteau "Babussy") all worked from the wrapper
  GHCi against the loaded graph.

---

## Toolchain note (not a repo issue)

`ghcup` 0.1.50.2 could not install GHC 9.10.3 usefully — it also reported itself as
`stray`. Upgrading to 0.2.6.2 first was necessary. Worth one line in the README next to
the ghcup link, since 9.10.3 is a hard requirement and an old ghcup is the likely state
of any machine that has been idle.

---

## Open — needs hardware / BIOS

- **Display routing per port.** HDMI on this chassis is hardwired to the dGPU. The
  machine has two Thunderbolt 4 host routers (`05:00.0`, `39:00.0`), and DP-over-TB is
  tunnelled through the Intel display engine — so a dock should land on the iGPU and let
  the dGPU sleep. Untestable until the BIOS MUX is set to Hybrid (in Discrete-only mode
  every output is on the dGPU by definition). Fill in after the switch:

  **RESOLVED 2026-08-31, after BIOS Hybrid was enabled:**

  | Route | DRM connector | Driver | dGPU can sleep? |
  |---|---|---|---|
  | Internal panel | `card1-eDP-1` | **i915 (iGPU)** | n/a — this is the win |
  | Lenovo dock (USB-C) | `card2-DP-4`, surfaced as MST `DP-1-2.3` | **nvidia (dGPU)** | **No** — `display_active: Enabled`, `runtime_status: active` |
  | HDMI-0 | not tested (dock in use) | | |

  My Thunderbolt prediction was **wrong**: DP over this dock lands on the dGPU,
  not the iGPU. So the dGPU keeps an active display and cannot enter D3cold
  while the dock is connected.

  It barely matters, because the win came from somewhere else. Moving the
  2560x1600 panel to the iGPU dropped the dGPU from two high-refresh scanouts to
  one 1080p, and its memory clock now sits at **405 MHz instead of 7000 MHz** at
  every refresh rate tested. Measured dGPU idle: **17.5 W → 5.4 W**.

  Practical consequence: the `NVreg_DynamicPowerManagement=0x02` D3cold setting
  (root script stage 5) only pays off with the dock **unplugged**. Plugged in,
  P8 at ~5.4 W is the floor. Output names also change with the dock and hub, so
  `display-profile` detects them rather than hardcoding.

- **MIDI priority list** cannot be finalised until the JV-1010, AIRA S-1/P-6 and 12 Step
  are connected. Only `M4 MIDI 1` and `Midi Through Port-0` are present today.

- **Personal snippets from the v2 machine** are preserved at
  `~/.pulsar/snippets.cson.bak` and are **not** in the v3 repo set: `303 timbre`,
  `boeing drone timbre`, `boeing impact timbre`, `electric bass overtones`, `juno`,
  `piano timbre`, `sh101`, `sine wave timbre`. They are v2-era API and would need porting.
  Decide whether they belong in `live/local/` or upstream.
