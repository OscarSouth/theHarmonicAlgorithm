#!/bin/sh
# theHarmonicAlgorithm -- Linux setup.
#
# Idempotent: safe to re-run, converges to the same state, and reports what was
# already correct rather than redoing it. Nothing here touches git.
#
# It does NOT install system packages. Dependencies are checked and reported
# with the command to install them. The one exception is GHC, which is fetched
# through ghcup (ghcup itself is a prerequisite).
#
# The steps marked SILENT below fail without any error if omitted: SuperCollider
# starts, Tidal answers, `d1 $ s "bd"` works, and none of this project's
# definitions exist.
#
#   ./scripts/install-linux.sh                 full setup
#   ./scripts/install-linux.sh --check         report only, change nothing
#   ./scripts/install-linux.sh --no-build      skip stack build/test
#   ./scripts/install-linux.sh --with-graph    also set up Neo4j + the corpus
#
# macOS: this script refuses to run. The repo files it configures are
# cross-platform; only the wiring here is Linux-specific.
set -eu

REPO=$(cd "$(dirname "$0")/.." && pwd)
GHC_WANT=9.10.3
PLUGIN_MIN=4.1.3
CORPUS_TAG=corpus-v3

CHECK=0; BUILD=1; GRAPH=0
for a in "$@"; do
  case "$a" in
    --check)      CHECK=1 ;;
    --no-build)   BUILD=0 ;;
    --with-graph) GRAPH=1 ;;
    -h|--help)    sed -n '2,26p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "unknown option: $a (try --help)" >&2; exit 2 ;;
  esac
done

# ---------------------------------------------------------------- output ----
BOLD=''; DIM=''; OFF=''
[ -t 1 ] && { BOLD=$(printf '\033[1m'); DIM=$(printf '\033[2m'); OFF=$(printf '\033[0m'); }
# The current step, so an abort can name where it stopped: under `set -e` a
# failing command exits without output, leaving later steps silently unrun.
STEP="startup"
FINISHED=0
step() { STEP="$*"; printf '\n%s==> %s%s\n' "$BOLD" "$*" "$OFF"; }
on_exit() {
  [ "$FINISHED" -eq 1 ] && return 0
  printf '\n%s!! Aborted during: %s%s\n' "$BOLD" "$STEP" "$OFF" >&2
  printf '   Steps after this one did NOT run. Fix the cause and re-run --\n' >&2
  printf '   this script is idempotent, so completed steps are skipped.\n' >&2
}
trap on_exit EXIT
ok()   { printf '  %s\n' "$*"; }
note() { printf '  %s%s%s\n' "$DIM" "$*" "$OFF"; }
warn() { printf '  !! %s\n' "$*"; }
die()  { printf '\nAborted: %s\n' "$*" >&2; exit 1; }
would() { [ "$CHECK" -eq 1 ] && { ok "would: $*"; return 0; } || return 1; }

MISSING=0
need() {  # need <cmd> <install hint> [optional]
  # ${3:-} not $3 -- `set -u` aborts on an unbound positional, and most calls
  # pass only two arguments.
  if command -v "$1" >/dev/null 2>&1; then
    ok "$1"
  elif [ "${3:-}" = optional ]; then
    note "$1 not found (optional) -- $2"
  else
    warn "$1 NOT FOUND -- $2"; MISSING=$((MISSING + 1))
  fi
}

# --------------------------------------------------------------- guards -----
[ "$(uname -s)" = Linux ] || die "this script is Linux-only; write the macOS equivalent there."
cd "$REPO"
[ -f stack.yaml ] && [ -d live ] || die "not a theHarmonicAlgorithm checkout: $REPO"

printf '%stheHarmonicAlgorithm -- Linux setup%s\n' "$BOLD" "$OFF"
note "repo: $REPO"
[ "$CHECK" -eq 1 ] && note "--check: reporting only, nothing will be written"

# ------------------------------------------------------------ 1. deps -------
step "1. dependencies"
need ghcup    "https://www.haskell.org/ghcup/"
need stack    "ghcup install stack"
need pulsar   "https://pulsar-edit.dev/  (or: apt install ./Linux.pulsar.deb)"
need sclang   "apt install supercollider supercollider-language"
need python3  "apt install python3"
need curl     "apt install curl"
[ "$GRAPH" -eq 1 ] && need docker "apt install docker.io  (and add yourself to the docker group)"
if python3 -c 'import flask' 2>/dev/null; then ok "python3-flask"
else warn "python3 flask NOT FOUND -- apt install python3-flask   (scripts/tidal-monitor)"; MISSING=$((MISSING + 1)); fi
for q in SuperDirt Dirt-Samples; do
  if [ -d "$HOME/.local/share/SuperCollider/downloaded-quarks/$q" ]; then ok "quark $q"
  else warn "SuperCollider quark $q missing -- in sclang: Quarks.install(\"$q\")"; MISSING=$((MISSING + 1)); fi
done
[ "$MISSING" -eq 0 ] || die "$MISSING dependency/ies missing (see above). Install them and re-run."

# ------------------------------------------------------------- 2. GHC ------
step "2. GHC $GHC_WANT"
. "$HOME/.ghcup/env" 2>/dev/null || true
export GHCUP_SKIP_UPDATE_CHECK=1
HAVE=$(ghc --version 2>/dev/null | sed 's/.*version //' || echo none)
if [ "$HAVE" = "$GHC_WANT" ]; then
  ok "already $GHC_WANT"
elif would "ghcup install ghc $GHC_WANT && ghcup set ghc $GHC_WANT (have: $HAVE)"; then :
else
  ok "have $HAVE, installing $GHC_WANT"
  ghcup install ghc "$GHC_WANT" && ghcup set ghc "$GHC_WANT"
fi

# ------------------------------------------------- 3. CLAUDE.md  (SILENT) ---
step "3. agent guidance stub"
# .github/CLAUDE.md is loaded by import from a root CLAUDE.md, which is
# gitignored; without this stub a clone carries no agent guidance.
if [ -f CLAUDE.md ]; then
  ok "CLAUDE.md present"
elif would "create CLAUDE.md -> @.github/CLAUDE.md"; then :
else
  echo '@.github/CLAUDE.md' > CLAUDE.md; ok "created CLAUDE.md"
fi

# ----------------------------------------------------------- 4. build ------
step "4. build"
if [ "$BUILD" -eq 0 ]; then note "skipped (--no-build)"
elif would "stack build && stack test"; then :
else
  stack build
  ok "built"
  stack test 2>&1 | grep -E "examples, .* failures|Test suite" | sed 's/^/  /' || true
fi

# --------------------------------------------- 5. SafeMIDIOut  (SILENT) ----
step "5. SafeMIDIOut.sc"
# Without this class the startup file aborts before writing sc_ready and the
# launcher waits indefinitely with no error. Symlinked, never copied: a copy
# diverges from the repository silently.
# sclang is asked rather than assumed, but under a timeout: it compiles the
# whole class library on startup, and one unreadable file in the extensions
# directory -- a symlink into a moved repo, say -- aborts the compile before
# `0.exit` is reached, so sclang never returns. That is the same directory this
# step repairs, so without the timeout a broken link blocks its own fix.
EXT=$(
  T=$(mktemp -t extdir.XXXXXX.scd)
  printf 'Platform.userExtensionDir.postln; 0.exit;\n' > "$T"
  timeout 30 sclang "$T" 2>/dev/null | grep '^/' | tail -1 || true
  rm -f "$T"
)
if [ -n "$EXT" ]; then :; else
  EXT="${XDG_DATA_HOME:-$HOME/.local/share}/SuperCollider/Extensions"
  note "sclang did not answer -- using the platform default"
fi
note "userExtensionDir: $EXT"
if [ "$(readlink "$EXT/SafeMIDIOut.sc" 2>/dev/null || true)" = "$REPO/live/system/SafeMIDIOut.sc" ]; then
  ok "already symlinked"
elif would "symlink $EXT/SafeMIDIOut.sc -> live/system/SafeMIDIOut.sc"; then :
else
  mkdir -p "$EXT"
  [ -e "$EXT/SafeMIDIOut.sc" ] && [ ! -L "$EXT/SafeMIDIOut.sc" ] && \
    mv "$EXT/SafeMIDIOut.sc" "$EXT/SafeMIDIOut.sc.bak" && note "existing copy -> SafeMIDIOut.sc.bak"
  ln -sfn "$REPO/live/system/SafeMIDIOut.sc" "$EXT/SafeMIDIOut.sc"; ok "symlinked"
fi

# ------------------------------------------------ 6. snippets  (SILENT) ----
step "6. snippets"
# A copy diverges and a snippet prefix stops expanding, with nothing in the
# editor to indicate why.
SNIP="$HOME/.pulsar/snippets.cson"
if [ "$(readlink "$SNIP" 2>/dev/null || true)" = "$REPO/live/system/pulsar/snippets.cson" ]; then
  ok "already symlinked"
elif would "symlink $SNIP -> live/system/pulsar/snippets.cson"; then :
else
  mkdir -p "$HOME/.pulsar"
  [ -e "$SNIP" ] && [ ! -L "$SNIP" ] && mv "$SNIP" "$SNIP.bak" && note "existing copy -> snippets.cson.bak"
  ln -sfn "$REPO/live/system/pulsar/snippets.cson" "$SNIP"; ok "symlinked"
fi

# ------------------------------------------------------- 7. Pulsar pkg -----
step "7. tidalcycles package (>= $PLUGIN_MIN)"
if pgrep -x pulsar >/dev/null 2>&1; then
  die "Pulsar is running. Close it first -- it rewrites config.cson on exit and
     the package directory cannot be replaced underneath it."
fi
PKG="$HOME/.pulsar/packages/tidalcycles"
PV=$(sed -n 's/.*"version": *"\([^"]*\)".*/\1/p' "$PKG/package.json" 2>/dev/null | head -1 || true)
if [ -z "$PV" ]; then
  if would "pulsar -p install tidalcycles"; then :
  else pulsar -p install tidalcycles && ok "installed"; fi
elif [ "$PV" = "$PLUGIN_MIN" ] || [ "$(printf '%s\n%s\n' "$PLUGIN_MIN" "$PV" | sort -V | tail -1)" = "$PV" ]; then
  ok "$PV (>= $PLUGIN_MIN, has event-highlighter.js)"
elif would "upgrade tidalcycles $PV -> $PLUGIN_MIN+"; then :
else
  pulsar -p install tidalcycles; ok "upgraded from $PV"
fi

# ppm replaces the whole package directory, so local patches must follow it.
# --check reports whether the patch is present rather than announcing an action,
# so an already-correct install shows nothing pending.
if [ "$CHECK" -eq 1 ]; then
  if grep -q "async sendToListener" "$PKG/lib/console-view.js" 2>/dev/null; then
    ok "listener patch: present"
  else
    ok "would: scripts/pulsar-apply-patches"
  fi
else
  "$REPO/scripts/pulsar-apply-patches"
fi

# ---------------------------------------------- 8. Pulsar settings ---------
step "8. Pulsar settings"
if [ "$CHECK" -eq 1 ]; then
  python3 "$REPO/scripts/pulsar_config.py" --ghci-path "$REPO/live/system/bin/ghci" --check || true
else
  python3 "$REPO/scripts/pulsar_config.py" --ghci-path "$REPO/live/system/bin/ghci"
fi
note "keymap: cmd-* does not bind on Linux. Copy the .platform-linux block from"
note "        live/system/pulsar/keymap.cson into ~/.pulsar/keymap.cson if not present."

# ------------------------------------------------------------- 9. PATH ----
step "9. livecode on PATH"
BIN="$HOME/.local/bin"
if [ "$(readlink "$BIN/livecode" 2>/dev/null || true)" = "$REPO/live/system/bin/livecode" ]; then
  ok "already symlinked"
elif would "symlink $BIN/livecode -> live/system/bin/livecode"; then :
else
  mkdir -p "$BIN"; ln -sfn "$REPO/live/system/bin/livecode" "$BIN/livecode"; ok "symlinked"
fi
case ":$PATH:" in
  *":$BIN:"*) ok "$BIN is on PATH" ;;
  *) warn "$BIN is NOT on PATH -- add it in your shell rc, or run live/system/bin/livecode directly" ;;
esac

# ------------------------------------------------------- 10. warm cache ----
step "10. -O2 object cache"
# ~21 s cold, ~1 s warm. Do it now, never at the start of a set.
if [ "$BUILD" -eq 0 ]; then note "skipped (--no-build)"
elif would "warm the -O2 object cache (printf ':quit' | live/system/bin/ghci)"; then :
else
  # `<<< ':quit'` is a bashism -- this script must run under dash/sh too.
  N=$(printf ':quit\n' | live/system/bin/ghci 2>&1 | grep -o 'Ok, [0-9]* modules added.' || true)
  ok "${N:-warmed}"
fi

# ---------------------------------------------------------- 11. graph -----
step "11. composer graph"
if [ "$GRAPH" -eq 0 ]; then
  note "skipped -- pass --with-graph. seek \"none\" works without it."
elif would "docker compose up -d neo4j; download+load $CORPUS_TAG"; then :
else
  docker compose up -d neo4j
  i=0; while [ $i -lt 40 ] && ! curl -sf -o /dev/null http://127.0.0.1:7474/; do sleep 3; i=$((i+1)); done
  NODES=$(curl -s -u neo4j:password -H 'Content-Type: application/json' \
    -d '{"statement":"MATCH (n) RETURN count(n) AS n"}' \
    http://127.0.0.1:7474/db/neo4j/query/v2 2>/dev/null | grep -o '\[\[[0-9]*\]\]' | tr -cd 0-9 || true)
  if [ -n "${NODES:-}" ] && [ "${NODES:-0}" -gt 0 ] 2>/dev/null; then
    ok "graph already populated ($NODES nodes)"
  else
    D="$REPO/.stack-work/corpus"; mkdir -p "$D"
    B=https://github.com/OscarSouth/theHarmonicAlgorithm/releases/download/$CORPUS_TAG
    [ -f "$D/ycacl-graph.dump" ] || curl -L --progress-bar -o "$D/ycacl-graph.dump" "$B/ycacl-graph.dump"
    curl -sL -o "$D/SHA256SUMS" "$B/SHA256SUMS"
    ( cd "$D" && sha256sum -c SHA256SUMS ) || die "corpus checksum mismatch"
    docker compose stop neo4j
    docker run --rm -i -v "$REPO/neo4j/data:/data" neo4j:5.26 \
      neo4j-admin database load neo4j --from-stdin --overwrite-destination < "$D/ycacl-graph.dump"
    docker compose up -d neo4j
    ok "corpus loaded"
  fi
fi

# --------------------------------------------------------- 12. summary ----
FINISHED=1
step "done"
if [ "$CHECK" -eq 1 ]; then
  ok "nothing was written (--check)"
else
  cat <<SUMMARY
  Start the rig with:   livecode          (or live/system/bin/livecode)
                        livecode --attach (re-inject the Q-Link bridge)

  In Pulsar, boot TidalCycles and read the console. A healthy boot says:
      * custom path configured        -> your ghciPath took
      * found in the current directory -> the right BootTidal.hs
      (no "Compiling" lines)          -> the -O2 object cache is warm
      Ok, 51 modules added.
      Connected to SuperDirt.
      theHarmonicAlgorithm V3 boot complete.

  If it says "use the default contained in the plugin", the wrong folder is
  open: the session will boot cleanly with NONE of this project's definitions.

  Still machine-specific, by design (edit live/superdirt_startup.scd):
      s.options.device       leave UNSET on Linux -- it names a JACK server
      s.options.sampleRate   must match your JACK graph
      ~midiPriorities        add your own interfaces
SUMMARY
fi
