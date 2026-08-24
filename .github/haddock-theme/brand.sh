#!/usr/bin/env bash
#
# Brand a staged Haddock site as theharmonicalgorithm.com.
#
#   usage: .github/haddock-theme/brand.sh <staged-site-dir>
#
# Called by .github/workflows/haddock.yml after the Hackage link rewrite, and
# runnable locally against a COPY of the Haddock output to preview the theme
# without pushing:
#
#   DOCS="$(find .stack-work -type d -path '*/doc/html/theHarmonicAlgorithm' | head -n1)"
#   rm -rf /tmp/ha-docs && cp -r "$DOCS" /tmp/ha-docs
#   .github/haddock-theme/brand.sh /tmp/ha-docs
#   python3 -m http.server 8080 --directory /tmp/ha-docs
#
# Never run it against .stack-work directly — it edits files in place, and the
# next `stack haddock` would then append the theme a second time.
#
# Rewrites use perl rather than `sed -i`, whose in-place flag takes no argument
# on GNU sed (Linux CI) but requires `-i ''` on BSD sed (macOS). perl -i is
# spelled the same on both, so this script is identical in CI and locally.

set -euo pipefail

SITE="${1:?usage: brand.sh <staged-site-dir>}"
THEME="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

[ -f "$SITE/index.html" ] || { echo "::error::$SITE/index.html missing — not a Haddock site" >&2; exit 1; }

# Guard against double-application: a second append would be harmless but noisy,
# and the banner would be injected twice.
if grep -q 'theharmonicalgorithm.com — dark terminal theme' "$SITE/linuwial.css" 2>/dev/null; then
	echo "already branded; nothing to do"
	exit 0
fi

# --- 1. custom domain ------------------------------------------------------
# GitHub also stores the custom domain in repo settings, but shipping CNAME in
# the artifact means an Actions deploy can never drop it.
cp "$THEME/CNAME" "$SITE/CNAME"

# --- 2. self-hosted fonts --------------------------------------------------
# Geist / Geist Mono (SIL OFL-1.1), the same woff2 the site serves. OFL.txt
# travels with them because the licence requires the notice to ship alongside.
mkdir -p "$SITE/fonts"
cp "$THEME/fonts/Geist.woff2" "$THEME/fonts/GeistMono.woff2" "$THEME/fonts/OFL.txt" "$SITE/fonts/"

# --- 3. append the theme to each stylesheet --------------------------------
# Appending beats injecting a <link> into every page: equal-specificity rules
# win by source order within the same sheet, and it avoids linuwial's
# title="Linuwial" preferred-stylesheet semantics.
cat "$THEME/chrome.css"     >> "$SITE/linuwial.css"
cat "$THEME/quick-jump.css" >> "$SITE/quick-jump.css"
cat "$THEME/source.css"     >> "$SITE/src/style.css"

# --- 4. drop the Google Fonts request --------------------------------------
# Every page pulls PT Sans from fonts.googleapis.com. Geist replaces it, so the
# request is now both redundant and a third-party call from a static docs site.
# Delimit the substitutions with # rather than {}: GNU find (Linux CI) rejects
# `-exec ... +` when `{}` occurs more than once anywhere in the argument list,
# and an empty {}-delimited replacement is itself a literal `{}`. BSD find
# (macOS) does not check, so this only ever fails in CI.
find "$SITE" -name '*.html' -exec perl -0pi -e \
	's#<link rel="stylesheet" type="text/css" href="https://fonts\.googleapis\.com[^"]*"\s*/?>##g' {} +

# --- 5. link the docs back to the site -------------------------------------
# Haddock's #page-menu carries "Contents | Index". Prepend the site so the docs
# are navigable back to where they are linked from.
find "$SITE" -name '*.html' -exec perl -0pi -e \
	's#(<ul class="links" id="page-menu">)#$1<li><a href="https://theharmonicalgorithm.com/">theharmonicalgorithm.com</a></li>#g' {} +

# --- 6. the ASCII wordmark, front page only --------------------------------
# The site's banner, byte-identical to src/components/Header.astro. Front page
# only: on all 100+ pages it would dominate a reference document.
#
# The block is built here and passed to perl through the environment. Reading
# the art inside a `perl -pi` script does not work — -pi runs an implicit read
# loop over the file being edited, so consuming <> there truncates it. The
# substitution then uses /e, making the replacement an expression rather than an
# interpolated string: the ASCII art contains backslashes, which a plain
# replacement would eat.
HA_ART="$(perl -pe 's/&/&amp;/g; s/</&lt;/g; s/>/&gt;/g' "$THEME/banner.txt")"
export HA_BLOCK="<a id=\"ha-banner-link\" href=\"https://theharmonicalgorithm.com/\" aria-label=\"The Harmonic Algorithm home\"><div id=\"ha-banner-scroll\"><pre id=\"ha-banner\" aria-hidden=\"true\">${HA_ART}</pre></div></a>"
perl -0pi -e 's{(<div id="content">)}{$1 . $ENV{HA_BLOCK}}e' "$SITE/index.html"

# --- report ----------------------------------------------------------------
# `grep -rl` exits 1 when it matches nothing, which for the font check is the
# success case — so neither pipeline may be allowed to trip `set -o pipefail`.
gf="$({ grep -rl 'fonts.googleapis.com' "$SITE" --include='*.html' || true; } | wc -l | tr -d ' ')"
bl="$({ grep -rl 'theharmonicalgorithm.com/' "$SITE" --include='*.html' || true; } | wc -l | tr -d ' ')"
echo "branded: google-font refs remaining=$gf, pages linking back=$bl"
[ "$gf" -eq 0 ] || { echo "::error::Google Fonts link survived in $gf page(s)" >&2; exit 1; }
grep -q 'id="ha-banner"' "$SITE/index.html" || { echo "::error::banner not injected" >&2; exit 1; }
