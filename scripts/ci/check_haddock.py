#!/usr/bin/env python3
"""Guard the Haddock documentation quality won during the docs pass.

Baselines were established 2026-08-23: all 35 modules at 100% coverage, zero accidental
italics, zero unlinked identifier refs beyond a known residue, and no module missing a
Description header. This re-measures the same things so a regression is visible.

Reporting only by design — see the `docs` job in .github/workflows/ci.yml. Documentation
drift warns; it does not block work.
"""
import re, subprocess, sys, pathlib, glob

ROOT = pathlib.Path(__file__).resolve().parents[2]

EXPECT_MODULES        = 39
MAX_OUT_OF_SCOPE      = 2    # residue: 'gen'/'genP' resolved via Lib re-export
MAX_DANGLING_INTERNAL = 2    # residue: Filter's local `type PitchClass = Int`

def sh(cmd, **kw):
    return subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True, shell=isinstance(cmd, str), **kw)

def main():
    findings, hard = [], False

    # --- source-level structural checks (cheap, no build needed) ---
    no_exports, no_desc = [], []
    for f in sorted(glob.glob(str(ROOT / "src/**/*.hs"), recursive=True)):
        text = pathlib.Path(f).read_text()
        rel = pathlib.Path(f).relative_to(ROOT).as_posix()
        if re.search(r'^module\s+[A-Za-z0-9_.]+\s+where', text, re.M):
            no_exports.append(rel)
        if not re.search(r'^-- Description\s*:', text, re.M):
            no_desc.append(rel)

    if no_exports:
        findings.append(f"{len(no_exports)} module(s) have no export list "
                        f"(everything internal leaks into the public docs): "
                        + ", ".join(no_exports))
    if no_desc:
        findings.append(f"{len(no_desc)} module(s) have no `-- Description :` header "
                        f"(blank slot on the front-page index): " + ", ".join(no_desc))

    # --- build the docs and read Haddock's own coverage report ---
    print("building haddock (--no-haddock-deps) ...", flush=True)
    b = sh(["stack", "haddock", "--no-haddock-deps"])
    log = b.stdout + b.stderr
    if b.returncode != 0:
        print("haddock quality: BUILD FAILED")
        print(log[-3000:])
        return 1

    cov = re.findall(r'^\s*(\d+)% \(\s*\d+\s*/\s*\d+\) in \'([\w.]+)\'', log, re.M)
    total = len(cov)
    below = [(m, int(p)) for p, m in cov if int(p) < 100]

    if total != EXPECT_MODULES:
        findings.append(f"expected {EXPECT_MODULES} documented modules, Haddock reported {total}")
    if below:
        hard = True
        findings.append(f"{len(below)} module(s) below 100% coverage: "
                        + ", ".join(f"{m} ({p}%)" for m, p in below))

    oos = len(re.findall(r"is out of scope", log))
    if oos > MAX_OUT_OF_SCOPE:
        findings.append(f"unlinked identifier refs rose to {oos} (baseline {MAX_OUT_OF_SCOPE}) — "
                        f"a `'name'` ref that Haddock cannot resolve renders as plain text")

    # GHC 9.10's haddock emits (unlinkable) references to derived Generic
    # representation types (Rep_Foo); those are compiler artefacts, not
    # documentation targets, so they are excluded from the count.
    dangling = len(re.findall(r'^\s+- Harmonic\.(?!\S*\.Rep_)', log, re.M))
    if dangling > MAX_DANGLING_INTERNAL:
        findings.append(f"dangling internal links rose to {dangling} "
                        f"(baseline {MAX_DANGLING_INTERNAL})")

    # --- rendered-HTML checks: the only real ground truth ---
    docdir = sh("find .stack-work -type d -path '*/doc/html/theHarmonicAlgorithm' | head -n1").stdout.strip()
    if docdir:
        d = ROOT / docdir
        html = " ".join(f'"{p}"' for p in sorted(glob.glob(str(d / "Harmonic-*.html"))))
        ems = sh(f"grep -ho '<em>[^<]*</em>' {html} | sort -u").stdout.splitlines()
        # Accidental italics come from an unescaped '/': they carry leading/trailing
        # spaces. Real emphasis and book titles never do.
        accidental = [e for e in ems if re.search(r'<em> | </em>', e)]
        if accidental:
            findings.append(f"{len(accidental)} accidental italic span(s) from an unescaped "
                            f"'/' (escape as '\\/'): " + ", ".join(accidental[:5]))

        siblings = sh(f"grep -o 'href=\"\\.\\./[^\"]*\"' {html} | wc -l").stdout.strip()
        if siblings and int(siblings) == 0:
            findings.append("no sibling cross-package links found — the Hackage rewrite in "
                            "haddock.yml may now be rewriting nothing; check it still applies")

    if findings:
        print(f"haddock quality: {len(findings)} finding(s)")
        for f in findings:
            print(f"  - {f}")
        return 1 if hard else 0
    print(f"haddock quality: OK — {total}/{EXPECT_MODULES} modules at 100%, "
          f"no accidental italics, refs within baseline")
    return 0

if __name__ == "__main__":
    sys.exit(main())
