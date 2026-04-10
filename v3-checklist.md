# V3 Release Checklist

Completion reference for the V3 release, generated from static analysis of the full codebase on 2026-04-08. Organised by priority.

---

## 1. Security (BLOCKING)

- [x] **Neo4j credentials** — Local-first tool; Neo4j runs on localhost via docker-compose with matching defaults. No network exposure. Config.hs documented with Haddock explaining the design; credentials are docker defaults, not secrets.
- [x] **docker-compose.yml** — Added inline comment noting `NEO4J_AUTH` is a local development default.
- [x] **Add `.env` patterns to .gitignore** — Added `.env`, `.env.*`, `!.env.example`.

---

## 2. Package Metadata (BLOCKING for Hackage/distribution)

`package.yaml` is missing several fields expected for a released package:

- [x] **version**: Bumped to `3.0.0`.
- [x] **maintainer**: Set to `"oscarsouth@gmail.com"`.
- [x] **synopsis**: Added.
- [x] **description**: Added.
- [x] **category**: Set to `"Music Composition"`.
- [x] **homepage**: Set to `"https://github.com/OscarSouth/theHarmonicAlgorithm"`.
- [x] **github**: Set to `"OscarSouth/theHarmonicAlgorithm"`.
- [x] **copyright**: Updated to `"2018-2026 Oscar South"`.

---

## 3. Git Hygiene

- [ ] **Commit all current changes** — 36 modified files and several new files (ORCHESTRAL_CATALOGUE.tidal, composition suites) are uncommitted.
- [ ] **Remove .DS_Store from tracking** — `git rm --cached .DS_Store` (already in .gitignore, but may still be tracked).
- [ ] **Decide on untracked composition suites** — `live/Brandenburg/`, `live/Planets/`, `live/Seasons/`, `live/TheGreat/`, `live/M-k191/` (47 untracked files). Either commit them as portfolio pieces or add to .gitignore if they're private/WIP.
- [x] **Add to .gitignore**: `.claude/`, `.serena/`, `.mcp.json` already present; `.env` patterns added.
- [x] **LICENSE copyright year** — Updated to "2018-2026".

---

## 4. CHANGELOG Gaps

The V3 CHANGELOG is comprehensive for core features but missing coverage of the Algorithmic Orchestration system. Consider adding:

- [x] **Algorithmic Orchestration** — The Orchestra module (`Orchestra.hs`) isn't mentioned as a V3 feature. It introduces: `instrument` function with MIDI range clipping, 15 pitched instrument functions (flute through contrabass), unpitched percussion (bassdrum, tamtam), the voice line system (SATB + octave variants via `VoiceLines` and `Voice` type), string articulation channel aliases (pizz/spicc/marc/legg/arco), orchestral section blocks (wind/brss/strg/perc), timbral blends (chalumeau/pastorale/brillante/maestoso/tutti). This is a major V3 feature.
- [x] **`iK` type and bundling** — The `IK = (Kinetics, Pattern Int)` type that bundles kinetics context with chord selection pattern, created via `iK bpm form chordPat`. Mentioned implicitly but not called out as a named concept.
- [x] **Three theoretical principles** — If you want these to be part of the V3 identity, briefly name them in the changelog (The Harmonic Algorithm, Algorithmic Orchestration, The Spectral Narrative).

---

## 5. Test Coverage Gaps

15/28 modules have tests (53.6%). All existing tests pass with no skipped/pending tests. Gaps to consider:

**User-facing modules without tests:**
- [ ] `Interface.Tidal.Instruments` — MIDI channel routing (ch, vel, oct, instrument launchers). Low complexity but untested.
- [ ] `Interface.Tidal.Utils` — Time/octave utilities (oct, pullBy, pushBy, humanise). Used in every performance file.

**Infrastructure modules without tests (lower priority — tested indirectly via Builder):**
- [ ] `Framework.Builder.Core` — Chain building, candidate pools, filtering. Core generation logic.
- [ ] `Framework.Builder.Diagnostics` — Diagnostic output formatting.
- [ ] `Framework.Builder.Portmanteau` — Composer name blending.
- [ ] `Framework.Builder.Types` — HarmonicContext, GenConfig types.

**Data pipeline modules without tests (lowest priority — run-once ingestion):**
- [ ] `Rules.Import.CSV`, `Rules.Import.Transform`, `Rules.Import.Graph`, `Rules.Import.Types`
- [ ] `Evaluation.Analysis.Markov` — Transition probability computation.
- [ ] `Config` — Constants only, self-documenting.

---

## 6. Documentation Polish

**Already strong — these are refinements:**

- [ ] **Haddock function-level docs** — All 28 modules have module-level Haddock headers. 19/28 have detailed function docs. The 9 without detailed function docs are mostly import pipeline internals. Consider running `stack haddock` and verifying the generated HTML looks professional.
- [ ] **`genSilent`/`genStandard`/`genVerbose` — mark as legacy in USER_GUIDE** — The positional API variants are still exported in Lib.hs alongside the new modifier-based `gen`/`gen'`/`gen''`. The CHANGELOG notes "the old positional interface remains available" but the USER_GUIDE doesn't explicitly flag these as legacy convenience functions.
- [ ] **45 exported symbols in Lib.hs without USER_GUIDE coverage** — Mostly internal re-exports (diagnostic types, filter parsers, voice leading cost functions, import pipeline). These are correctly "internal" but consider adding a brief "Advanced/Internal API" appendix to the USER_GUIDE, or using Haddock's `-- $internal` grouping.

---

## 7. CI/CD (Important for ongoing quality)

No CI configuration exists. Consider:

- [ ] **GitHub Actions workflow** — `stack build && stack test` on push/PR. Protects against regressions.
- [ ] **Haddock generation in CI** — Ensures docs stay buildable.
- [ ] **Neo4j service container for integration tests** — `BuilderSpec.hs` requires Neo4j. CI needs a Neo4j service or these tests should be tagged/skippable.

---

## 8. Professional Polish

Things you might not have thought of:

### Code Quality
- [ ] **`-Wall` compilation flag** — Not currently in package.yaml's `ghc-options`. Adding `-Wall` catches warnings (unused imports, incomplete patterns, missing signatures). Consider adding `-Wall -Wcompat` to catch issues before release.
- [ ] **Compiler warnings audit** — Run `stack build --ghc-options="-Wall"` and review any warnings. Fix or suppress intentionally.

### User Experience
- [ ] **Error messages for Neo4j connection failure** — If Neo4j isn't running when a user tries to generate, what error do they see? Consider a clear message: "Cannot connect to Neo4j at bolt://localhost:7687. Run: docker compose up -d neo4j".
- [ ] **`stack run` first-run experience** — The data ingestion (`stack run`) populates Neo4j. Does it give clear progress feedback? Is there a "success" message at the end?
- [ ] **Empty database detection** — If a user tries to generate before running `stack run`, do they get a helpful error or a cryptic empty-result?

### Repository Presentation
- [ ] **GitHub repository description** — When pushing to GitHub, set the repo description and topics (haskell, music, live-coding, tidalcycles, algorithmic-composition).
- [ ] **GitHub release** — Create a v3.0.0 release with release notes (can be derived from CHANGELOG).
- [ ] **Social preview image** — The `img/header.png` referenced in README. Verify it exists and renders well on GitHub.
- [ ] **Badges** — Consider adding build status, license, and Hackage version badges to README.md header.

### Distribution
- [ ] **Pre-built Neo4j dump** — Requires populated database + docker dump + hosting decision (GitHub release asset). Deferred.
- [x] **Minimal working example** — `live/demo.tidal` restructured: Example 1 is fully offline (`fromCadenceStates` + `iK` + `arrange`, no Neo4j); generation lines clearly labelled as ONLINE. Header explains offline vs online paths. `seek "none"` provides an algorithmic offline path (consonance fallback, no corpus) complementing manual `fromCadenceStates` construction.

### Legal
- [x] **YCACL corpus license** — Source documented in `NOTICES` file with UCI attribution and instructions for users to obtain corpus files directly if redistribution rights are unclear.
- [x] **Third-party attribution** — `NOTICES` file created covering: YCACL/UCI corpus, TidalCycles (GPL-3.0), hosc (GPL-3.0), Hindemith dissonance theory, Wiggins CSF, hasbolt (Apache-2.0), and all Haskell dependencies.

---

## 9. Known Rough Edges

Items discovered during analysis that aren't bugs but are worth knowing:

- [x] **`live/demo.tidal`** — Deleted for V3 release.
- [x] **`live/notes.txt`** — Moved to `notes/notes.txt` (gitignored).
- [x] **`literal` is an alias for `lite`** — Kept intentionally; descriptive name adds clarity.
- [x] **`voiceBy` / `VoiceType`** — Removed (duplicate entry; see above).
- [x] **`harmony` legacy function** — Removed from Bridge.hs and all exports.
- [x] **`defaultContext` alias** — Removed from Builder/Types.hs, Builder.hs facade, and all exports. Lib.hs usage example updated to `hContext`.
- [x] **`voiceBy` / `VoiceType`** — Removed from Bridge.hs and all exports; corresponding smoke tests removed from BridgeSpec.hs.
- [x] **Cello range comment** — Fixed: `C2–C5` → `C2–C6` (MIDI 84 = C6).

---

## 10. Stretch Goals

Not required for V3 but would elevate the release:

- [ ] **Hackage publication** — Register the package on Hackage for `cabal install theHarmonicAlgorithm`.
- [ ] **GitHub Pages for Haddock** — `notes/haddock_guide.md` documents the process. Automated via CI.
- [ ] **Docker image** — Pre-built container with Stack, Neo4j, and the library ready to go. Zero-setup experience.
- [ ] **Video walkthrough** — The README and USER_GUIDE have media placeholders. Even one 60-second overview video would dramatically increase engagement.
- [ ] **CONTRIBUTING.md** — Symlink or rename from CLAUDE.md for conventional discoverability.
- [ ] **CODE_OF_CONDUCT.md** — Standard for open source projects.

---

## Summary: Critical Path to Release

**Must do (blocking):**
1. Decide on credentials approach (document as local defaults vs. externalise)
2. Fill package.yaml metadata
3. Set version number
4. Commit all changes
5. Clean git state

**Should do (professional):**
6. Add Algorithmic Orchestration to CHANGELOG
7. Fix cello range comment typo
8. Add `-Wall` to ghc-options and audit warnings
9. Verify `img/header.png` exists
10. Update LICENSE copyright year

**Nice to do (polish):**
11. GitHub Actions CI
12. Self-contained offline example
13. YCACL license verification
14. GitHub release with notes
