# Stravinsky, Suite No. 1 — I. Andante: reconciliation of the 2021 transcription against the Chester full score

Status: S1 deliverable for review. Nothing below has been ported yet; the port (`01_andante.tidal`) will be written only after this table is agreed.

## Sources

- Score: Chester 1926 full score, IMSLP 792184 (`~/Documents/IMSLP792184-PMLP1198800-Stravinsky_-_Suite_No._1_(orch._score_-_Chester).pdf`), Andante = PDF pp. 2–4. No text layer; read from 300 dpi renders with 3–6× zooms per staff and bar (scratchpad `scores/`). Every pitch was read by counting staff lines and ledgers from the clef.
- Transcription: `udagan-sensoria-concert/perform/Stravinsky_suite1.tidal` with its `BootTidal.hs` (Sept–Oct 2021). Pitch semantics there: `key = D \\\ mMaj` = `toScale [2,4,6,7,9,11,13]` (legacy `MusicData`, `pitchClass D = 2`), so **degree 0 = D4 (Tidal note 2)**, degree 7 = D5, degree −7 = D3; `transpose 3 key` adds 3 semitones after the scale lookup; `+|` takes rhythm from the pitch pattern; `- 12` / `- 24` are octave shifts.

Degree reference (D major, sounding): −14 D2 · −13 E2 · −12 F♯2 · −11 G2 · −10 A2 · −9 B2 · −8 C♯3 · −7 D3 · −6 E3 · −5 F♯3 · −4 G3 · −3 A3 · −2 B3 · −1 C♯4 · 0 D4 · 1 E4 · 2 F♯4 · 3 G4 · 4 A4 · 5 B4 · 6 C♯5 · 7 D5 · 8 E5 · 9 F♯5 · 10 G5 · 11 A5 · 12 B5 · 13 C♯6.

## Score facts

- Scoring in this movement: 2 Fl, 2 Cl in A (written in F major, a minor third above sounding), 2 Fg, Vn I, Vn II, Vla, Vc, Cb. No oboe, brass or percussion.
- D major, 4/4, ♩ = 86–92, 23 bars. Rehearsal marks: [1] bar 3, [2] bar 10, [3] bar 17. Fermata on bar 23 (all parts), with a decrescendo hairpin on the held notes.
- Systems: p1 top = bars 1–3, p1 bottom = 4–7, p2 top = 8–11, p2 bottom = 12–15, p3 top = 16–19, p3 bottom = 20–23.
- Dynamics and articulation printed: Vla `pizz. p` (bar 1), Vc `p pizz.` (bar 1), Cb pizz (bar 1, the same `p pizz.`), Fl `p` (bar 3), Vn I `mp` (bar 3), Vc `pizz. mf` (bar 14), Vn I `Vno Solo` (bars 14–16), Vla `pizz.` (bar 17), Vc `arco p` (bar 17), Fl `p` (bar 17, second flute), Cl I `pp` (bar 22). Clarinets carry no mark on the page at bar 1 (p in 2021).
- Slurs: clarinet ostinato slurred in 4-note groups; flute/violin phrases slurred across the bar; cello G2–F♯3 and E2–D3 slurred in pairs.

## Bar table — sounding pitches

"2021" shows what the 2021 code produces (sounding), "score" what the page shows (sounding). ✓ = identical. Rhythm matches unless noted.

### Clarinets (I: 2021 line 1, II: 2021 line 2; `r1blend` bars 1–9, `r2blend` 10–16, `r3blend` 17–22)

| bar | Cl I score (sounding) | Cl I 2021 degrees | Cl II score | Cl II 2021 | verdict |
|---|---|---|---|---|---|
| 1 | B3 E4 C♯4 F♯4 B3 E4 C♯4 𝄾 | −2 1 −1 2 −2 1 −1 ~ | 𝄽𝄽 𝄽 C♯4 F♯4 | `<~ …>` ~!5 −1 2 | degrees ✓ — but every clarinet note sounds **3 semitones high** because `transpose 3` lifts the sounding degrees to the written pitches |
| 2 | 𝄽𝄽 B3 E4 C♯4 F♯4 | ~ [−2 1 −1 2] | B3 E4 C♯4 F♯4 B3 𝄾 𝄽 | −2 1 −1 2 −2 ~!3 | degrees ✓ (same +3 error) |
| 3 | B3 E4 C♯4 𝄾 𝄽𝄽 | [−2 1 −1 ~] ~ | 𝄽 C♯4 F♯4 B3 E4 C♯4 F♯4 | ~!2 −1 2 −2 1 −1 2 | degrees ✓ |
| 4–9 | repeats 1–3 exactly; Cl II bars 4 and 7 begin with the B3 eighth (tail of the previous group) | `<~ -2!2>` | | | ✓ in content; alignment: see quirk Q4 |
| 10 | E4 F♯3 C♯3 F♯3 F♯4 𝄾 𝄽 (the low note hangs below the third ledger: written E3) | 1 −5 −8 −5 2 ~!3 | E4 𝄾𝄽𝄽 F♯4 G3 D3 G3 | 1 ~!3 2 −4 −7 −4 | degrees ✓ |
| 11 | G4 A3 C♯3 A3 A4 𝄾 𝄽 | 3 −3 −8 −3 4 ~!3 | G4 … A4 B3 D3 B3 | 3 ~!3 4 −2 −7 −2 | degrees ✓ |
| 12 | G4 A3 C♯3 A3 F♯4 | 3 −3 −8 −3 2 ~!3 | G4 … F♯4 G3 D3 G3 | 3 ~!3 2 −4 −7 −4 | degrees ✓ |
| 13 | E4 F♯3 C♯3 F♯3 D4 | 1 −5 −8 −5 0 ~!3 | E4 … D4 F♯3 D3 F♯3 | 1 ~!3 0 −6 −7 −6 | degrees ✓ |
| 14–16 | tacet | ~!3 | tacet | ~!3 | ✓ |
| 17–22 | ostinato as bars 1–3, in the order A B C A B C (bar 17 = A: Cl II starts with rests, bar 20 = A with the B3 eighth) | r3blend | | | content ✓; alignment Q4 |
| 22 | beat 4½: **G4 pp** (written B♭4), slurred into bar 23 | `"~@7 3"` → degree 3 = G4 ✓ … but `|+` made it silent (Q5) and `transpose 3` would have made it B♭4 | | | pitch ✓, fix Q5 + drop +3 |
| 23 | G4 whole, fermata, decrescendo | nothing (pedal only) | 𝄻 fermata | | **add: hold G4 through bar 23** |

### Flutes (two flutes on one staff; 2021 `r1focus` flute = one line + octave, `r2focus` two lines, `r3focus` two lines)

| bar | Fl I score | Fl II score | 2021 (sounding) | verdict |
|---|---|---|---|---|
| 3 | B5 whole p | B4 whole | F♯4 + F♯5 (degree 2 + [0,12]) | **2021 three scale-degrees low** |
| 4 | F♯5 A5 B5(half) | F♯4 A4 B4(half) | C♯4 E4 F♯4 (+8ve) | 3 degrees low |
| 5 | A5 G5 F♯5 A5 B5(half) | A4 G4 F♯4 A4 B4(half) | E4 D4 C♯4 E4 F♯4 (+8ve) | 3 degrees low |
| 6 | A5(half) B5(half) | A4 B4 | E4 F♯4 (+8ve) | 3 degrees low |
| 7–9 | tacet | tacet | ~!3 | ✓ |
| 10 | C♯5 E5 B4(half) | E4(half) F♯4(half) | C♯5 E5 B4 / E4 F♯4 | ✓ |
| 11 | [B4 D5] E5 B4(half) | F♯4(half) A4(half) | ✓ | ✓ |
| 12 | C♯5 E5 **[D5 C♯5] B4** | G4(half) F♯4(half) | C♯5 E5 . D5 [C♯5 B4] | pitches ✓; **rhythm: eighths D5 C♯5 then quarter B4** (2021 has quarter D5 then eighths) |
| 13 | [C♯5 D5] E5 B4(half) | E4(half) D4(half) | ✓ | ✓ |
| 14 | tacet | tacet | ~ | ✓ |
| 15 | 𝄽𝄽 A4 G4 F♯4 A4 (one flute) | — | flute 2 line: ~ . 4 3 [2@7 ~] 4 | ✓ |
| 16 | E4 [C♯4 E4] F♯4 F♯4 (one flute) | — | flute 2: 1 [−1 1] [2@7 ~] 2 | ✓ |
| 17 | B4(half) B5(half) | 𝄽 B4 dotted-half p, tied → | `[-2 5]`+12 = B4 B5; flute 2: B4 from beat 2 (`0.125 ~>`) | ✓ (the 1/8-cycle shift models the tie) |
| 18 | B4(half) B5(half) | B4♪(tie end) 𝄾 B4 dotted-half tied → | same | ✓ |
| 19 | B4(half) B5(half) | B4♪ 𝄾 𝄽 𝄽𝄽 | `[-2 5]`; flute 2 silent | ✓ |
| 20 | B5 dotted-half tied to ♪, 𝄾 | silent | `5` = B5 whole | ✓ (whole ≈ dotted-half + tied eighth) |
| 21 | **B5 half** (tenuto), 𝄽, B5 quarter → | silent | `[5 ~!2 5]` = quarter, rests, quarter | **first note is a half**: `[5@2 ~ 5]` |
| 22 | B5 whole, tied → | B4 whole, tied → | `[-2, 5]` | ✓ |
| 23 | B5 whole, fermata, decresc. | B4 whole, fermata | `~` + pedal released after beat 1 | **hold both through bar 23** |

### Violin I (`r1focus` 3–9, `r2focus` 10–16 solo, `r3focus` 17–23)

| bar | score | 2021 (sounding) | verdict |
|---|---|---|---|
| 3 | B5 whole mp | F♯5 (degree 9) | **3 degrees low** |
| 4 | F♯5 A5 B5(half) | C♯5 E5 F♯5 | 3 degrees low |
| 5 | A5 G5 F♯5 A5 B5(half) | E5 D5 C♯5 E5 F♯5 | 3 degrees low |
| 6 | A5(half) B5(half) | E5 F♯5 | 3 degrees low |
| 7 | A5 G5 F♯5 A5 · E5 [C♯5 E5] | E5 D5 C♯5 E5 . B4 [G4 B4] | 3 degrees low; rhythm note R1 |
| 8 | F♯5 F♯5 · A5 G5 F♯5 A5 | C♯5 C♯5 . E5 D5 C♯5 E5 | 3 degrees low |
| 9 | E5 [A5 E5] · F♯5(half) | B4 [E5 B4] . C♯5 | 3 degrees low |
| 10–13 | tacet | ~!4 | ✓ |
| 14 | A5 G5 F♯5 A5 · E5 [C♯5 E5] (Vno Solo) | ✓ (`+12`) | ✓ pitches; rhythm R1 |
| 15 | F♯5 F♯5 (tenuto) · A5 G5 F♯5 A5 | ✓ | ✓ |
| 16 | E5 [C♯5 E5] · F♯5 F♯5 ’ | ✓ | ✓ |
| 17 | B5(half) · F♯5 A5 | ✓ | ✓ |
| 18 | B5(half) · A5 G5 F♯5 A5 | ✓ | ✓ |
| 19 | B5(half) A5(half) | ✓ | ✓ |
| 20 | B4 whole, tied → bar 21 ♪ | `[-2]`+12 = B4, pedal models the tie | ✓ |
| 21–23 | ♪ 𝄾 𝄽 𝄻 ; tacet; tacet (fermata) | ~ | ✓ |

### Violin II

| bar | score | 2021 | verdict |
|---|---|---|---|
| 3 | B4 whole | F♯4 | **3 degrees low** (same line as the flute without the octave) |
| 4 | F♯4 A4 B4(half) | C♯4 E4 F♯4 | 3 degrees low |
| 5 | A4 G4 F♯4 A4 B4(half) | E4 D4 C♯4 E4 F♯4 | 3 degrees low |
| 6 | A4(half) B4(half) | E4 F♯4 | 3 degrees low |
| 7–14 | tacet | ~ | ✓ |
| 15 | 𝄽𝄽 · A5 G5 F♯5 A5 | `[~ . 4 3 2 4]`+12 | ✓ |
| 16 | E5 [C♯5 E5] · F♯5 F♯5 ’ | ✓ | ✓ |
| 17 | B4(half) · F♯4 A4 | `[5 . 2 4]` | ✓ |
| 18 | B4(half) · A4 G4 F♯4 A4 | ✓ | ✓ |
| 19 | B4(half) A4(half) | ✓ | ✓ |
| 20 | B4 whole, tied → | `[5]` = B4 | ✓ |
| 21 | B4 whole (tied), → bar 22 ♪ | ~ + pedal held to bar 22 beat 1 | ✓ (pedal models the two-bar tie) |
| 22–23 | ♪ 𝄾 𝄽 𝄻; tacet (fermata) | ~ | ✓ |

### Bassoons (`r2blend`, bars 14–16, parallel tenths; 2021 `- "[0,9]"`)

| bar | Fg I score | Fg II score | 2021 upper / lower | verdict |
|---|---|---|---|---|
| 14 | B3 C♯4 D4 E4 | G2 A2 B2 C♯3 | G3 A3 B3 C♯4 / E2 F♯2 G2 A2 | **both voices 2 degrees low** (the tenth is right) |
| 15 | F♯4 G4 F♯4 E4 | D3 E3 D3 C♯3 | D4 E4 D4 C♯4 / B2 C♯3 B2 A2 | 2 degrees low |
| 16 | D4 C♯4 B3 A3 | B2 A2 G2 F♯2 | B3 A3 G3 F♯3 / G2 F♯2 E2 D2 | 2 degrees low |

### Viola (pizz throughout its entries; `pizzicato` in 2021)

| bar | score | 2021 | verdict |
|---|---|---|---|
| 1–9 | 𝄽 F♯3 𝄽 D3 (pizz p) | `~ -5 ~ -7` | ✓ |
| 10–16 | tacet | (no viola line in r2blend) | ✓ |
| 17–21 | 𝄽 F♯3 𝄽 D3 (pizz) | ✓ | ✓ |
| 22 | 𝄽 F♯3 𝄽𝄽 | ~ −5 ~ −7 | **no D3 on beat 4** |
| 23 | 𝄻 fermata | — | ✓ |

### Cello

| bar | score | 2021 | verdict |
|---|---|---|---|
| 1–9 | G2–F♯3, E2–D3 (slurred pairs), **p pizz.** | `cello "p"` arco patch, pitches ✓ | pitches ✓; **articulation: pizz** (2021 used the arco instrument) |
| 10–13 | tacet | — | ✓ |
| 14 | 𝄾E3 𝄾D3 𝄾G3 𝄾F♯3 (pizz mf, off-beats) | F♯3 E3 A3 G3 | **one degree high** |
| 15 | 𝄾B3 𝄾A3 𝄾B3 𝄾F♯3 | ✓ | ✓ |
| 16 | 𝄾A3 𝄾D3 𝄾E3 𝄾B2 | ✓ | ✓ |
| 17–21 | G2–F♯3, E2–D3, **arco p** | `cello "p"` | ✓ |
| 22 | **E2–D3, C♯2–B2**, B2 tied → | G2 F♯3 E2 D3 | **different bar** (the only variant of the ostinato) |
| 23 | B2 whole, fermata, decresc. | nothing (pedal, released after beat 1) | **hold B2 through bar 23** |

### Double bass (pizz; written an octave above sounding)

| bar | score (written → sounding) | 2021 | verdict |
|---|---|---|---|
| 1–9 | G3 𝄽 E3 𝄽 → **G2 𝄽 E2 𝄽** (doubling the cello's downbeats) | A1 F♯1 (`"-3 ~ -5 ~" - 24`) | **one degree high and an octave low**: should be `"-4 ~ -6 ~" - 12` |
| 10–16 | tacet | — | ✓ |
| 17–21 | G2 𝄽 E2 𝄽 | A1 F♯1 | as above |
| 22 | E3 𝄽 C♯3 B3 → **E2 𝄽 C♯2 B2** | A1 F♯1 | different bar: `"-13 ~ -15 -9"` |
| 23 | 𝄻 fermata | — | ✓ |

## Corrections (what the port will change, each shown on the page)

Ordered by weight. Confidence: H = read at 4–6× with the clef in frame, consistent across several bars; M = read once at 3×.

1. **Clarinets sound three semitones high** (H). `transpose 3` was applied to degrees that are already the sounding pitches (score bar 1 Cl I written D4 G4 E4 A4 = sounding B3 E4 C♯4 F♯4 = degrees −2 1 −1 2). Drop the transposition in all three blends and in the bar-22 note. The A clarinet is a transposing instrument; the transcription had already transposed by writing sounding degrees, then transposed again.
2. **Flute, Vn I, Vn II bars 3–9 are three scale-degrees (a fourth) low** (H; every note of bars 3–9 checked, all three lines). The melody begins on B5/B4, not F♯. The 2021 patterns are otherwise exact in rhythm and contour, so the fix is `+ 3` on the degrees of the three `r1focus` lines (`"[2 [-1 1 . 2] …]"` → `"[5 [2 4 . 5] [4 3 2 4 . 5] [4 5] ~!3]"`, Vn I `"[12 [9 11 . 12] [11 10 9 11 . 12] [11 12] [11 10 9 11 . 8 [6 8]] [9 9 . 11 10 9 11] [8 [11 8] . 9]]"`). The `r2focus` and `r3focus` lines are right, which is why bars 14–19 in 2021 (identical figures, correct pitch) sound a fourth above bars 3–9.
3. **Bassoons bars 14–16 two degrees low, both voices** (H). Pattern becomes `"[~!4 [-2 -1 0 1] [2 3 2 1] [0 -1 -2 -3]]/7" - "[0,9]"` (tenths kept).
4. **Double bass**: G2/E2 not A1/F♯1 (H, bars 1, 4 and 17 at 6×): `"-4 ~ -6 ~" - 12`. Bar 22: E2 𝄽 C♯2 B2 (H).
5. **Cello bar 22** E2–D3, C♯2–B2 with B2 held into bar 23 (H); **bar 14** E3 D3 G3 F♯3 (H); **bars 1–9 pizz** (H, "p pizz." printed under the cello at bar 1, "arco" at bar 17).
6. **Viola bar 22**: no D3 on beat 4 (H).
7. **Bar 23 holds**: Fl I B5, Fl II B4, Cl I G4, Vc B2 through the fermata bar with the printed decrescendo (H). 2021 modelled bar-22→23 ties with sustain-pedal patterns released after beat 1; the port writes the ties as note durations instead (no CC64 needed, and the NanoSynth's damper state cannot leak).
8. **Clarinet bar 22 entry** G4 pp on the last eighth, tied into 23 (H) — the 2021 degree was right, the note never sounded (Q5).
9. **Flute bar 12 rhythm**: eighths D5 C♯5 then quarter B4 (M); **bar 21**: half B5, rest, quarter B5 (H).

R1 (open, low confidence, recommended to keep 2021): bars 7 and 14, Vn I second half. The page shows a quarter E5 followed by a beamed C♯5–E5 pair engraved with two beams (sixteenths), which only adds up if the E5 is dotted; no dot is legible at 5×. Bar 16 has the same figure engraved as quarter + eighths. 2021 uses quarter + eighths in all three places; the port keeps that unless you can see the dot on your copy.

## The port

`01_andante.tidal`: one stream (`andante f k d`), blocks as pattern functions of `k`, degrees
read against the M layer of `inKey D "major"` (degree 0 = D at the bass anchor; the 2021 register
is octave 1, `- 12` octave 0, `+ 12` octave 2, the two flutes `"[1,2]"`), divisors ×4 (one cycle a
beat), the form carrying the dynamic arc, ties as durations. Rendered through the boot it emits
the same 490 events (bar, beat, channel, note, duration) as the first port.

## Legacy quirks (code, not transcription) and how the port treats them

| id | quirk | effect in 2021 | port |
|---|---|---|---|
| Q1 | `bar 1 9 $ r1blend dyn1*0.85` parses as `r1blend dyn1 * 0.85` → `speed 0.85` | the 0.85 never scaled loudness | drop; dynamics are names plus a bar-level envelope |
| Q2 | `#vc` = ch 15 while `cello` = ch 16 (and viola/contrabass = ch 15) | the bar-22 pedal reached the viola/bass channel | ties as durations (see 7) |
| Q3 | `dyn1 = cat [lfo 0.4→0.5, lfo 0.5→0.6, 0.6 ×7]` is a 9-**cycle** (= 9-beat) loop, and `slow 9 $ lfo saw 0.9 1.3` ramps over 9 beats, repeating | a 2¼-bar swell cycle rather than a one-off crescendo | one `ramp` per phrase, in bars |
| Q4 | blocks are `slow 4 $ "…/3"` / `"…/7"` patterns; alignment depends on `seqP` starting each block from its own cycle 0 (it does, in 2021's Tidal and today's) | none — the ostinato began on its first bar at bars 1 and 17, the melodies on their first sub-bar at 3, 10 and 17, and `<~ -2!2>` gave the B3 eighth at bars 4, 7 and 20 but not 1 and 17, exactly as the score. (An earlier draft of this table claimed the opposite from querying the patterns outside `seqP`; retracted.) | `bars` is a plain `seqP` triple; nothing to fix |
| Q5 | `clarinet "pp" \|+ note (…"~@7 3")` takes structure from the left (the dynamic, a whole-cycle event) | the bar-22 note never sounded | primed instruments take rhythm from the pitch pattern |
| Q6 | `dyn2` unused; `moog`, `harp` etc. unused | — | — |
| Q7 | tempo 74 → 75 → 76 | slower than the score's 86–92 | ♩ = 88 steady; 74/75/76 kept as a commented alternative |

## Mix decisions carried over unchanged (2021 choices, not score marks)

Clarinets p/pp per section, viola mp, cello p, bass p, flute p / ppp+pp (bars 17–23), Vn I mp (3–9) p (14–16) mf (17–23), Vn II mp/p/mp, bassoon pp, cello pizz mp (14–16). Score marks differ in places (cello pizz **mf** at 14, flute **p** at 17, Vn I **mp** only); kept as the author's balance for this rig, listed here so they can be revisited at the desk.
