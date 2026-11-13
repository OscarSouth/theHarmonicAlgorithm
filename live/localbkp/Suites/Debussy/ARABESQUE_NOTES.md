# Première Arabesque — notes to the transcription

Debussy, Deux Arabesques L.66 No.1 (1888–91). Score-mode piece on the continuo channel:
`01_arabesque.tidal` beside this file.

## Sources

| source | what it is | used for |
|---|---|---|
| `~/Documents/IMSLP255354-PMLP02383-Debussy,_Claude-Deux_Arabesques_Durand_4395_filter.pdf` | Durand & Fils 1904 print, plate D.&F. 4395 (the 1891 plates re-engraved), No.1 on pp. 1–5, no text layer | the authority: every marking, the bar-67 Rit., the spot checks |
| Mutopia #1777 (`debussy_Arabesque_1.ly`, `.mid`, `-a4.pdf`), Keith OHara, LilyPond 2.12.3, public domain, source "Durand et Fils (1904)" | a setting of the same print; four voices (upper red/green, lower blue/grey), `\barNumberCheck` at 6 13 17 26 47 55 63 76 83 87 95, dynamics and hairpins in the `.ly` but stripped from the MIDI (`Dynamic_performer` removed), MIDI tempo 144 (a render tempo, not Debussy's) | the notes, by `scripts/midi2score.py` |

No free MusicXML or kern exists; piano-midi.de (a performed MIDI) was unreachable. The
interpretation is therefore the print's words and dynamics, set by hand (below).

## Harvest log

`python3 -I scripts/midi2score.py debussy_Arabesque_1.mid --blocks … --mark …`, one run per
phrase block (marks p / pp / f as printed at the block's start).

- Format 1, division 384, 3 tracks (control, upper staff, lower staff), 107 bars; 4/4 with one
  2/4 bar (94, tick 142848); range E1–G♯6.
- Four notes dropped as doubles (bars 1–2: C♯4, B3, A3, G♯3): the `.ly` writes them a second time
  in the upper voice with hidden noteheads (`\ohn`) so a slur can start on the lower staff.
- Cross-bar ties kept as one step: bars 3 (A4, C♯5), 4 (A5), 26 (A2, C♯4), 28 (F♯4), 29 (D♯4),
  31 and 33 (A4 C♯5), 38 → 39 (E5, into the first triplet of the Rubato), 67 (C5), 69 (C5, E4),
  73 (A4, C♯5), 74 (A5), 89–93 (the descending sequence), 95 (A4).
- Three bass notes lie below the continuo's floor (B1): F♯1 in bars 31 and 33, E1 in bar 37.
  Each is the bottom of an ascending arpeggio, not an octave doubling; written an octave up and
  marked `(fs1→)` / `(e1→)`. Everything else is in range (G♯1–G♯6 rendered).
- Blocks: 1–5, 6–16, 17–25, 26–46 (one block, because the E5 of 38 is tied into 39), 47–54,
  55–62, 63–70, 71–75, 76–86, 87–93.5 (the 2/4 bar makes it 30 beats; the range ends half a bar
  early or the block's start repeats for two beats), 94.5–106.5 (score bars 95–107, each half a
  bar early from here on). The form's nodes from 95 on are written at N − 0.5 likewise.

## Verification

- `stack test`: 1255 examples, 0 failures (two new `staff` cases in ScoreSpec).
- Boot render (`live/BootTidal.hs`, `p` shadowed) over 109 bars compared with the MIDI note list
  as (cycle, pitch, duration) multisets: 1444 notes each way, 0 missing, 0 extra, with the four
  doubles and the three raised notes applied to the expectation. The 1010 remaining events are
  the tempo blocks' `cps`.
- Durand vs Mutopia, read system by system from the 300 dpi renders of pp. 1–5 against the
  Mutopia PDF: bar numbering identical (the 2/4 bar is 94 in both, 107 bars); every tempo word
  and dynamic at the same bar — p 1, Rit. 5, a Tempo pp 6, poco a poco cresc. 10, Stringendo /
  sempre cresc. 13, Rit. 16, a Tempo p 17, Rit. 22 and 24, a Tempo 23 and 26, Poco mosso 29
  (Durand prints "cresc." where Mutopia draws a hairpin), p 37, Tempo rubato (un peu moins vite)
  39, sf 42, Mosso p cresc. 47, f 49, Rit. 50, Mosso 51, a Tempo p 55, Risoluto f 63, dim. molto
  67, più dim. 70, 1º Tempo p 71, Rit. 75, a Tempo 76, poco a poco cresc. 80, Stringendo 83,
  Rit. 86, a Tempo p 87, 2/4 94, dim. 95, più dim. 97, p 99, pp 101, pp and 8va 106.
  **One difference:** Durand prints **Rit.** over bar 67 (with the dim. molto, running to the
  1º Tempo at 71); Mutopia has no rit. there. The tempo map follows Durand.
- Note spot checks (Durand against the harvested text): bars 1–6 (the triplet figure, the
  crossed hands, the ties of 3–4), 17–18 (the rising figure with the hidden-head A5s sounding
  once), 26–27 (A1/A2 under the C♯4 and the E–G–A–B figure), 31–33 (the F♯1 arpeggios, raised),
  39–40 (the Rubato's E5–D5–E5–C♯5 triplet over the F♯4/A4 third), 63 (Risoluto chords), 94
  (the 2/4 bar: E4 F♯4 E4 over G♯2, B2 E3 G♯3 E3 B2), 106–107 (the 8va chords E6/G♯6 over E5/B5,
  the closing E2/E3). All agree.
- Hardware listening on the nano is the author's.

## Interpretation (the tempo map and the arc)

- ♩ = 80 for Andantino con moto; recordings sit between 72 and 88. `tempo` is the only number;
  everything else is a ratio of it.
- rit. = a one-bar ramp to 0.85 (two bars for 24–25, four for 67–70); stringendo = three bars up
  to 1.15, its rit. ramps from 1.15 down; Tempo Rubato "un peu moins vite" = 0.9 and its
  "a tempo" at 55 the same; mosso = the base; poco mosso = 1.06; Risoluto and 1º Tempo = the base.
- The printed dynamic at a phrase's start is the mark on its lines (`dyn`: p 0.40, pp 0.35,
  f 0.65). Hairpins, cresc. and dim. are the form's arc, a multiplier on the mark that ramps
  between the printed points and snaps (`rh'`) back to 1 at each new mark: the two long
  crescendi (10–16, 80–86) climb from pp to about f (×1.9); the mosso surges reach f (×1.6);
  the Risoluto's dim. molto / più dim. fall to ×0.55 by 71; the close falls through dim. and
  più dim. to pp. All starting values — tune at the desk.
- No pedal: the print has none and the Harpitz patch sustains. Voices are balanced equally; a
  `|* vel 0.8` on the inner-voice lines is the first thing to try if the melody needs lifting.
