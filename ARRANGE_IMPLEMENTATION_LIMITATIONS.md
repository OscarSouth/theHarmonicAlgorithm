# Arrange Implementation Limitations

## rep < 1 not supported (legacy limitation)

- `slow (4 * rep)` with rep < 1 reduces chord slot duration and produces a perceived tempo change
  relative to other instruments using `arrange` with the same rep
- The `fast rep` inside compensation still maintains constant note density, but the phrase becomes shorter
- `state.tidal` currently uses `rep = 0.5` — this is a known degraded state, not a crash, but a known quirk
- Recommended rep values for correct behaviour: positive integers 1, 2, 4, 8

## k909 hi-hat `[0 1]/4` anomaly

- `hh "[0 1]/4"` via `ur 1 pat ps fs` (BootTidal.hs, `bars=1`) produces an extra hi-hat on cycle 3 of 4
- Expected: hit only at cycle 2 (position 1 of `[0 1]` stretched over 4 cycles)
- Actual: `[0 0 1 1]/4` — hits at cycles 2 and 3
- Cause: `ur 1` with `bars=1` does not correctly honour the `/4` duration modifier in the meta-pattern
- Workaround: `slow 4 $ hh "[0 1]"` or a direct MIDI struct pattern
- Not fixable in Groove.hs scope; would require changes to BootTidal.hs or live file

## autoOff shift is approximate at rep < 1

- For rep >= 1: `maxDur / rep <= maxDur < 1`, clamp never triggers, shift = `maxDur/rep` exactly
- Absolute outer note duration = `(maxDur/rep) × rep = maxDur` cycles (constant, rep-independent ✓)
- For rep < 1: shift `maxDur/rep > maxDur` may exceed 1 cat-item cycle; clamp `(1-1/128)` prevents
  wrap-around but constrains the effective note duration
