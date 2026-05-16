// =============================================================================
// LED Coordinator — passive MIDI-observing pitch-class LED state machine
//                 + 4-character display driver for the 12 Step
// =============================================================================
//
// PURPOSE
//   Drives a 12-LED row (one CC per pitch class) on a piano-style MIDI
//   device by passively observing the audio MIDI traffic that already flows
//   through SafeMIDIOut. No Tidal-side LED code is needed: instruments emit
//   their normal note-on/note-off + sustain pedal CCs, and the coordinator
//   derives "currently sounding pitch classes" across watched channels and
//   emits CC 20 + (pc) on the LED channel.
//
//   Also drives the 12 Step's 4-character display (CC 50-53, ASCII-valued).
//   The display has NO local clock: every value is computed Tidal-side and
//   broadcast as CCs. This coordinator simply caches the latest values and
//   paints. Alignment with form-local time is automatic because Tidal's
//   computation shares the same cycle counter that drives the form's
//   interpolation.
//
// DISPLAY PROTOCOL (from 12 Step manual)
//   CC 50..53 address the four cells left-to-right.
//   Values: 48..57 = digits 0..9, 65..90 = letters A..Z, 58..64 = symbols/blank.
//   Same channel as the LEDs (midichan 9 = MIDI ch 10).
//
// TIDAL → SC SIGNALS (added by `display k` helper in BootTidal.hs)
//   CC 113 on ch 10: bar number 1..8 at chord onset, value 0 at +1/8 cycle (off).
//   CC 114 on ch 10: form loop length high byte (kLoopSecs DIV 128).
//   CC 115 on ch 10: form loop length low byte  (kLoopSecs MOD 128).
//   CC 117 on ch 10: current form-local seconds high byte (sampled 30x/cycle).
//   CC 118 on ch 10: current form-local seconds low byte  (sampled 30x/cycle).
//   Atemporal forms (single-node iK, lK) send kLoopSecs = 0 → counter blank.
//
// HOW IT IS WIRED
//   ~ledCoordinator.init(~midiOut)               — pass the SafeMIDIOut wrapper
//   ~midiOut.onNoteOn  = { |c, n, v| ~ledCoordinator.onNoteOn(c, n, v) }
//   ~midiOut.onNoteOff = { |c, n, v| ~ledCoordinator.onNoteOff(c, n, v) }
//   ~midiOut.onControl = { |c, ct, v| ~ledCoordinator.onControl(c, ct, v) }
//   ~ledCoordinator.watch(7, nil)                — ch 8: lineHarmony (all notes)
//   ~ledCoordinator.watch(9, (36..47))           — ch 10: subKick range only
//
// LED CONVENTION
//   Output is on the LED-device channel (default midichan 9 = MIDI ch 10).
//   pc 0 (C) → CC 20, pc 1 (C#) → CC 21, ..., pc 11 (B) → CC 31.
//   Value 1 = lit, 0 = unlit.
//
// PER-CHANNEL STATE (LED side)
//   notesHeld   — notes that received note-on but not (yet) a "true release"
//   sustained   — notes that received note-off while pedal was down
//                 (released by pedal-up)
//   pedalDown   — Bool, last seen CC 64 state (>= 64 means down)
//   lastT       — wall-clock of last observed event (for the watchdog)
//   range       — nil for all notes, else IntegerArray of allowed MIDI notes
//
// DISPLAY STATE MACHINE
//   \idle    — pre-launch; nothing painted.
//   \active  — display painting normally (bar flash + counter, or just bar for
//              atemporal forms).
//   \hushing — "HUSH" blink for hushAnimSecs, then back to \active.
//   Transitions:
//     \idle → \active     on any display CC (113/114/115/117/118).
//     \active → \hushing  on CC 123.
//     \hushing → \active  after hushAnimSecs (via SystemClock.sched).
//
// CLEANUP PATHWAYS (LED side, unchanged)
//   - Normal note-off, pedal release, CC 123, CC 121, watchdog, ~ledCoordinator.panic.
// =============================================================================

(
// Re-running this script reassigns ~ledCoordinator to a fresh Event. Stop any
// background Routines on the previous instance first so they don't keep
// emitting CCs from a closure over the orphaned state.
~ledCoordinator !? { |old|
    old.watchdogTask !? { |t| t.stop };
};

~ledCoordinator = (
    channels:   nil,                   // chan -> Event of state (filled in init)
    litState:   nil,                   // pc -> last-sent value (0 or 1)
    midiOut:    nil,                   // raw MIDIOut (unwrapped from SafeMIDIOut)
    ledChannel: 9,                     // emit LEDs on midichan 9 (= MIDI ch 10)
    watchdogTimeout: 2.0,              // seconds before stranded notesHeld is cleared
    watchdogTask: nil,

    // --- Display state (4-char screen on the 12 Step) ---
    displayChannel:    9,              // CC 50..53 routed via midichan 9
    displayLoopSecs:   0,              // 14-bit combined loop length; 0 = atemporal
    displayLoopHigh:   0,              // last CC 114 value
    displayLoopLow:    0,              // last CC 115 value
    displayElapsedSecs: 0,             // 14-bit combined current form-local seconds
    displayElapsedHigh: 0,             // last CC 117 value
    displayElapsedLow:  0,             // last CC 118 value
    displayLastSecondInt: -1,          // last integer-second value force-painted into P2..P4
    displayBarLit:     0,              // 0 = P1 blank, 1..8 = digit shown
    displayState:      nil,            // last-sent CC values per cell (-1 = unknown)
    displayBlank:      64,             // best-guess blank char (manual: 58..64 = "symbols/blank")
    displayMode:       \idle,          // \idle | \active | \hushing
    hushAnimSecs:      0.6,            // duration of the "HUSH" blink

    init: { |self, midiOut|
        // Emit LED CCs via the raw MIDIOut underneath any SafeMIDIOut wrapper,
        // so our own CC traffic does NOT loop back through onControl and
        // trigger a refresh-emit feedback cascade.
        self.midiOut      = if (midiOut.respondsTo(\midiOut)) { midiOut.midiOut } { midiOut };
        self.channels     = Dictionary.new;
        self.litState     = Array.fill(12, 0);
        self.displayState = Array.fill(4, -1);
        self.startWatchdog;
        self
    },

    // Declare a channel to track. range = nil for all MIDI notes,
    // else an IntegerArray (e.g. (36..47)) of allowed notes.
    watch: { |self, chan, range|
        self.channels[chan] = (
            notesHeld:  Set.new,
            sustained:  Set.new,
            pedalDown:  false,
            lastT:      Main.elapsedTime,
            range:      range
        );
        self
    },

    inRange: { |self, chan, num|
        var st = self.channels[chan];
        st.notNil and: { st.range.isNil or: { st.range.includes(num) } }
    },

    onNoteOn: { |self, chan, num, vel|
        if (self.inRange(chan, num)) {
            var st = self.channels[chan];
            st.lastT = Main.elapsedTime;
            if (vel > 0) {
                st.notesHeld.add(num);
                st.sustained.remove(num);
            } {
                // noteOn with vel = 0 is conventionally a noteOff
                self.releaseNote(chan, num);
            };
            self.refresh;
        };
    },

    onNoteOff: { |self, chan, num, vel|
        if (self.inRange(chan, num)) {
            self.releaseNote(chan, num);
            self.channels[chan].lastT = Main.elapsedTime;
            self.refresh;
        };
    },

    releaseNote: { |self, chan, num|
        var st = self.channels[chan];
        if (st.pedalDown) {
            // Pedal is down — note transitions from "held" to "sustained"
            if (st.notesHeld.includes(num)) {
                st.notesHeld.remove(num);
                st.sustained.add(num);
            };
        } {
            st.notesHeld.remove(num);
            st.sustained.remove(num);
        };
    },

    onControl: { |self, chan, ctl, val|
        var st;
        var changed = false;

        // Display-protocol CCs are addressed only on the display channel.
        if (chan == self.displayChannel) {
            case
                { ctl == 113 } { self.handleBarCC(val) }
                { ctl == 114 } { self.handleLoopHigh(val) }
                { ctl == 115 } { self.handleLoopLow(val) }
                { ctl == 117 } { self.handleElapsedHigh(val) }
                { ctl == 118 } { self.handleElapsedLow(val) };
        };

        // Per-channel watched note/pedal state machine (unchanged).
        st = self.channels[chan];
        if (st.notNil) {
            st.lastT = Main.elapsedTime;
            case
                { ctl == 64 } {
                    if (val >= 64) {
                        if (st.pedalDown.not) { changed = true };
                        st.pedalDown = true;
                    } {
                        if (st.pedalDown or: { st.sustained.size > 0 }) { changed = true };
                        st.pedalDown = false;
                        st.sustained.clear;
                    };
                }
                { ctl == 123 } {
                    if ((st.notesHeld.size + st.sustained.size) > 0) { changed = true };
                    st.notesHeld.clear;
                    st.sustained.clear;
                }
                { ctl == 121 } {
                    if (st.pedalDown or: { (st.notesHeld.size + st.sustained.size) > 0 }) {
                        changed = true;
                    };
                    st.notesHeld.clear;
                    st.sustained.clear;
                    st.pedalDown = false;
                };
            if (changed) { self.refresh };
        };

        // CC 123 (broadcast on every channel by `hush`/`allNotesOff`) triggers
        // the HUSH blink. Idempotent across 16-channel broadcast — only the
        // first one triggers since subsequent ones see mode = \hushing.
        if (ctl == 123 and: { self.displayMode == \active }) { self.beginHushing };
    },

    // --- Display protocol handlers -------------------------------------------
    // Any display CC moves \idle → \active on first arrival. Each handler
    // updates cached state and re-renders.

    enterActiveIfIdle: { |self|
        if (self.displayMode == \idle) { self.displayMode = \active };
    },

    handleBarCC: { |self, val|
        var bar = val.asInteger.clip(0, 8);
        self.displayBarLit = bar;
        self.enterActiveIfIdle;
        if (self.displayMode == \active) { self.renderRunning };
    },

    handleLoopHigh: { |self, val|
        self.displayLoopHigh = val.asInteger;
        self.displayLoopSecs = (self.displayLoopHigh * 128) + self.displayLoopLow;
        self.enterActiveIfIdle;
        if (self.displayMode == \active) { self.renderRunning };
    },

    handleLoopLow: { |self, val|
        self.displayLoopLow = val.asInteger;
        self.displayLoopSecs = (self.displayLoopHigh * 128) + self.displayLoopLow;
        self.enterActiveIfIdle;
        if (self.displayMode == \active) { self.renderRunning };
    },

    handleElapsedHigh: { |self, val|
        self.displayElapsedHigh = val.asInteger;
        self.displayElapsedSecs = (self.displayElapsedHigh * 128) + self.displayElapsedLow;
        self.enterActiveIfIdle;
        if (self.displayMode == \active) { self.renderRunning };
    },

    handleElapsedLow: { |self, val|
        self.displayElapsedLow = val.asInteger;
        self.displayElapsedSecs = (self.displayElapsedHigh * 128) + self.displayElapsedLow;
        self.enterActiveIfIdle;
        if (self.displayMode == \active) { self.renderRunning };
    },

    beginHushing: { |self|
        self.displayMode = \hushing;
        self.renderHushing;
        SystemClock.sched(self.hushAnimSecs, {
            if (self.displayMode == \hushing) {
                self.displayMode    = \active;
                self.displayBarLit  = 0;     // P1 blank between flashes
                self.renderRunning;
            };
            nil;                              // do not reschedule
        });
    },

    // Project current note state across all watched channels into a Set of pcs.
    litPcs: { |self|
        var pcs = Set.new;
        self.channels.do { |st|
            st.notesHeld.do { |n| pcs.add(n % 12) };
            st.sustained.do { |n| pcs.add(n % 12) };
        };
        pcs
    },

    // Diff-only LED emit: only send CCs that change from last state.
    refresh: { |self|
        var lit = self.litPcs;
        12.do { |pc|
            var want = if (lit.includes(pc)) { 1 } { 0 };
            if (self.litState.at(pc) != want) {
                self.midiOut.control(self.ledChannel, 20 + pc, want);
                self.litState.put(pc, want);
            };
        };
    },

    // Hard reset: clear all LEDs, lift the pedal, blank the display, drop state.
    panic: { |self|
        12.do { |pc|
            self.midiOut.control(self.ledChannel, 20 + pc, 0);
            self.litState.put(pc, 0);
        };
        self.midiOut.control(self.ledChannel, 64, 0);
        self.channels.do { |st|
            st.notesHeld.clear;
            st.sustained.clear;
            st.pedalDown = false;
        };
        self.paintCells([self.displayBlank, self.displayBlank, self.displayBlank, self.displayBlank]);
        self.displayMode   = \idle;
        self.displayBarLit = 0;
    },

    // --- Display rendering ---------------------------------------------------

    // Diff-only paint of the four display cells.
    paintCells: { |self, ccs|
        ccs.do { |v, i|
            if (self.displayState.at(i) != v) {
                self.midiOut.control(self.displayChannel, 50 + i, v);
                self.displayState.put(i, v);
            };
        };
    },

    // Render from cached values. No SC clock: Tidal owns the time.
    // P1 = bar digit when displayBarLit > 0, else blank.
    // P2-P4 = elapsed-seconds counter, right-aligned, expanding leftwise.
    //         When the counter overflows into 4 digits, P1 is the thousands
    //         digit and the bar is suppressed.
    renderRunning: { |self|
        var blank   = self.displayBlank;
        var bar     = self.displayBarLit;
        var loop    = self.displayLoopSecs;
        var elapsed = self.displayElapsedSecs.asInteger.clip(0, 9999);
        var p1Bar   = if (bar > 0) { 48 + bar } { blank };
        var d       = { |n| 48 + n };
        var ccs;
        // Force-refresh counter cells (P2..P4) once per integer-second tick.
        // External display functions can write CCs 51..53 directly; without
        // this, the diff cache would suppress re-emitting an unchanged digit
        // and the stale character would linger. Invalidating the cache at
        // each tick costs ~3 extra MIDI CCs per wall-clock second (trivial).
        if (loop > 0 and: { elapsed != self.displayLastSecondInt }) {
            self.displayState.put(1, -1);
            self.displayState.put(2, -1);
            self.displayState.put(3, -1);
            self.displayLastSecondInt = elapsed;
        };
        case
            { loop <= 0 }      { ccs = [p1Bar, blank, blank, blank] }              // atemporal
            { elapsed < 10 }   { ccs = [p1Bar, blank, blank, d.(elapsed)] }
            { elapsed < 100 }  { ccs = [p1Bar, blank, d.(elapsed div: 10), d.(elapsed % 10)] }
            { elapsed < 1000 } { ccs = [p1Bar, d.(elapsed div: 100), d.((elapsed div: 10) % 10), d.(elapsed % 10)] }
            { true }           { ccs = [d.(elapsed div: 1000),
                                        d.((elapsed div: 100) % 10),
                                        d.((elapsed div: 10) % 10),
                                        d.(elapsed % 10)] };
        self.paintCells(ccs);
    },

    // Paint "HUSH" across all four cells.
    renderHushing: { |self|
        self.paintCells([72, 85, 83, 72]);   // H U S H
    },

    // Watchdog: clear stranded notesHeld on idle channels. Never touches
    // sustained — pedal-held notes are intentionally ringing in the synth
    // and must survive arbitrary idle stretches.
    startWatchdog: { |self|
        if (self.watchdogTask.notNil) { self.watchdogTask.stop };
        self.watchdogTask = Routine({
            loop {
                var now = Main.elapsedTime;
                self.channels.do { |st|
                    if ((now - st.lastT) > self.watchdogTimeout
                        and: { st.notesHeld.size > 0 }) {
                        st.notesHeld.clear;
                        self.refresh;
                    };
                };
                0.5.wait;
            };
        }).play(AppClock);
    }
);

"~ledCoordinator loaded.".postln;
)
