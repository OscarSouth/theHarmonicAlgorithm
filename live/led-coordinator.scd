// =============================================================================
// LED Coordinator — passive MIDI-observing pitch-class LED state machine
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
// PER-CHANNEL STATE
//   notesHeld   — notes that received note-on but not (yet) a "true release"
//   sustained   — notes that received note-off while pedal was down
//                 (released by pedal-up)
//   pedalDown   — Bool, last seen CC 64 state (>= 64 means down)
//   lastT       — wall-clock of last observed event (for the watchdog)
//   range       — nil for all notes, else IntegerArray of allowed MIDI notes
//
// "Currently sounding" on a channel = notesHeld ∪ sustained.
// LED projection across all watched channels = ⋃ { n mod 12 | n in either set }.
//
// CLEANUP PATHWAYS
//   - Normal note-off arrives                            → state updates, LEDs follow
//   - Pedal release (CC 64 < 64)                         → sustained set clears
//   - CC 123 (All Notes Off) on a channel                → both sets clear on that ch
//   - CC 121 (Reset All Controllers)                     → also clears pedalDown
//   - Watchdog: idle channel with held (non-pedal) notes → notesHeld cleared
//                                                          (sustained ALWAYS preserved
//                                                           — it represents a real
//                                                           pedal-held synth state)
//   - ~ledCoordinator.panic                              → all CCs 20–31 = 0,
//                                                          CC 64 = 0, all state cleared
// =============================================================================

(
~ledCoordinator = (
    channels:   nil,                   // chan -> Event of state (filled in init)
    litState:   nil,                   // pc -> last-sent value (0 or 1)
    midiOut:    nil,                   // SafeMIDIOut wrapper
    ledChannel: 9,                     // emit LEDs on midichan 9 (= MIDI ch 10)
    watchdogTimeout: 2.0,              // seconds before stranded notesHeld is cleared
    watchdogTask: nil,

    init: { |self, midiOut|
        // Emit LED CCs via the raw MIDIOut underneath any SafeMIDIOut wrapper,
        // so our own CC traffic does NOT loop back through onControl and
        // trigger a refresh-emit feedback cascade.
        self.midiOut  = if (midiOut.respondsTo(\midiOut)) { midiOut.midiOut } { midiOut };
        self.channels = Dictionary.new;
        self.litState = Array.fill(12, 0);
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
        var st = self.channels[chan];
        var changed = false;
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
                    // All Notes Off
                    if ((st.notesHeld.size + st.sustained.size) > 0) { changed = true };
                    st.notesHeld.clear;
                    st.sustained.clear;
                }
                { ctl == 121 } {
                    // Reset All Controllers — also drop pedal latch
                    if (st.pedalDown or: { (st.notesHeld.size + st.sustained.size) > 0 }) {
                        changed = true;
                    };
                    st.notesHeld.clear;
                    st.sustained.clear;
                    st.pedalDown = false;
                };
            // Only refresh when state actually mutated. Avoids needless work
            // for incoming CCs that aren't part of the LED protocol (CC 20-31
            // arriving as our own emissions before we routed around them, etc).
            if (changed) { self.refresh };
        };
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
    // Uses explicit .at/.put on the Array to avoid any Event-syntax
    // ambiguity around `self.litState[pc] = want` chains.
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

    // Hard reset: clear all LEDs, lift the pedal, drop all state.
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
