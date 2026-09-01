SafeMIDIOut {
    var <>midiOut;
    var <>noteOnCounts;
    // Optional hooks for downstream observers (e.g. LED coordinator).
    // Fired after the actual MIDI message is emitted to the device.
    // The internal precautionary noteOff inside noteOn does NOT fire onNoteOff,
    // and a noteOff suppressed by reference-counting does NOT fire onNoteOff
    // either — hooks reflect Tidal-level intent, not every wire-level emit.
    var <>onNoteOn, <>onNoteOff, <>onControl;

    *new { |midiOut|
        ^super.new.init(midiOut);
    }

    init { |mo|
        midiOut = mo;
        noteOnCounts = Dictionary.new;
    }

    noteOn { |chan, note, vel|
        var key = (chan * 128) + note;
        var count = noteOnCounts[key] ? 0;
        // Precautionary noteOff: clear any sounding note at this pitch
        midiOut.noteOff(chan, note, 0);
        // Send the actual noteOn
        midiOut.noteOn(chan, note, vel);
        // Track reference count
        noteOnCounts[key] = count + 1;
        // Notify observers of the user-intent noteOn
        onNoteOn !? { |fn| fn.value(chan, note, vel) };
    }

    noteOff { |chan, note, vel|
        var key = (chan * 128) + note;
        var count = (noteOnCounts[key] ? 0) - 1;
        if(count <= 0) {
            // Last reference — send the real noteOff
            midiOut.noteOff(chan, note, vel);
            noteOnCounts[key] = 0;
            // Notify observers only when the wire actually sees a noteOff
            onNoteOff !? { |fn| fn.value(chan, note, vel) };
        } {
            // Other patterns still hold this pitch — suppress
            noteOnCounts[key] = count;
        };
    }

    // Handle CC 123 (All Notes Off) and CC 120 (All Sound Off)
    control { |chan, ctlNum, val|
        if((ctlNum == 123) or: { ctlNum == 120 }) {
            // Reset all counts for this channel
            noteOnCounts.keysValuesDo { |key, count|
                if((key div: 128) == chan) {
                    noteOnCounts[key] = 0;
                };
            };
        };
        midiOut.control(chan, ctlNum, val);
        // Notify observers (LED coordinator cares about CC 64 / 121 / 123)
        onControl !? { |fn| fn.value(chan, ctlNum, val) };
    }

    // Forward all other MIDIOut methods transparently
    doesNotUnderstand { |selector ... args|
        ^midiOut.performList(selector, args);
    }

    // Explicit forwarding for properties accessed directly by SuperDirt
    latency { ^midiOut.latency }
    latency_ { |val| midiOut.latency = val }
    uid { ^midiOut.uid }
    port { ^midiOut.port }
}
