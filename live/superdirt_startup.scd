(
s.reboot {
  // --- CONFIGURATION CHANGES ---
  // 1. Explicitly set your desired sample rate.
  s.options.sampleRate = 44100;

  // 2. Use the correct device name for your Mac's built-in speakers.
  //    The error log showed it as "MacBook Pro Speakers", not "PCH".
  s.options.device = "MacBook Pro Speakers";

  // 3. Set input channels to 0. This is the simplest way to fix the
  //    sample rate mismatch error if you don't need audio input.
  s.options.numInputBusChannels = 0;

  // --- Standard Options (mostly unchanged) ---
  s.options.numOutputBusChannels = 2;
  s.options.numBuffers = 1024 * 4; // Increased for stability
  s.options.memSize = 8192 * 16;   // Increased for more samples
  s.options.numWireBufs = 64;
  s.options.maxNodes = 1024 * 32;
  s.latency = 0.03; // A slightly higher latency can prevent issues

  s.waitForBoot {
    ~dirt = SuperDirt(2, s);

    // 4. Use the correct path for samples on macOS.
    //    The '~' symbol is a shortcut for your user's home directory (/Users/oscarsouth).
    //    This path points to where the Dirt-Samples quark installs files.
    ~dirt.loadSoundFiles("~/Library/Application Support/SuperCollider/downloaded-quarks/Dirt-Samples/*");

    s.sync;
    ~dirt.start(57120, [0, 1]);

    // // MIDI setup (unchanged)
    // MIDIClient.init;
    // try {
    // // ~midiTHRU = MIDIOut.newByName("U2MIDI Pro", "U2MIDI Pro");
    // // ~midiTHRU = MIDIOut.newByName("IAC Driver", "Bus 1");
    //  ~midiTHRU = MIDIOut.newByName("iConnectAUDIO4+", "DIN");
    //  ~midiTHRU.latency = 0.0;
    //  ~dirt.soundLibrary.addMIDI(\thru, ~midiTHRU);
    // };

    // MIDI setup: Auto-fallback through priority list
    MIDIClient.init;

    ~midiFound = false;

    // Order of priority: [Device Name, Port Name]
    ~midiPriorities = [
      ["U2MIDI Pro", "U2MIDI Pro"],
      ["iConnectAUDIO4+", "DIN"],
      ["IAC Driver", "Bus 1"]
    ];

    ~midiPriorities.do { |pair|
      var devName = pair[0];
      var portName = pair[1];

      // Only try connecting if we haven't found one yet
      if(~midiFound.not) {
        try {
          // Check if the device is currently recognized by the system
          var isAvailable = MIDIClient.destinations.any { |endpoint|
            (endpoint.device == devName) && (endpoint.name == portName)
          };

          if(isAvailable) {
            ~midiTHRU = MIDIOut.newByName(devName, portName);
            ~midiTHRU.latency = 0.0;
            ~dirt.soundLibrary.addMIDI(\thru, SafeMIDIOut(~midiTHRU));

            ~midiFound = true; // Stop looking

            // Post a specific warning if we fell back to the IAC Driver
            if(devName == "IAC Driver") {
              "====> INFO: Using fallback IAC Driver. (External MIDI interfaces were not detected)".postln;
            } {
              ("====> SUCCESS: SuperDirt connected to MIDI interface: " ++ devName).postln;
            };
          };
        } {
          // Fail gracefully and silently if an unexpected error occurs
        };
      };
    };

    if(~midiFound.not) {
      "====> WARNING: No priority MIDI interfaces were found. MIDI thru is disabled.".postln;
    };

    // OSC "Backdoor" (unchanged)
    OSCdef.new(
      \runCode,
      { |msg|
        msg[1].asString.interpret;
      },
      '/run-code', nil, 57121
    );

    "SuperDirt is ready.".postln;
  };
};
)

// ============================================================================
// SafeMIDIOut — Precautionary Note Off + Reference-Counted Note Off Suppression
// ============================================================================
//
// This class wraps MIDIOut to fix MIDI note collisions when multiple
// TidalCycles patterns produce the same MIDI note simultaneously.
//
// Without this wrapper, SuperDirt schedules each event's Note Off independently.
// When two patterns overlap on the same pitch, the earlier pattern's Note Off
// kills the later pattern's still-active note. Synths that ignore duplicate
// Note Ons for already-sounding pitches compound the problem.
//
// SafeMIDIOut fixes this with two behaviors:
//   1. Precautionary Note Off before every Note On (ensures clean retrigger)
//   2. Reference-counted Note Off suppression (only the last Note Off fires)
//
// SETUP:
//   1. Copy the class below (without the leading //) to a new file:
//      ~/Library/Application Support/SuperCollider/Extensions/SafeMIDIOut.sc
//   2. Recompile SuperCollider's class library (Cmd+Shift+L or reboot)
//   3. In this startup script, use SafeMIDIOut(~midiTHRU) instead of ~midiTHRU
//      when calling ~dirt.soundLibrary.addMIDI
//
// ----------------------------------------------------------------------------
//
// SafeMIDIOut {
//     var <>midiOut;
//     var <>noteOnCounts;
//
//     *new { |midiOut|
//         ^super.new.init(midiOut);
//     }
//
//     init { |mo|
//         midiOut = mo;
//         noteOnCounts = Dictionary.new;
//     }
//
//     noteOn { |chan, note, vel|
//         var key = (chan * 128) + note;
//         var count = noteOnCounts[key] ? 0;
//         // Precautionary noteOff: clear any sounding note at this pitch
//         midiOut.noteOff(chan, note, 0);
//         // Send the actual noteOn
//         midiOut.noteOn(chan, note, vel);
//         // Track reference count
//         noteOnCounts[key] = count + 1;
//     }
//
//     noteOff { |chan, note, vel|
//         var key = (chan * 128) + note;
//         var count = (noteOnCounts[key] ? 0) - 1;
//         if(count <= 0) {
//             // Last reference — send the real noteOff
//             midiOut.noteOff(chan, note, vel);
//             noteOnCounts[key] = 0;
//         } {
//             // Other patterns still hold this pitch — suppress
//             noteOnCounts[key] = count;
//         };
//     }
//
//     // Handle CC 123 (All Notes Off) and CC 120 (All Sound Off)
//     control { |chan, ctlNum, val|
//         if((ctlNum == 123) or: { ctlNum == 120 }) {
//             // Reset all counts for this channel
//             noteOnCounts.keysValuesDo { |key, count|
//                 if((key div: 128) == chan) {
//                     noteOnCounts[key] = 0;
//                 };
//             };
//         };
//         midiOut.control(chan, ctlNum, val);
//     }
//
//     // Forward all other MIDIOut methods transparently
//     doesNotUnderstand { |selector ... args|
//         ^midiOut.performList(selector, args);
//     }
//
//     // Explicit forwarding for properties accessed directly by SuperDirt
//     latency { ^midiOut.latency }
//     latency_ { |val| midiOut.latency = val }
//     uid { ^midiOut.uid }
//     port { ^midiOut.port }
// }
//
// ============================================================================
