(
// This file's own directory, captured at evaluation time: nowExecutingPath is
// nil inside the async s.reboot closure below, so it cannot be read there.
// Keeps paths under live/ machine-independent. The fallback literal is the
// author's own clone and applies only if nowExecutingPath is nil.
~thaLiveDir = thisProcess.nowExecutingPath !? { |pth| pth.dirname } ?? { "/home/os/theHarmonicAlgorithm/live" };
~thaLiveDir.postln;

// MIDI-only rig by default; see the sample-loading block below.
~thaLoadSamples = false;

s.reboot {
  // --- CONFIGURATION CHANGES ---
  // 1. Explicitly set your desired sample rate.
  // Under JACK the server dictates the rate and a mismatch fails the boot.
  // PipeWire serves JACK here and is force-clocked to 48000
  // (clock.force-rate / clock.allowed-rates).
  s.options.sampleRate = 48000;

  // 2. Audio output device — uncomment ONE (comment the others).
  //    AIRA devices appear over USB when connected; both run at 44.1 kHz.
  // Left unset on Linux: s.options.device names a JACK *server* there, not an
  // audio device, and ServerOptions.devices raises "Primitive
  // '_ListAudioDevices' failed" because enumeration is CoreAudio/PortAudio only.
  // Unset connects to the default server; routing belongs in the patchbay.
  // s.options.device = "MacBook Pro Speakers";  // macOS rig
  // s.options.device = "P-6";
  // s.options.device = "S-1";

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
    // MIDI-only rig: Tidal never touches audio hardware, so Dirt-Samples costs
    // ~444 MB of RAM and ~40 s of boot for banks that are never played. Set
    // ~thaLoadSamples = true above s.reboot for sample-based work.
    //
    // Platform.userAppSupportDir resolves per platform:
    //   macOS -> ~/Library/Application Support/SuperCollider
    //   Linux -> ~/.local/share/SuperCollider
    // A literal path loads zero samples on the other platform and warns "no
    // folders found in paths", which reads as a missing quark.
    if(~thaLoadSamples == true) {
      ~dirt.loadSoundFiles(Platform.userAppSupportDir +/+ "downloaded-quarks/Dirt-Samples/*");
    } {
      "====> samples skipped (MIDI-only rig). ~thaLoadSamples = true to load.".postln;
    };

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
    // Two different philosophies, one per platform.
    //
    // NOTE: thisProcess.platform.name -- NOT Platform.name, which returns the
    // string "Platform" (the class name) and compares false against \linux.
    ~midiPriorities = if (thisProcess.platform.name == \linux) {
      // LINUX: bind the kernel's always-present virtual port and let the
      // patchbay route it onward to hardware. Deliberately hardware-agnostic --
      // swap or replug an interface and nothing here changes.
      //
      // A direct device binding is not stable on Linux: the PipeWire node name
      // embeds the USB bus path ("CME Pro U2MIDI Pro at usb-0000:00:14-0-3-3-
      // full speed") and changes whenever the device enumerates on a different
      // socket.
      //
      // MIDI therefore reaches hardware only if something routes Midi Through --
      // see live/local/midi-autoconnect, started by live/system/bin/livecode. No
      // fallback entries: Midi Through always exists, so nothing below it is
      // reachable.
      [ ["Midi Through", "Midi Through Port-0"] ]
    } {
      // macOS: CoreMIDI has no always-present virtual port unless the IAC
      // Driver is enabled, so bind the device itself, by priority.
      [ ["U2MIDI Pro", "U2MIDI Pro"],
        ["iConnectAUDIO4+", "DIN"],
        ["P-6", "MIDI IN"],
        ["S-1", "MIDI IN"],
        ["IAC Driver", "Bus 1"] ]
    };

    ~midiPriorities.do { |pair|
      var devName = pair[0];
      var portName = pair[1];

      // Only try connecting if we haven't found one yet
      if(~midiFound.not) {
        try {
          // Exact (device, port) match first -- preserves macOS behaviour.
          var chosen = MIDIClient.destinations.detect { |endpoint|
            (endpoint.device == devName) && (endpoint.name == portName)
          };

          // Port names are platform-specific: ALSA appends " MIDI n"
          // ("U2MIDI Pro MIDI 1" vs CoreMIDI's "U2MIDI Pro"), and the AIRA units
          // differ again ("P-6 MIDI 1" vs "MIDI IN"). Match on the DEVICE, which
          // is the stable identity; the port name only disambiguates. An
          // exact-pair-only match binds a lower-priority device while still
          // reporting success.
          chosen = chosen ?? { MIDIClient.destinations.detect { |endpoint|
            endpoint.device == devName
          }};

          if(chosen.notNil) {
            ~midiTHRU = MIDIOut.newByName(chosen.device, chosen.name);
            ~midiTHRU.latency = 0.0;
            // Keep a handle on the SafeMIDIOut wrapper so the LED coordinator
            // can register noteOn/noteOff/control hooks on it later.
            ~midiSafe = SafeMIDIOut(~midiTHRU);
            ~dirt.soundLibrary.addMIDI(\thru, ~midiSafe);

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

    // ----------------------------------------------------------------------
    // LED coordinator: passive pitch-class LED state machine.
    // Loads system/led-coordinator.scd, hooks into the SafeMIDIOut wrapper
    // (~midiSafe), and watches the bass-instrument channels. See that file's
    // header for design rationale.
    // ----------------------------------------------------------------------
    if(~midiFound and: { ~midiSafe.notNil }) {
      this.executeFile(~thaLiveDir +/+ "system" +/+ "led-coordinator.scd");
      ~ledCoordinator.init(~midiSafe);
      ~midiSafe.onNoteOn  = { |c, n, v| ~ledCoordinator.onNoteOn(c, n, v) };
      ~midiSafe.onNoteOff = { |c, n, v| ~ledCoordinator.onNoteOff(c, n, v) };
      ~midiSafe.onControl = { |c, ct, v| ~ledCoordinator.onControl(c, ct, v) };
      ~ledCoordinator.watch(7, nil);          // ch 8  = lineHarmony, all notes
      ~ledCoordinator.watch(9, (36..47));     // ch 10 = subKick range only
      "====> LED coordinator wired to SafeMIDIOut on ch 8 + ch 10 (subKick range).".postln;
    };

    "SuperDirt is ready.".postln;
    File(Platform.userAppSupportDir ++ "/sc_ready", "w").write(Date.getDate.stamp).close;
  };
};
)

// ============================================================================
// SafeMIDIOut
// ============================================================================
//
// Precautionary Note Off + reference-counted Note Off suppression, plus the
// onNoteOn / onNoteOff / onControl observer hooks that led-coordinator.scd
// binds to. SuperDirt schedules each event's Note Off independently, so two
// overlapping patterns on the same pitch cut each other short; this fixes that.
//
// The class lives at live/system/SafeMIDIOut.sc.
//
// INSTALL BY SYMLINK, never a copy. A copy diverges from the repository
// silently: if this class lacks the onNoteOn / onNoteOff / onControl hooks that
// led-coordinator.scd binds to, the startup aborts before writing sc_ready and
// the launcher waits indefinitely with no error printed.
//
//   scripts/install-linux.sh does this for you. By hand:
//     ln -sfn "$PWD/live/system/SafeMIDIOut.sc" "<Platform.userExtensionDir>/SafeMIDIOut.sc"
//   where Platform.userExtensionDir is
//     macOS  ~/Library/Application Support/SuperCollider/Extensions
//     Linux  ~/.local/share/SuperCollider/Extensions
//
// Then recompile the class library (Ctrl+Shift+L, or restart sclang).
// ============================================================================
