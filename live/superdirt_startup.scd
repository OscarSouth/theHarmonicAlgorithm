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
            ~dirt.soundLibrary.addMIDI(\thru, ~midiTHRU);

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

    // 12 Step LED feedback (optional — no error if unplugged)
    block {
      var isAvailable = MIDIClient.destinations.any { |endpoint|
        endpoint.device.containsi("12Step")
      };
      if(isAvailable) {
        try {
          ~midi12step = MIDIOut.newByName("12Step", "Port 1");
          ~midi12step.latency = 0.0;
          ~dirt.soundLibrary.addMIDI(\twelve, ~midi12step);
          "====> SUCCESS: 12 Step LED output connected.".postln;
        } {
          "====> WARNING: 12 Step detected but connection failed.".postln;
        };
      } {
        "====> INFO: 12 Step not found — LED feedback disabled.".postln;
      };
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
