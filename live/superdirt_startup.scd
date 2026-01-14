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

    // MIDI setup (unchanged)
    MIDIClient.init;
    try {
//      ~midiTHRU = MIDIOut.newByName("U2MIDI Pro", "U2MIDI Pro");
//      ~midiTHRU = MIDIOut.newByName("IAC Driver", "Bus 1");
  // Select iConnectAUDIO4+ DIN output for MIDI (exact name from sclang)
  ~midiTHRU = MIDIOut.newByName("iConnectAUDIO4+", "DIN");
      ~midiTHRU.latency = 0.0;
      ~dirt.soundLibrary.addMIDI(\thru, ~midiTHRU);
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
