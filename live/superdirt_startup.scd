(
s.reboot {
  s.options.device = "PCH";
  s.options.numOutputBusChannels = 2;
  s.options.numInputBusChannels = 2;
  s.options.numBuffers = 1024;
  s.options.memSize = 8192;
  s.options.numWireBufs = 64;
  s.options.maxNodes = 1024 * 32;
  s.latency = 0.02;

  s.waitForBoot {
    ~dirt = SuperDirt(2, s);
    ~dirt.loadSoundFiles("/home/oscarsouth/dirt_samples/*");
    s.sync;
    ~dirt.start(57120, [0, 1]);

    MIDIClient.init;
    ~midiTHRU = MIDIOut.newByName("Midi Through", "Midi Through Port-0");
    ~midiTHRU.latency = 0.0;
    ~dirt.soundLibrary.addMIDI(\thru, ~midiTHRU);

    // -- CORRECTED OSC "Backdoor" --
    OSCdef.new(
      \runCode,
      { |msg|
        // The code to execute is the first argument in the message (msg[1])
        msg[1].asString.interpret;
      },
      '/run-code', nil, 57121
    );

    "SuperDirt initialized. Ready for OSC commands.".postln;
  };
};
)
