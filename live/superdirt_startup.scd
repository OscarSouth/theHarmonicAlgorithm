var hydraAddr = NetAddr.new("127.0.0.1", 3333);
var tidalAddr = NetAddr.new("127.0.0.1", 6010);
OSCFunc.newMatching({ |msg, time, addr, port|
	var latency = time - Main.elapsedTime;
	msg = msg ++ ["time", time, "latency", latency];
	msg.postln;
	hydraAddr.sendBundle(latency, msg)
}, '/dirt/play', tidalAddr);

(

s.reboot {

  s.options.device = "PCH";
	s.options.numOutputBusChannels = 4;
	s.options.numInputBusChannels = 4;
	s.options.numBuffers = 1024 * 256;
	s.options.memSize = 8192 * 16;
  // s.options.memSize = 1024*500;
	s.options.numWireBufs = 65;
	s.options.maxNodes = 1024 * 32;

	s.waitForBoot {
		~dirt = SuperDirt(2, s);
		~dirt.loadSoundFiles("/home/oscarsouth/dirt_samples/*");
		// ~dirt.loadSoundFiles();
		s.sync;
		~dirt.start(57120, [2]);
		~latency = 0.0;
		MIDIClient.init;
		MIDIClient.destinations;

    ~midiTHRU = MIDIOut.newByName("Midi Through", "Midi Through Port-0");
		~midiTHRU.latency = ~latency;
		~dirt.soundLibrary.addMIDI(\thru, ~midiTHRU);
	};

	s.latency = 0.0;

};
);



(
  (
    SynthDef(\pitch, {
      |
      out,
      n,
      channel,
      stepsPerOctave = 12,
      rate = 1,
      delta,
      begin,
      end,
      portamento = 0 |
      var slew, env, phase;
      n = n + (stepsPerOctave / 12);
      n = (n + (n / stepsPerOctave) * (1 / (stepsPerOctave + 1)) / 10);
      slew = (portamento);
      slew = (slew + (slew / stepsPerOctave) * (1 / (stepsPerOctave + 1)) / 10);
      env = Env([n, n + slew], [delta / rate]);
      phase = Line.ar(begin, end, delta / rate);
      OffsetOut.ar(channel, IEnvGen.ar(env, phase));
    }).add
  );

	/*
  (
    SynthDef(\gate, {
      |
      out,
      channel,
      amp,
      n |
      n = n * 5;
      OffsetOut.ar(channel, DC.ar(n) * amp);
    }).add
  );
	*/

  (
    SynthDef(\voltage, {
      |
      out,
      channel,
      n,
      rate = 1,
      delta,
      begin,
      amp = 1,
      end,
      portamento = 0 |
      var slew, env, phase;
      n = n * 5;
      slew = (portamento);
      rate = rate;
      env = Env([n, n + slew], [delta / rate]);
      phase = Line.ar(begin, end, delta / rate);
      OffsetOut.ar(channel, EnvGen.ar(env, phase) * amp);
    }).add
  );

	/*
  (
    SynthDef(\saw, {
      |
      out,
      channel,
      amp = 1,
      rate = 0.5 |
      OffsetOut.ar(channel, abs(Saw.ar(rate)) * amp);
    }).add;
  );

  (
    SynthDef(\lfo, {
      |
      out,
      channel,
      amp = 1,
      rate = 0.5 |
      OffsetOut.ar(channel, K2A.ar(abs(SinOsc.ar(rate, 0)) * amp));
    }).add;
  );

  (
    SynthDef(\ar, {
      |
      out,
      channel,
      attack = 0.01,
      decay = 0.25,
      sustain = 0.25,
      release = 0.25,
      begin,
      amp = 1,
      end |
      var env, phase;
      env = Env.adsr(attack, decay, sustain, release);
      phase = Line.ar(begin, end, sustain);
      OffsetOut.ar(channel, IEnvGen.ar(env, phase) * amp);
    }).add
  );

  (
    SynthDef(\nPitch, {
      | out,
      channel = 0,
      freq = 440,
      portamento = 0 |
      var n = Lag.ar(log2(K2A.ar(freq)/440), portamento);
      var sig = LinLin.ar(n, -1, 9, 0, 1);
      OffsetOut.ar(channel, [sig]);
    }).add
  );

   (
    SynthDef(\nGate, {
      | out,
      channel = 0,
      n,
      portamento = 0 |
      var sig = LinLin.ar(n, -1, 9, 0, 1);
      OffsetOut.ar(channel, [sig]);
    }).add
  );
	*/

)
