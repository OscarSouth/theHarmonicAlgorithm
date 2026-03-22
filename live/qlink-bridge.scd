(
	~statusFilePath = Platform.userAppSupportDir ++ "/sc_status.line";
	~maxVal = 127*2;
	~osc = NetAddr.new("127.0.0.1", 6010);

	// --- INITIALIZATION: 4 encoders, 7 new absolute CCs ---
	~qlinkValues =
		(~maxVal / 2).asInteger.dup(4) ++ // 4 Encoders starting at midpoint
		Array.fill(7, 64);                // 7 Absolute CCs starting at midpoint (64)

	// This function remains UNCHANGED. It only monitors the first 4 values.
	~updateStatusLine = {
		var statusFile, scaledValues, formattedStrings, finalString;

		scaledValues = ~qlinkValues[0..3].collect { |val| (val / ~maxVal).round(0.01) };
		
		formattedStrings = scaledValues.collect { |val|
			var str = val.asString;
			str = str ++ "0";
			str = str.keep(4);
			str;
		};
		
		finalString = formattedStrings.join("\t");

		statusFile = File(~statusFilePath, "w");
		if(statusFile.isOpen) {
			statusFile.write(finalString);
			statusFile.close;
		};
	};

	~updateStatusLine.value;
	MIDIIn.connectAll;

	// --- NEW: Logic to handle both encoders and absolute CCs ---
	MIDIFunc.cc({ |val, num, chan, src|
		var index, oscValue;
		// Listen for all CCs from 100 to 110
		if (chan == 0 and: { (100..110).includes(num) }) {
			index = num - 100;
			
			// If it's a relative encoder (index 0-3)
			if (index < 4) {
				if (val == 127) { ~qlinkValues[index] = ~qlinkValues[index] + 1 };
				if (val == 0)   { ~qlinkValues[index] = ~qlinkValues[index] - 1 };
				~qlinkValues[index] = ~qlinkValues[index].clip(0, ~maxVal);
				oscValue = ~qlinkValues[index] / ~maxVal;
			} {
				// If it's an absolute CC (index 4-10)
				~qlinkValues[index] = val; // Store the direct value (0-127)
				oscValue = val / 127.0;    // Scale it to 0.0-1.0 for Tidal
			};
			
			// Update the display (only shows encoders) and send the OSC message
			~updateStatusLine.value;
			~osc.sendMsg("/ctrl", num.asString, oscValue);
		};
	});

	"✅ Qlink Bridge with Absolute CCs is active.".postln;
)
