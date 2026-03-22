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

// ============================================================================
// Shell Setup — add to ~/.zshrc
// ============================================================================
//
// start_qlinks() {
//   cd /Users/oscarsouth/.stack/global-project/live || return 1
//
//   echo "Waiting for SuperCollider (boot TidalCycles first)..."
//   while ! lsof -nP -iUDP:57121 2>/dev/null | grep -q UDP; do
//     sleep 2
//   done
//   sleep 1
//
//   python3 osc-send.py localhost 57121 /run-code s --file qlink-bridge.scd
//   echo "Q-Link bridge active. Monitoring encoders (Ctrl-C to stop)..."
//
//   local status_file="$HOME/Library/Application Support/SuperCollider/sc_status.line"
//   touch "$status_file"
//   trap 'echo "\nStopped."; return 0' INT TERM
//   while true; do
//     printf "\r%s" "$(cat "$status_file" 2>/dev/null)"
//     sleep 0.05
//   done
// }
//
// liveCode() {
//   cd /Users/oscarsouth/.stack/global-project/live || return 1
//
//   /Applications/Pulsar.app/Contents/MacOS/Pulsar . > /dev/null 2>&1 &
//   local pulsar_pid=$!
//
//   cleanup() { kill $pulsar_pid 2>/dev/null; wait $pulsar_pid 2>/dev/null; echo "\nStopped."; }
//   trap cleanup EXIT INT TERM
//
//   echo "Waiting for SuperCollider (boot TidalCycles in Pulsar)..."
//   while ! lsof -nP -iUDP:57121 2>/dev/null | grep -q UDP; do
//     kill -0 $pulsar_pid 2>/dev/null || { echo "Pulsar exited."; return 1; }
//     sleep 2
//   done
//   sleep 1
//
//   python3 osc-send.py localhost 57121 /run-code s --file qlink-bridge.scd
//   echo "Q-Link bridge active. Monitoring encoders (Ctrl-C to stop)..."
//
//   local status_file="$HOME/Library/Application Support/SuperCollider/sc_status.line"
//   touch "$status_file"
//   while kill -0 $pulsar_pid 2>/dev/null; do
//     printf "\r%s" "$(cat "$status_file" 2>/dev/null)"
//     sleep 0.05
//   done
// }
//
// Usage:
//   liveCode     — Opens Pulsar, waits for SC, injects bridge, monitors.
//                   Ctrl-C or closing Pulsar stops everything.
//   start_qlinks — Standalone: injects bridge into already-running SC.
// ============================================================================
