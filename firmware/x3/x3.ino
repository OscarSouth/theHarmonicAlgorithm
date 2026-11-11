/*
  MiDiNiNjA X3 -- Harmonic Algorithm build.
  Part of theHarmonicAlgorithm (GPL-3.0, see LICENSE).

  The MiDiNiNjA X3 hardware is Matt Todman's design, and this firmware began as a
  fork of his MiDiNiNjA_X3_V1_CLEAN_MUTE (github.com/submodify/MiDiNiNjA). The DIN
  port instances, the LED blink state machine and the button polling are his; the
  pad-mute idea is his too. See NOTICES.md.

  The X3 sits between the laptop, the JV-1010 and the MPC (USB host) and does three jobs:

    gateway  splits the laptop's stream between the JV-1010 (DIN) and the MPC (USB), merges
             the MPC's own output into the JV's feed, with momentary pad-mutes
    looper   Tidal's clock, the RC-10R lane, Tidal's drum lane and the MPC's drum pads -> Boss RC-10R
    mapper   a played keyboard locked to the harmony the laptop is holding

  DIN                                      USB (MIDIx4)
    In A   laptop stream, TO_MPC    ->       1 Main     (ch 7 and 10, the MPC's programs; control lanes
                                                        removed, muted note-ons dropped)
    In A   laptop stream, TO_JV     ->       Out A      (every channel but 10, straight to the JV-1010;
                                                        muted note-ons dropped)
    Out A  JV-1010 and laptop       <-       1 Main     (human input from the MPC, except pad-mute notes
                                                        0-15; note-on velocities on Out A floored to
                                                        JV_VEL_FLOOR-127 for the JV-1010)
    In B   poly keyboard, omni      ->       1 Main     (mapped, input's channel, merged with the
                                                        laptop stream) and Out B
                                    <-       4 Mapper   (a second mapper input, from the MPC)
    Out B  mapped stream, input's channel (not cabled)
    In A   ch 10 CC 33-48           ->       Out C      (the drum lane, straight to the RC-10R kit; USB Drums is idle)
    Out C  RC-10R                   <-       3 Looper   (channel messages; notes 19-34 become the
                                                        RC-10R kit's notes, drumNote[])
                                    <-       In A ch 10 CC 70-84 (the RC-10R lane), CC 33-48 (the drum lane)
                                             and In A clock
    In C   unused

  CONTROL LANES on In A, channel 10, consumed here and never sent to the MPC:

    STATE  notes 108-119 (pitch class = note mod 12), held for the length of a chord,
           name the pitch classes the mapper locks to (any number of them). Sent by
           Tidal's `kmap`. CC 120/123 on channel 10 (hush) clears the set.
    RC-10R CC 70-83 go to Out C on RC_CONTROL_CHANNEL unchanged; CC 84 (value 1-16)
           becomes program change value-1 on RC_NOTE_CHANNEL (the rhythm kit). Sent
           by Tidal's Devices.RC10R.
    DRUMS  CC 33-48 = pad 0-15, value = velocity: a note-on and an immediate note-off
           for the kit's note (drumNote[pad]) on RC_NOTE_CHANNEL, straight to Out C,
           DIN to DIN. The ch 10 pad-mute and the button's hold silence it. The MPC's dkit track keeps its pads (notes 19-34 on USB
           Looper, translated on the way to Out C).
           Sent by Tidal's `dkit` (x3rc10).

  PAD-MUTES: note-on 0-15 from the MPC on the Main port, any channel, mutes laptop
  channel 1-16 until the matching note-off. Muted channels lose note-ons only;
  note-offs always pass, so nothing hangs.

  MAPPER INPUTS: DIN In B and USB Mapper (from the MPC) are mapped alike, each with
  its own key table. The mapped stream goes to USB Main and Out B only.

  IN B CONTROLS (the JV-1010 controller, ported from its Mozaic script). Nothing from
  In B is forwarded raw: notes go through the mapper, and so does every control that
  is not interpreted below (sustain, other CCs, bend, pressure travel with the mapped
  notes). Interpreted:
    CC 114, not ch 10/16  steps that channel's JV program (63/62 down 1/2, 65/66 up 1/2,
                          clamped 1-128): bank select first if a bank key is held, then
                          program change, to the JV on that channel (Out A). Channel 16
                          is the JV's Perform Ctrl CH, so it is not stepped.
    CC 30/31/86/87        bank keys, any channel: held (127) selects bank 84/0, 84/1,
                          84/2, 84/3 for the next step.
    CC 115 = 127, ch 10   selects the JV performance (as the button).
    CC 20/21/22/24/27,    MMC to Out A and USB Main: stop / locate-zero + play / record
      ch 10, = 127        strobe / locate-zero / punch in (CC 27 = 0: punch out).

  BUTTON (PUSH ME):
    press          selects the JV-1010's orchestral performance (USER 1: bank 80/0,
                   program 1) on JV_PERF_CHANNEL out of DIN Out A. The JV's Perform
                   Ctrl CH is set to 16 because the U6 filters channel 10 from the JV.
    hold (1 s)     panic: clears pad-mutes, releases mapped notes, sends all-notes-off,
                   sustain-off and expression 0 (CC 11) on 16 channels to Out A, Out B
                   and the MPC (USB Main), and presses the RC-10R's LOOP STOP and
                   RHY STOP. While held, only clock and transport pass, so every device
                   stays in sync; the LED stays lit and the state lane keeps tracking.
                   Release sends all-notes-off and expression 127 (every part's default,
                   so the performances' CC 7 levels are untouched) and resumes the flow.
                   What was stopped stays stopped until Tidal or the pedal starts it.

  CLOCK: Tidal's clock is the master. Its ticks reach In A scattered by the hops they
  share with the music, so the X3 re-clocks them (see CLOCK below) and sends the
  even ticks to the MPC (USB Main) and the RC-10R (Out C) in parallel. Start, stop,
  continue and song position pass In A -> USB Main untouched; the RC-10R takes
  Timing Clock only. Realtime from the MPC on USB Looper is dropped: its regenerated
  clock bunches and skips ticks, and the pedal time-stretches its loops to chase it.
  Measured at 87 bpm (tick interval sd): Tidal 0.13 ms, arriving at In A 2.5 ms,
  re-clocked 0.10 ms, the MPC's own clock 2.7 ms. Each device has one clock source.
  SysEx does not pass the gateway.
*/

#include <MIDI.h>
#include <OneButton.h>

#define CABLE_MAIN 0
#define CABLE_DRUMS 1
#define CABLE_LOOPER 2
#define CABLE_MAPPER 3

#define STATE_CHANNEL 10
#define STATE_LOW 108   // state lane: notes 108-119 on STATE_CHANNEL
#define RC_CC_LOW 70    // RC-10R lane: CC 70-83 forwarded, CC 84 = rhythm kit
#define RC_CC_KIT 84
#define RC_CONTROL_CHANNEL 1
#define RC_NOTE_CHANNEL 2
#define DRUM_CC_LOW 33  // drum lane: CC 33-48 on STATE_CHANNEL = pads 0-15
#define DRUM_PADS 16
// The MPC dkit pads' notes on USB Looper, 19-34: the free run between the ch 10 kit
// (0-15, and `count` at 7 + chord index) and the MPC sub's silence pad (35), so
// an All Ports ch 10 track never plays them. The dkit track's pads A01-A16 send
// 19-34 directly (track Transpose 0).
#define DRUM_NOTE_LOW 19
#define STATE_GRACE_MS 20  // a chord change can land its note-offs before its note-ons

#define BUTTONPIN 12
#define HOLD_MS 1000

#define JV_PERF_CHANNEL 16  // the JV-1010's Perform Ctrl CH
#define JV_PERF_MSB 80      // USER performance group
#define JV_PERF_LSB 0
#define JV_PERF_PROGRAM 0   // performance 1

// Out A reaches only the JV-1010 and the laptop (through the U6), so every note the
// JV plays passes here. Its orchestral sounds stutter on sustained notes at low
// velocity (no documented cause), so note-on velocity 1-127 is rescaled to
// JV_VEL_FLOOR-127. Velocity 0 (note-off) is unchanged.
#define JV_VEL_FLOOR 42

// The current orchestral rig's routing of the laptop's channels (In A), the same
// pattern as the studio's U6: Tidal reaches the JV-1010 straight through Out A,
// and the MPC gets only what its own programs play, so the orchestra never
// crosses USB. Ch 7 (continuo) goes to both: the MPC's grnd program plays it when
// the JV is off and stays muted otherwise. Bit n = channel n + 1.
#define TO_JV  0xFDFF  // every channel but 10
#define TO_MPC 0x0240  // ch 7 and 10 (sub, kit, count, 12 Step)
#define LEDPIN 13
#define BLINKRATE 150

// The RC-10R kit's note for each pad, 0-15 (Roland's RC-10R MIDI implementation;
// the same map in all 16 kits), in the order of the rig's shared 16-pad drum map:
//   kick, clap, hat closed, hat open, ride, crash, side stick, snare;
//   ride 2, crash 2, claves, snare 2, pedal hat, ride bell, floor tom, kick 2.
const uint8_t drumNote[DRUM_PADS] = {36, 39, 42, 46, 51, 49, 37, 38,
                                     59, 57, 75, 40, 44, 53, 41, 35};

MIDI_CREATE_INSTANCE(HardwareSerial, Serial5, MIDI_A);
MIDI_CREATE_INSTANCE(HardwareSerial, Serial4, MIDI_B);
MIDI_CREATE_INSTANCE(HardwareSerial, Serial3, MIDI_C);

// A DIN write blocks the whole loop -- clock, drum lane and all -- once its port's
// transmit buffer is full, and a byte takes 0.32 ms to leave. The core's buffer is
// 40 bytes; each output gets 128 more, so the largest burst (the hold's 144 bytes
// of all-notes-off, sustain-off and expression per port) is queued without waiting.
uint8_t outBuffer[3][128];

elapsedMillis ledOnMillis;
elapsedMillis buttonMillis;
elapsedMillis stateEmptyMillis;

enum ledStates { MONITOR,
                 BLINK_ENABLE,
                 LED_OFF,
                 LED_ON };
enum ledStates ledState = MONITOR;

uint8_t blinkCount = 0;
bool midiActivity = false;

OneButton button;

bool chanMute[16];
bool holding = false;  // long press held: every path but clock and transport is closed

// In B program stepping: the JV's current program per channel, and the bank keys.
uint8_t jvProgram[16];
bool bankKey[4];  // CC 30, 31, 86, 87 held

uint16_t statePcs = 0;      // bit n = pitch class n held on the state lane
uint16_t lastStatePcs = 0;  // last non-empty set, used during the grace window

// Mapper inputs. Output keeps the input's channel. Keys are tracked per source and
// channel, so one key held on two sources or two channels is two keys; output
// refcounts are per channel. Indices: channel 0-15 = MIDI channel 1-16.
#define SRC_DIN_B 0
#define SRC_USB_MAPPER 1
int8_t keyToOut[2][16][128];  // input note -> mapped note sounding for it, -1 = none
uint8_t outCount[16][128];    // keys currently sounding each mapped note

inline void blinkLED() __attribute__((always_inline));
inline void blink(uint8_t) __attribute__((always_inline));
inline void checkButton() __attribute__((always_inline));

void setup() {
  pinMode(LEDPIN, OUTPUT);
  button.setup(BUTTONPIN, false, true);  // no pullup, active low
  button.setPressMs(HOLD_MS);
  button.attachClick(selectPerformance);
  button.attachLongPressStart(holdStart);
  button.attachLongPressStop(holdEnd);
  memset(keyToOut, -1, sizeof(keyToOut));
  memset(outCount, 0, sizeof(outCount));
  MIDI_A.begin(MIDI_CHANNEL_OMNI);
  MIDI_B.begin(MIDI_CHANNEL_OMNI);
  MIDI_C.begin(MIDI_CHANNEL_OMNI);
  Serial5.addMemoryForWrite(outBuffer[0], sizeof(outBuffer[0]));
  Serial4.addMemoryForWrite(outBuffer[1], sizeof(outBuffer[1]));
  Serial3.addMemoryForWrite(outBuffer[2], sizeof(outBuffer[2]));
  MIDI_A.turnThruOff();
  MIDI_B.turnThruOff();
  MIDI_C.turnThruOff();
  resetAll();
}

void loop() {
  if (MIDI_A.read()) readLaptop();
  if (MIDI_B.read()) readKeys();
  MIDI_C.read();  // drained: In C carries nothing yet
  if (usbMIDI.read()) readUSB();
  clockService();
  usbMIDI.send_now();
  blinkLED();
  checkButton();
}

// ##### GATEWAY: DIN In A -> Out A (JV-1010) and USB Main (MPC) #####

void readLaptop() {
  byte type = MIDI_A.getType();
  byte channel = MIDI_A.getChannel();
  byte data1 = MIDI_A.getData1();
  byte data2 = MIDI_A.getData2();
  if (!channel) {  // the laptop's clock and transport go to the MPC, held or not; SysEx does not
    switch (type) {
      case midi::Clock:
        clockReceived();  // re-clocked to the MPC and the RC-10R by clockService()
#if defined(USB_MIDI4_SERIAL)
        clockStats();
#endif
        break;
      case midi::Start:
      case midi::Continue:
      case midi::Stop:
        usbMIDI.sendRealTime(type, CABLE_MAIN);
        break;
      case midi::SongPosition:
        usbMIDI.sendSongPosition(data1 | (data2 << 7), CABLE_MAIN);
        break;
      default:
        break;
    }
    return;
  }

  if (channel == STATE_CHANNEL) {
    if ((type == midi::NoteOn || type == midi::NoteOff) && data1 >= STATE_LOW && data1 < STATE_LOW + 12) {
      setStatePc(data1 % 12, type == midi::NoteOn);
      return;
    }
    if (type == midi::ControlChange && data1 >= RC_CC_LOW && data1 <= RC_CC_KIT) {
      if (holding) return;
      rcPress(data1, data2);  // held for the pedal's next tick (see CLOCK)
      midiActivity = true;
      return;
    }
    if (type == midi::ControlChange && data1 >= DRUM_CC_LOW && data1 < DRUM_CC_LOW + DRUM_PADS) {
      if (holding) return;
      if (data2 && !chanMute[STATE_CHANNEL - 1]) {
        MIDI_C.sendNoteOn(drumNote[data1 - DRUM_CC_LOW], data2, RC_NOTE_CHANNEL);
        MIDI_C.sendNoteOff(drumNote[data1 - DRUM_CC_LOW], 0, RC_NOTE_CHANNEL);
      }
      midiActivity = true;
      return;
    }
    if (type == midi::ControlChange && (data1 == 120 || data1 == 123)) clearState();
  }

  if (holding) return;
  if (type == midi::NoteOn && chanMute[channel - 1]) return;
  uint16_t bit = 1 << (channel - 1);
  if (TO_JV & bit) toOutA((midi::MidiType)type, data1, data2, channel);
  if (TO_MPC & bit) usbMIDI.send(type, data1, data2, channel, CABLE_MAIN);
  midiActivity = true;
}

// Everything bound for the JV-1010 leaves Out A here, from the laptop (TO_JV) and
// from the MPC (human input, USB Main), with note-on velocity floored.
void toOutA(midi::MidiType type, byte data1, byte data2, byte channel) {
  if (type == midi::NoteOn && data2 > 0)
    data2 = JV_VEL_FLOOR + ((data2 - 1) * (127 - JV_VEL_FLOOR) + 63) / 126;
  MIDI_A.send(type, data1, data2, channel);
}

// ##### CLOCK: Tidal's clock, re-clocked #####
//
// Tidal's ticks leave the laptop evenly (interval sd 0.08 ms) but reach In A
// scattered by the USB-MIDI and DIN hops they share with the music: measured at
// 87 bpm, interval sd 2.5 ms and arrivals spread over 10-13 ms within 4 beats.
// Queueing only ever delays a tick, so the earliest arrivals mark the true grid.
// The X3 fits the tick period over the last CLOCK_WINDOW arrivals, takes their
// lower edge as the phase, and eases the running schedule 10% of the way to that
// fit on each tick (the edge steps when its earliest tick leaves the window).
// Output stays within one tick of the input count, so no tick is lost or added.
// Until the window fills, and after input pauses for 4 periods, ticks pass
// straight through.
//
// Two lanes leave the schedule:
//   MPC (USB Main)    on the schedule.
//   RC-10R (Out C)    pedalLag() later. The pedal has no start/stop/position, so
//                     its beat is set by when a start press arrives; lag costs it
//                     nothing. rc10 presses (In A ch 10 CC 70-84) share the music's
//                     path and arrive up to ~13 ms late on busy downbeats, so each
//                     is held and released RC_PRESS_LEAD before the pedal's next
//                     tick: every start lands at the same offset from a tick. A
//                     lag just under a tick admits presses up to ~25 ms late at
//                     87 bpm. The pedal takes Timing Clock only; its transport is
//                     the rc10 lane.
#define CLOCK_WINDOW 96      // 4 beats
#define RC_PRESS_LEAD 1000   // us before the pedal's tick that a held press goes out
#define RC_QUEUE 16

uint32_t clockIn[CLOCK_WINDOW];  // arrival times (us), indexed by tick count mod window
uint32_t clockCount = 0;         // ticks received since the clock (re)started
uint32_t clockSent = 0;          // ticks emitted to the MPC since then
uint32_t pedalSent = 0;          // ticks emitted to the pedal since then
double clockPeriod = 0;          // fitted tick period (us); 0 until the window fills
uint32_t clockBase = 0;          // scheduled time of tick clockBaseIndex (us)
uint32_t clockBaseIndex = 0;

struct RcPress { uint8_t cc, value; uint32_t arrived; };
RcPress rcQueue[RC_QUEUE];
uint8_t rcHead = 0, rcTail = 0;

// The pedal lane's lag behind the MPC's: a tick less 2 ms, never under half a tick.
double pedalLag() {
  return max(clockPeriod / 2, clockPeriod - 2000);
}

uint32_t scheduled(uint32_t tick) {
  return clockBase + (int32_t)(((double)(int32_t)(tick - clockBaseIndex)) * clockPeriod);
}

void rcSend(uint8_t cc, uint8_t value) {
  if (cc < RC_CC_KIT) MIDI_C.sendControlChange(cc, value, RC_CONTROL_CHANNEL);
  else if (value >= 1 && value <= 16) MIDI_C.sendProgramChange(value - 1, RC_NOTE_CHANNEL);
}

// An rc10 press from In A: held for the pedal's next tick while the clock runs.
void rcPress(uint8_t cc, uint8_t value) {
  uint8_t next = (rcTail + 1) % RC_QUEUE;
  if (clockPeriod <= 0 || next == rcHead) {
    rcSend(cc, value);
    return;
  }
  rcQueue[rcTail] = {cc, value, micros()};
  rcTail = next;
}

void rcFlush() {
  while (rcHead != rcTail) {
#if defined(USB_MIDI4_SERIAL)
    Serial.printf("press cc %u held %lu us\n", rcQueue[rcHead].cc, micros() - rcQueue[rcHead].arrived);
#endif
    rcSend(rcQueue[rcHead].cc, rcQueue[rcHead].value);
    rcHead = (rcHead + 1) % RC_QUEUE;
  }
}

void rcClear() {
  rcHead = rcTail;
}

void emitMain() {
  usbMIDI.sendRealTime(midi::Clock, CABLE_MAIN);
  clockSent++;
}

void emitPedal() {
  MIDI_C.sendRealTime(midi::Clock);
  pedalSent++;
}

void clockRestart() {
  rcFlush();
  clockCount = 0;
  clockSent = 0;
  pedalSent = 0;
  clockPeriod = 0;
}

void clockReceived() {
  uint32_t now = micros();
  if (clockCount) {
    uint32_t gap = now - clockIn[(clockCount - 1) % CLOCK_WINDOW];
    if (gap > (clockPeriod > 0 ? 4 * clockPeriod : 500000)) clockRestart();
  }
  clockIn[clockCount % CLOCK_WINDOW] = now;
  clockCount++;
  if (clockCount < CLOCK_WINDOW) {  // filling the window: pass through
    emitMain();
    emitPedal();
    return;
  }
  // Least-squares period over the window, then the lower edge of the arrivals.
  uint32_t first = clockCount - CLOCK_WINDOW;
  uint32_t t0 = clockIn[first % CLOCK_WINDOW];
  double sx = 0, sy = 0, sxx = 0, sxy = 0;
  for (uint32_t i = 0; i < CLOCK_WINDOW; i++) {
    double y = clockIn[(first + i) % CLOCK_WINDOW] - t0;
    sx += i; sy += y; sxx += (double)i * i; sxy += i * y;
  }
  double slope = (CLOCK_WINDOW * sxy - sx * sy) / (CLOCK_WINDOW * sxx - sx * sx);
  double low = 1e18;
  for (uint32_t i = 0; i < CLOCK_WINDOW; i++) {
    double r = (double)(clockIn[(first + i) % CLOCK_WINDOW] - t0) - i * slope;
    if (r < low) low = r;
  }
  uint32_t target = t0 + (int32_t)(low + (clockSent - first) * slope);  // the fit's time for the next tick
  if (clockPeriod <= 0) {
    clockBase = target;
  } else {
    uint32_t current = scheduled(clockSent);
    clockBase = current + (int32_t)(0.1 * (int32_t)(target - current));
  }
  clockBaseIndex = clockSent;
  clockPeriod = slope;
}

// Called every loop: emits each lane's next tick, and the held presses just
// before the pedal's, when they fall due.
void clockService() {
  if (clockPeriod <= 0) return;
  uint32_t now = micros();
  if (now - clockIn[(clockCount - 1) % CLOCK_WINDOW] > 4 * clockPeriod) {
    clockRestart();  // input stopped
    return;
  }
  // MPC lane: within one tick of the input count.
  if (clockSent + 1 < clockCount) emitMain();  // more than one behind: catch up
  else if (clockSent <= clockCount && (int32_t)(now - scheduled(clockSent)) >= 0) emitMain();
  // Pedal lane: the same schedule, pedalLag() later; it may trail the input by a tick.
  uint32_t pedalDue = scheduled(pedalSent) + (int32_t)pedalLag();
  if (pedalSent + 2 < clockCount) {  // more than a tick behind its lag: catch up
    rcFlush();
    emitPedal();
  } else if (pedalSent <= clockCount) {
    if (rcHead != rcTail && (int32_t)(now - (pedalDue - RC_PRESS_LEAD)) >= 0) rcFlush();
    if ((int32_t)(now - pedalDue) >= 0) emitPedal();
  }
}

#if defined(USB_MIDI4_SERIAL)
// Debug build: the In A clock's timing as the X3 receives it, every 96 ticks (4
// beats): tick interval mean / sd / min / max, and the spread of arrivals about a
// straight line through the window (how late the queued ticks land), in us.
void clockStats() {
  static uint32_t t[96];
  static uint8_t n = 0;
  t[n++] = micros();
  if (n < 96) return;
  n = 0;
  double mean = (double)(t[95] - t[0]) / 95, sumsq = 0, lo = 1e9, hi = 0, eLo = 1e9, eHi = -1e9;
  for (uint8_t i = 1; i < 96; i++) {
    double iv = t[i] - t[i - 1];
    sumsq += (iv - mean) * (iv - mean);
    if (iv < lo) lo = iv;
    if (iv > hi) hi = iv;
  }
  for (uint8_t i = 0; i < 96; i++) {
    double e = (double)(t[i] - t[0]) - i * mean;
    if (e < eLo) eLo = e;
    if (e > eHi) eHi = e;
  }
  Serial.printf("clock us: mean %.0f sd %.0f min %.0f max %.0f  phase spread %.0f\n",
                mean, sqrt(sumsq / 95), lo, hi, eHi - eLo);
}
#endif

// ##### STATE LANE #####

void setStatePc(uint8_t pc, bool on) {
  if (on) statePcs |= (1 << pc);
  else statePcs &= ~(1 << pc);
  if (statePcs) lastStatePcs = statePcs;
  else stateEmptyMillis = 0;
#if defined(USB_MIDI4_SERIAL)
  Serial.print("state ");
  for (uint8_t i = 0; i < 12; i++)
    if (statePcs & (1 << i)) { Serial.print(i); Serial.print(' '); }
  Serial.println();
#endif
}

void clearState() {
  statePcs = 0;
  lastStatePcs = 0;
}

// The set the mapper locks to: the held set, or the last one while a chord change
// is still landing.
uint16_t activePcs() {
  if (statePcs) return statePcs;
  if (stateEmptyMillis < STATE_GRACE_MS) return lastStatePcs;
  return 0;
}

// ##### MAPPER: DIN In B or USB Mapper -> USB Main + DIN Out B #####

// Nearest note whose pitch class is in pcs; ties resolve downward. -1 if pcs is empty.
int16_t nearestInSet(uint8_t note, uint16_t pcs) {
  if (!pcs) return -1;
  for (int16_t d = 0; d < 12; d++) {
    int16_t lo = note - d, hi = note + d;
    if (lo >= 0 && (pcs & (1 << (lo % 12)))) return lo;
    if (hi <= 127 && (pcs & (1 << (hi % 12)))) return hi;
  }
  return -1;
}

void sendMapped(midi::MidiType type, byte channel, byte data1, byte data2) {
  usbMIDI.send(type, data1, data2, channel, CABLE_MAIN);
  MIDI_B.send(type, data1, data2, channel);
}

void releaseKey(uint8_t src, byte channel, uint8_t key) {
  uint8_t c = channel - 1;
  int8_t out = keyToOut[src][c][key];
  if (out < 0) return;
  keyToOut[src][c][key] = -1;
  if (outCount[c][out] && --outCount[c][out] == 0) sendMapped(midi::NoteOff, channel, out, 0);
}

void mapInput(uint8_t src, byte type, byte channel, byte data1, byte data2) {
  uint8_t c = channel - 1;
  switch (type) {
    case midi::NoteOn: {
      releaseKey(src, channel, data1);  // a repeated note-on without its note-off
      int16_t out = nearestInSet(data1, activePcs());
      if (out < 0) return;  // no harmony held: silent
      keyToOut[src][c][data1] = out;
      outCount[c][out]++;
      sendMapped(midi::NoteOn, channel, out, data2);  // retriggers a shared pitch
      break;
    }
    case midi::NoteOff:
      releaseKey(src, channel, data1);
      break;
    case midi::AfterTouchPoly:
      if (keyToOut[src][c][data1] >= 0) sendMapped(midi::AfterTouchPoly, channel, keyToOut[src][c][data1], data2);
      break;
    default:
      sendMapped((midi::MidiType)type, channel, data1, data2);  // pedal, CCs, bend, pressure
      break;
  }
  midiActivity = true;
}

void readKeys() {
  byte type = MIDI_B.getType();
  byte channel = MIDI_B.getChannel();
  byte data1 = MIDI_B.getData1();
  byte data2 = MIDI_B.getData2();
  if (!channel || holding) return;

  if (type == midi::ControlChange) {
    bool consumed = (channel == 10) ? controlChannel10(data1, data2)
                                    : controlJV(channel, data1, data2);
    if (consumed) return;
  }
  mapInput(SRC_DIN_B, type, channel, data1, data2);
}

// MIDI Machine Control (universal real-time SysEx, all devices) to the laptop (Out A)
// and the MPC (USB Main).
void sendMMC(uint8_t command) {
  const uint8_t msg[6] = { 0xF0, 0x7F, 0x7F, 0x06, command, 0xF7 };
  MIDI_A.sendSysEx(sizeof(msg), msg, true);
  usbMIDI.sendSysEx(sizeof(msg), msg, true, CABLE_MAIN);
}

void sendMMCLocateZero() {
  const uint8_t msg[13] = { 0xF0, 0x7F, 0x7F, 0x06, 0x44, 0x06, 0x01, 0, 0, 0, 0, 0, 0xF7 };
  MIDI_A.sendSysEx(sizeof(msg), msg, true);
  usbMIDI.sendSysEx(sizeof(msg), msg, true, CABLE_MAIN);
}

// The controller's channel 10 functions. True when the message is consumed.
bool controlChannel10(uint8_t cc, uint8_t value) {
  if (bankKeyUpdate(cc, value)) return true;
  switch (cc) {
    case 115: if (value == 127) selectPerformance(); return true;
    case 20:  if (value == 127) sendMMC(0x01); return true;                       // stop
    case 21:  if (value == 127) { sendMMCLocateZero(); sendMMC(0x03); } return true;  // locate zero, deferred play
    case 22:  if (value == 127) sendMMC(0x06); return true;                       // record strobe
    case 24:  if (value == 127) sendMMCLocateZero(); return true;                 // reset
    case 27:  sendMMC(value == 127 ? 0x06 : 0x07); return true;                   // punch in / out
    default:  return false;
  }
}

// Bank keys count from any channel: the controller sends them on channel 10.
// True when cc is a bank key.
bool bankKeyUpdate(uint8_t cc, uint8_t value) {
  static const uint8_t bankCC[4] = { 30, 31, 86, 87 };
  for (uint8_t i = 0; i < 4; i++)
    if (cc == bankCC[i]) {
      bankKey[i] = (value == 127);
      return true;
    }
  return false;
}

// The JV controls on channels other than 10. True when the message is consumed.
bool controlJV(uint8_t channel, uint8_t cc, uint8_t value) {
  if (bankKeyUpdate(cc, value)) return true;
  if (cc == 115) return true;
  if (cc != 114) return false;
  if (channel == JV_PERF_CHANNEL) return true;  // a program change here would switch performances

  int16_t step = 0;
  switch (value) {
    case 62: step = -2; break;
    case 63: step = -1; break;
    case 65: step = 1;  break;
    case 66: step = 2;  break;
    default: return true;
  }
  uint8_t c = channel - 1;
  jvProgram[c] = (uint8_t)constrain((int16_t)jvProgram[c] + step, 0, 127);
  for (uint8_t i = 0; i < 4; i++)
    if (bankKey[i]) {
      MIDI_A.sendControlChange(0, 84, channel);
      MIDI_A.sendControlChange(32, i, channel);
      break;
    }
  MIDI_A.sendProgramChange(jvProgram[c], channel);
  midiActivity = true;
  return true;
}

// ##### USB -> DIN #####

void readUSB() {
  byte type = usbMIDI.getType();
  byte channel = usbMIDI.getChannel();
  byte data1 = usbMIDI.getData1();
  byte data2 = usbMIDI.getData2();
  byte cable = usbMIDI.getCable();
  midi::MidiType mtype = (midi::MidiType)type;

  switch (cable) {
    case CABLE_MAIN:
      if (!channel) return;
      if (data1 <= 15 && type == midi::NoteOn) {
        chanMute[data1] = true;
      } else if (data1 <= 15 && type == midi::NoteOff) {
        chanMute[data1] = false;
      } else if (!holding) {
        toOutA(mtype, data1, data2, channel);
      }
      midiActivity = true;
      break;

    case CABLE_LOOPER:
      if (channel) {
        if (holding) return;
        if ((type == midi::NoteOn || type == midi::NoteOff)
            && data1 >= DRUM_NOTE_LOW && data1 < DRUM_NOTE_LOW + DRUM_PADS)
          data1 = drumNote[data1 - DRUM_NOTE_LOW];  // a dkit pad becomes the kit's note
        MIDI_C.send(mtype, data1, data2, channel);
      } else {
        return;  // the pedal's clock comes from In A (see CLOCK)
      }
      midiActivity = true;
      break;

    case CABLE_MAPPER:
      if (channel && !holding) mapInput(SRC_USB_MAPPER, type, channel, data1, data2);
      break;

    default:
      break;
  }
}

// ##### BUTTON / LED #####

// Pad-mutes cleared and every mapped note released.
void releaseAll() {
  memset(chanMute, false, sizeof(chanMute));
  for (uint8_t channel = 1; channel <= 16; channel++)
    for (uint8_t key = 0; key < 128; key++) {
      releaseKey(SRC_DIN_B, channel, key);
      releaseKey(SRC_USB_MAPPER, channel, key);
    }
}

void resetAll() {
  releaseAll();
  clearState();
  blink(2);
}

// All-notes-off and sustain-off on every channel, to the JV and laptop (Out A),
// the MPC (USB Main) and the mapper's DIN output (Out B).
void allNotesOff() {
  for (uint8_t channel = 1; channel <= 16; channel++) {
    MIDI_A.sendControlChange(123, 0, channel);
    MIDI_A.sendControlChange(64, 0, channel);
    MIDI_B.sendControlChange(123, 0, channel);
    MIDI_B.sendControlChange(64, 0, channel);
    usbMIDI.sendControlChange(123, 0, channel, CABLE_MAIN);
    usbMIDI.sendControlChange(64, 0, channel, CABLE_MAIN);
  }
  usbMIDI.send_now();
}

// Expression on every channel, to the same places as allNotesOff.
void expressionAll(byte value) {
  for (uint8_t channel = 1; channel <= 16; channel++) {
    MIDI_A.sendControlChange(11, value, channel);
    MIDI_B.sendControlChange(11, value, channel);
    usbMIDI.sendControlChange(11, value, channel, CABLE_MAIN);
  }
  usbMIDI.send_now();
}

// The RC-10R's LOOP STOP (CC 74) and RHY STOP (CC 78), pressed and released as the
// rc10 lane does, observing the pedal's STOP MODE.
void stopLooper() {
  for (byte cc : {74, 78}) {
    MIDI_C.sendControlChange(cc, 127, RC_CONTROL_CHANNEL);
    MIDI_C.sendControlChange(cc, 0, RC_CONTROL_CHANNEL);
  }
}

void selectPerformance() {
  MIDI_A.sendControlChange(0, JV_PERF_MSB, JV_PERF_CHANNEL);
  MIDI_A.sendControlChange(32, JV_PERF_LSB, JV_PERF_CHANNEL);
  MIDI_A.sendProgramChange(JV_PERF_PROGRAM, JV_PERF_CHANNEL);
  blink(1);
}

void holdStart() {
  holding = true;
  releaseAll();
  allNotesOff();
  expressionAll(0);
  rcClear();  // held rc10 presses don't follow the stop
  stopLooper();
}

void holdEnd() {
  allNotesOff();
  expressionAll(127);
  holding = false;
  blink(2);
}

void blinkLED() {
  if (holding) {
    digitalWriteFast(LEDPIN, HIGH);
    return;
  }
  switch (ledState) {
    case MONITOR:
      if (midiActivity) {
        digitalWriteFast(LEDPIN, HIGH);
        ledOnMillis = 0;
        midiActivity = false;
      }
      if (ledOnMillis > 15) {
        digitalWriteFast(LEDPIN, LOW);
      }
      break;
    case BLINK_ENABLE:
      digitalWriteFast(LEDPIN, LOW);
      ledOnMillis = 0;
      ledState = LED_OFF;
      break;
    case LED_OFF:
      if (blinkCount) {
        if (ledOnMillis > BLINKRATE) {
          digitalWriteFast(LEDPIN, HIGH);
          ledState = LED_ON;
          ledOnMillis = 0;
        }
      } else {
        ledState = MONITOR;
        ledOnMillis = 0;
      }
      break;
    case LED_ON:
      if (ledOnMillis > BLINKRATE) {
        digitalWriteFast(LEDPIN, LOW);
        ledState = LED_OFF;
        ledOnMillis = 0;
        blinkCount--;
      }
      break;
    default:
      break;
  }
}

void blink(uint8_t i) {
  ledState = BLINK_ENABLE;
  blinkCount = i;
}

void checkButton() {
  if (buttonMillis > 25) {
    button.tick();
    buttonMillis = 0;
  }
}
