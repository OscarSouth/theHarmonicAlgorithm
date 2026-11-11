# make          compile the performance build (MIDIx4)
# make debug    compile with a USB serial port that prints the state set
# make flash    compile and upload the performance build; press the Teensy's button if asked
# make flash-debug
#
# Toolchain: arduino-cli with teensy:avr pinned below; the Teensy udev rule must be installed.
# Every build first runs patch-core, which gives the teensy4 core named USB-MIDI ports.

CORE    := teensy:avr@1.62.0
BOARD   := teensy:avr:teensy40:speed=600,opt=o2std
SKETCH  := $(CURDIR)
PORT     = $(shell arduino-cli board list | awk '/teensy/ {print $$1; exit}')

all: patch
	arduino-cli compile -b $(BOARD),usb=midi4 --output-dir build $(SKETCH)

debug: patch
	arduino-cli compile -b $(BOARD),usb=serialmidi4 --output-dir build-debug $(SKETCH)

flash: all
	arduino-cli upload -b $(BOARD),usb=midi4 --input-dir build -p "$(PORT)" $(SKETCH)

flash-debug: debug
	arduino-cli upload -b $(BOARD),usb=serialmidi4 --input-dir build-debug -p "$(PORT)" $(SKETCH)

patch:
	./patch-core

core:
	arduino-cli core install $(CORE)
	arduino-cli lib install OneButton@2.6.2
	./patch-core

.PHONY: all debug flash flash-debug patch core
