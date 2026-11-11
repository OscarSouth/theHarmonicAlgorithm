// USB device and port names. Must be a .c file of its own (not .cpp, not the .ino).
//
// The port names take effect through patch-core, which gives the teensy4 core
// the teensy3 core's usb_string_midi_portN mechanism; `make` applies it.

#include "usb_names.h"

#define VENDOR_NAME {'M','i','D','i','N','i','N','j','A'}
#define VENDOR_NAME_LEN 9

#define MIDI_NAME   {'X','3'}
#define MIDI_NAME_LEN  2

#define MIDI_PORT1_NAME   {'M','a','i','n'}
#define MIDI_PORT1_NAME_LEN 4

#define MIDI_PORT2_NAME   {'D','r','u','m','s'}
#define MIDI_PORT2_NAME_LEN 5

#define MIDI_PORT3_NAME   {'L','o','o','p','e','r'}
#define MIDI_PORT3_NAME_LEN 6

#define MIDI_PORT4_NAME   {'M','a','p','p','e','r'}
#define MIDI_PORT4_NAME_LEN 6

struct usb_string_descriptor_struct usb_string_manufacturer_name = {
        2 + VENDOR_NAME_LEN * 2,
        3,
        VENDOR_NAME
};

struct usb_string_descriptor_struct usb_string_product_name = {
        2 + MIDI_NAME_LEN * 2,
        3,
        MIDI_NAME
};

struct usb_string_descriptor_struct usb_string_midi_port1 = {
        2 + MIDI_PORT1_NAME_LEN * 2,
        3,
        MIDI_PORT1_NAME
};

struct usb_string_descriptor_struct usb_string_midi_port2 = {
        2 + MIDI_PORT2_NAME_LEN * 2,
        3,
        MIDI_PORT2_NAME
};

struct usb_string_descriptor_struct usb_string_midi_port3 = {
        2 + MIDI_PORT3_NAME_LEN * 2,
        3,
        MIDI_PORT3_NAME
};

struct usb_string_descriptor_struct usb_string_midi_port4 = {
        2 + MIDI_PORT4_NAME_LEN * 2,
        3,
        MIDI_PORT4_NAME
};
