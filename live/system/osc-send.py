#!/usr/bin/env python3
"""Minimal OSC message sender using only Python stdlib.
Usage: python3 osc-send.py <host> <port> <address> <type> <value>
       python3 osc-send.py <host> <port> <address> s --file <path>
Types: s (string), f (float), i (int32)
"""
import socket, struct, sys

def osc_string(s):
    b = s.encode("utf-8") + b"\x00"
    b += b"\x00" * ((4 - len(b) % 4) % 4)
    return b

def osc_message(address, typetag, *args):
    msg = osc_string(address) + osc_string("," + typetag)
    for t, v in zip(typetag, args):
        if t == "s":
            msg += osc_string(v)
        elif t == "f":
            msg += struct.pack(">f", float(v))
        elif t == "i":
            msg += struct.pack(">i", int(v))
    return msg

if __name__ == "__main__":
    if len(sys.argv) < 6:
        print(__doc__.strip(), file=sys.stderr)
        sys.exit(1)

    host, port, address, typetag = sys.argv[1], int(sys.argv[2]), sys.argv[3], sys.argv[4]
    value = sys.argv[5]

    if value == "--file" and len(sys.argv) > 6:
        with open(sys.argv[6], "r") as f:
            value = f.read()

    msg = osc_message(address, typetag, value)
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.sendto(msg, (host, port))
    sock.close()
