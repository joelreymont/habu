#!/usr/bin/env python3
"""Decode ARM64 bytecode from hex dump"""

import sys
import struct

def decode_instruction(word):
    """Decode a 32-bit ARM64 instruction"""
    # STP/LDP
    if (word & 0xFFC00000) == 0xA9800000:
        rt2 = (word >> 10) & 0x1F
        rn = (word >> 5) & 0x1F
        rt = word & 0x1F
        imm = ((word >> 15) & 0x7F)
        if imm >= 64:
            imm -= 128
        return f"STP x{rt}, x{rt2}, [x{rn}, #{imm*8}]!"
    if (word & 0xFFC00000) == 0xA8C00000:
        rt2 = (word >> 10) & 0x1F
        rn = (word >> 5) & 0x1F
        rt = word & 0x1F
        imm = ((word >> 15) & 0x7F)
        return f"LDP x{rt}, x{rt2}, [x{rn}], #{imm*8}"

    # MOVZ
    if (word & 0xFF800000) == 0xD2800000:
        rd = word & 0x1F
        imm16 = (word >> 5) & 0xFFFF
        hw = (word >> 21) & 0x3
        return f"MOVZ x{rd}, #0x{imm16:X}, LSL #{hw*16}"

    # MOVK
    if (word & 0xFF800000) == 0xF2800000:
        rd = word & 0x1F
        imm16 = (word >> 5) & 0xFFFF
        hw = (word >> 21) & 0x3
        return f"MOVK x{rd}, #0x{imm16:X}, LSL #{hw*16}"

    # MOV (ORR)
    if (word & 0xFFE0FC00) == 0xAA0003E0:
        rd = word & 0x1F
        rm = (word >> 16) & 0x1F
        return f"MOV x{rd}, x{rm}"

    # BLR
    if (word & 0xFFFFFC1F) == 0xD63F0000:
        rn = (word >> 5) & 0x1F
        return f"BLR x{rn}"

    # RET
    if word == 0xD65F03C0:
        return "RET"

    return f"<unknown: 0x{word:08X}>"

if len(sys.argv) != 2:
    print("Usage: decode-bytecode.py <bytecode-file>")
    sys.exit(1)

with open(sys.argv[1], 'rb') as f:
    data = f.read()

print(f"Bytecode size: {len(data)} bytes\n")
print("Offset  Instruction")
print("------  -----------")

for i in range(0, len(data), 4):
    if i + 4 <= len(data):
        word = struct.unpack('<I', data[i:i+4])[0]
        print(f"0x{i:04X}  {decode_instruction(word)}")
