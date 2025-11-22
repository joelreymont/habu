#!/usr/bin/env python3
"""Analyze let binding bytecode"""

# Bytecode from debug-let.lisp
bytecode = [
    0xFF, 0xC3, 0x00, 0xD1,  # 00: SUB SP, SP, #48
    0xFD, 0x7B, 0x00, 0xA9,  # 04: STP X29, X30, [SP, #0]
    0xF3, 0x53, 0x01, 0xA9,  # 08: STP X19, X20, [SP, #16]
    0xF5, 0x5B, 0x02, 0xA9,  # 0C: STP X21, X22, [SP, #32]
    0xF3, 0x03, 0x00, 0xAA,  # 10: MOV X19, X0
    0xF4, 0x03, 0x1F, 0xAA,  # 14: MOV X20, SP
    0xF5, 0x03, 0x14, 0xAA,  # 18: MOV X21, X20 (save old env)
    0xFF, 0x23, 0x00, 0xD1,  # 1C: SUB SP, SP, #8 (allocate for 1 var)
    0xF4, 0x03, 0x1F, 0xAA,  # 20: MOV X20, SP (new env base)
    0x00, 0x14, 0x80, 0xD2,  # 24: MOVZ X0, #0xA0 (10 << 4 = 160)
    0x80, 0x02, 0x00, 0xF9,  # 28: STR X0, [X20, #0]
    0x80, 0x02, 0x40, 0xF9,  # 2C: LDR X0, [X20, #0]
    0xFF, 0x23, 0x00, 0x91,  # 30: ADD SP, SP, #8 (deallocate)
    0xF4, 0x03, 0x15, 0xAA,  # 34: MOV X20, X21 (restore old env)
    0xF5, 0x5B, 0x42, 0xA9,  # 38: LDP X21, X22, [SP, #32]
    0xF3, 0x53, 0x41, 0xA9,  # 3C: LDP X19, X20, [SP, #16]
    0xFD, 0x7B, 0x40, 0xA9,  # 40: LDP X29, X30, [SP, #0]
    0xFF, 0xC3, 0x00, 0x91,  # 44: ADD SP, SP, #48
    0xC0, 0x03, 0x5F, 0xD6,  # 48: RET
]

print("Let binding bytecode analysis:")
print("==============================")
print()
print("Prologue:")
print("  00: SUB SP, SP, #48       - Allocate main stack frame")
print("  04: STP X29, X30, [SP]    - Save frame pointer and link register")
print("  08: STP X19, X20, [SP+16] - Save runtime table and env base")
print("  0C: STP X21, X22, [SP+32] - Save temp registers")
print("  10: MOV X19, X0           - Save runtime table from arg")
print("  14: MOV X20, SP           - Initial env base = SP")
print()
print("Let expression:")
print("  18: MOV X21, X20          - Save old env base")
print("  1C: SUB SP, SP, #8        - Allocate space for 1 binding")
print("  20: MOV X20, SP           - New env base = new SP")
print("  24: MOVZ X0, #0xA0        - Load 10 << 4 = 160 (tagged 10)")
print("  28: STR X0, [X20, #0]     - Store value at env[0]")
print("  2C: LDR X0, [X20, #0]     - Load value from env[0]")
print("  30: ADD SP, SP, #8        - Deallocate binding space")
print("  34: MOV X20, X21          - Restore old env base")
print()
print("Epilogue:")
print("  38: LDP X21, X22, [SP+32] - Restore temp registers")
print("  3C: LDP X19, X20, [SP+16] - Restore runtime/env registers")
print("  40: LDP X29, X30, [SP]    - Restore frame/link registers")
print("  44: ADD SP, SP, #48       - Deallocate main frame")
print("  48: RET                    - Return")
print()
print("Analysis:")
print("- The code structure looks correct")
print("- Variable is properly stored and loaded at X20")
print("- Stack is properly allocated and deallocated")
print("- All registers are saved and restored")
print()
print("The code should work. The crash might be from:")
print("1. Stack alignment issues (ARM64 requires 16-byte alignment)")
print("2. The runtime expecting different calling convention")
print("3. Incorrect offset calculations")