#!/bin/bash
# Disassemble ARM64 bytecode file

if [ $# -ne 1 ]; then
    echo "Usage: $0 <bytecode-file>"
    exit 1
fi

# Create temp object file with the bytecode as .text section
objcopy -I binary -O mach-o-arm64 -B aarch64 \
    --rename-section .data=.text,alloc,load,readonly,code \
    "$1" /tmp/bytecode.o 2>/dev/null || {
    # macOS doesn't have objcopy, use a different approach
    echo "Creating wrapper assembly..."
    cat > /tmp/wrapper.s << 'ASM'
.section __TEXT,__text
.globl _bytecode_start
_bytecode_start:
ASM

    # Convert binary to hex and emit as bytes
    hexdump -v -e '/1 ".byte 0x%02x\n"' "$1" >> /tmp/wrapper.s

    as /tmp/wrapper.s -o /tmp/bytecode.o
}

# Disassemble
otool -tv /tmp/bytecode.o
