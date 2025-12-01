# Habu Disassemble Command

Disassemble Habu-generated binaries with function name annotations.

## Arguments
- `$ARGUMENTS` - Path to binary or function name to disassemble

## Workflow

1. **Load Binary**
   - Use lldb to load the binary
   - Check if it's a valid Mach-O executable

2. **List Functions** (if no specific function given)
   - Run `nm <binary>` to list all symbols
   - Identify local functions (lowercase 't') vs external ('T')
   - Show addresses and names

3. **Disassemble Function** (if function name given)
   - Use `lldb -b -o "disassemble -n <function>"`
   - Show ARM64 instructions with offsets
   - Annotate known patterns (prologue, epilogue, syscalls)

4. **Compare with Map File** (if .map exists)
   - Load corresponding .map file
   - Cross-reference addresses
   - Show byte offsets from function start

## Output Format

```
BINARY: /path/to/binary
SIZE: 16384 bytes
CODE SECTION: 0x400 - 0x5A0

FUNCTIONS:
  0x400  _main (T)      - 172 bytes
  0x4AC  _ADD (t)       - 108 bytes
  0x518  _MUL (t)       - 108 bytes
  0x584  stub__exit     - 12 bytes

DISASSEMBLY of _ADD:
  +0x00: sub    sp, sp, #0x800    ; prologue
  +0x04: str    x29, [sp, #0x7f0]
  ...
  +0x68: ret                       ; epilogue
```

## Example Usage
```
/habu-disasm /tmp/test_program
/habu-disasm /tmp/test_program ADD
/habu-disasm ADD   ; assumes last compiled binary
```
