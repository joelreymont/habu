# Habu Compare Command

Compare two Habu binaries byte-by-byte for fixed-point verification.

## Arguments
- `$ARGUMENTS` - Two binary paths to compare, e.g., "/tmp/stage2 /tmp/stage3"

## Workflow

1. **Load Binaries**
   - Read both files as byte arrays
   - Verify both are valid Mach-O executables

2. **Size Comparison**
   - Compare total file sizes
   - If different, note the size delta

3. **Section Analysis**
   - Compare __TEXT segment sizes
   - Compare __DATA segment sizes
   - Compare __LINKEDIT sizes

4. **Byte-by-Byte Diff**
   - Find first differing byte position
   - Show context around differences
   - Group consecutive differences

5. **Symbol Comparison**
   - Run `nm` on both binaries
   - Diff symbol tables
   - Note any missing/extra symbols

## Output Format

```
BINARY COMPARISON
=================

File A: /tmp/stage2 (67584 bytes)
File B: /tmp/stage3 (67584 bytes)

SIZE: MATCH

SECTIONS:
  __TEXT:    16384 / 16384  MATCH
  __DATA:    16384 / 16384  MATCH
  __LINKEDIT: 4096 / 4096   MATCH

CONTENT:
  Bytes 0x000-0x3FF (headers): MATCH
  Bytes 0x400-0x5FF (code):    MATCH
  ...

SYMBOLS:
  File A: 42 symbols
  File B: 42 symbols
  Difference: NONE

RESULT: IDENTICAL (fixed point achieved)
```

## Difference Report

```
DIFFERENCES FOUND:

Offset 0x4A8: A=0x52 B=0x53
  Context A: ... 91 00 00 [52] 00 00 94 ...
  Context B: ... 91 00 00 [53] 00 00 94 ...
  Location: _ADD + 0x0C (in function ADD)

Offset 0x510: A=0xE8 B=0xE9
  ...

Total: 2 differences in 67584 bytes
```

## Use Cases

1. **Fixed-point verification** - Stage 2 should equal Stage 3
2. **Regression testing** - Compare before/after a change
3. **Debug non-determinism** - Find what varies between builds

## Example Usage
```
/habu-compare /tmp/stage2 /tmp/stage3
/habu-compare /tmp/before_fix /tmp/after_fix
```
