# Habu Hexdump Command

Hex dump Habu binaries with Mach-O section annotations.

## Arguments
- `$ARGUMENTS` - Binary path and optional range/section, e.g., "/tmp/test" or "/tmp/test 0x400-0x500"

## Workflow

1. **Parse Mach-O Headers**
   - Read Mach-O header to get segment info
   - Map file offsets to sections
   - Identify code vs data regions

2. **Annotate Sections**
   - Mark header region (0x000-0x3FF typically)
   - Mark __TEXT,__text (code)
   - Mark __TEXT,__stubs (import stubs)
   - Mark __DATA_CONST,__got (GOT entries)
   - Mark __LINKEDIT (symbols, strings)

3. **Show Hex Dump**
   - Standard hex dump with ASCII
   - Section annotations in margin
   - Function boundaries from symbol table

## Output Format

```
HABU HEXDUMP: /tmp/test
========================

[MACH-O HEADER]
00000000: cf fa ed fe 0c 00 00 01  00 00 00 00 02 00 00 00  |................|
00000010: 0e 00 00 00 d8 02 00 00  85 00 20 00 00 00 00 00  |.......... .....|
...

[__TEXT,__text @ 0x400]
00000400: d1 03 00 10 fd 7b bf a9  53 d0 3b d5 00 00 00 00  |.....{..S.;.....|  <- _main
00000410: ...
000004ac: d1 03 00 10 fd 7b bf a9  ...                      |.....{..|        <- ADD
...

[__TEXT,__stubs @ 0x518]
00000518: 10 00 00 90 00 00 00 00  ...                      |........|        <- stub__exit

[__DATA_CONST,__got @ 0x4000]
00004000: 00 00 00 00 00 00 00 00  ...                      |........|

[__LINKEDIT @ 0x8000]
00008000: [symbol table, string table, fixups...]
```

## Options

- Range: `/habu-hexdump /tmp/test 0x400-0x500` - dump specific bytes
- Section: `/habu-hexdump /tmp/test __text` - dump named section
- Function: `/habu-hexdump /tmp/test ADD` - dump function bytes

## Example Usage
```
/habu-hexdump /tmp/test_program
/habu-hexdump /tmp/test_program 0x400-0x4FF
/habu-hexdump /tmp/test_program __stubs
```
