# DWARF5 Debug Info Implementation

This document describes the DWARF5 debug info generation for Habu compiled programs.

## Overview

DWARF (Debug With Arbitrary Record Formats) is the standard debug info format used by debuggers like lldb and gdb. Habu generates DWARF5 sections embedded in Mach-O executables to enable:

- **Function names in backtraces** - `bt` command shows function names
- **Source line mapping** - step through code at source level
- **Symbol lookup** - set breakpoints by function name

## Sections Generated

### __debug_abbrev

Contains the abbreviation table that defines the structure of DIE (Debugging Information Entry) records.

**Abbreviation 1: DW_TAG_compile_unit** (has children)
- DW_AT_name - source filename
- DW_AT_producer - "Habu Lisp Compiler 0.1"
- DW_AT_language - DW_LANG_Lisp (0x08)
- DW_AT_low_pc - start address of code
- DW_AT_high_pc - size of code (offset from low_pc)
- DW_AT_stmt_list - offset to line table
- DW_AT_comp_dir - compilation directory

**Abbreviation 2: DW_TAG_subprogram** (no children)
- DW_AT_name - function name
- DW_AT_low_pc - function start address
- DW_AT_high_pc - function size
- DW_AT_external - function is visible
- DW_AT_decl_file - source file index
- DW_AT_decl_line - line number

### __debug_info

Contains the actual debug information as a tree of DIEs:

```
Compile Unit Header:
  - unit_length (4 bytes, DWARF32)
  - version (2 bytes) = 5
  - unit_type (1 byte) = DW_UT_compile
  - address_size (1 byte) = 8
  - debug_abbrev_offset (4 bytes) = 0

DIE Tree:
  DW_TAG_compile_unit (abbrev 1)
    DW_TAG_subprogram (abbrev 2)  ; for each function
    ...
    NULL  ; end of children
```

### __debug_line

Contains line number program to map addresses to source lines.

**Header (DWARF5 format):**
- unit_length, version=5, address_size=8
- segment_selector_size=0
- header_length
- min_inst_length=4 (ARM64)
- max_ops_per_inst=1
- default_is_stmt=1
- line_base=-5, line_range=14, opcode_base=13
- Directory entry format (DW_LNCT_path, DW_FORM_string)
- File entry format (DW_LNCT_path + DW_LNCT_directory_index)
- Directory table
- File table

**Line Program:**
For each function:
1. DW_LNE_set_address - set PC to function start
2. DW_LNS_advance_line - set line number
3. DW_LNS_set_prologue_end - mark end of prologue
4. DW_LNS_copy - emit line entry

End with DW_LNE_end_sequence.

## Mach-O Integration

DWARF sections are placed in a __DWARF segment:

```
__DWARF segment (no VM mapping needed):
  __debug_abbrev section
  __debug_info section
  __debug_line section
```

Section flags use S_ATTR_DEBUG (0x02000000) to indicate debug data.

## Usage

### Generating Debug Info

```lisp
;; Generate DWARF sections
(generate-dwarf functions source-name comp-dir code-start code-size)
;; Returns: (values abbrev-bytes info-bytes line-bytes)

;; For Mach-O integration
(dwarf-sections-for-macho functions source-file code-base code-size)
;; Returns: (("__debug_abbrev" . bytes) ("__debug_info" . bytes) ("__debug_line" . bytes))
```

### Function List Format

Functions are specified as a list of tuples:
```lisp
((name offset size line-num)
 ("add" 0 32 1)
 ("main" 32 48 2))
```

Where:
- `name` - function name string
- `offset` - byte offset from code start
- `size` - function size in bytes
- `line-num` - source line number (or nil for 1)

## Debugging with lldb

Once DWARF is embedded, use lldb:

```bash
# Show backtrace with function names
(lldb) bt

# Set breakpoint by function name
(lldb) b add

# Step through source
(lldb) n

# Show current source line
(lldb) l
```

## Implementation Notes

### ULEB128/SLEB128 Encoding

Variable-length integers used throughout DWARF:
- ULEB128: unsigned, 7 bits per byte, high bit = more bytes
- SLEB128: signed, uses sign extension

### Address Encoding

We use DW_FORM_addr (8 bytes, little-endian) for all addresses.
High_pc uses DW_FORM_data4 as an offset from low_pc to save space.

### Limitations

Current implementation:
- No type information (DW_TAG_base_type not fully used)
- No variable location info (DW_AT_location)
- No inlined function info
- Single compilation unit only

## References

- [DWARF Version 5 Standard](https://dwarfstd.org/doc/DWARF5.pdf)
- [DWARF Explorer (online tool)](https://dwarfstd.org/dwarf-explorer.html)
- Apple Mach-O Debug Information Format
