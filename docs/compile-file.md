# File Compilation Design (compile-file)

## Overview

Habu implements Common Lisp file compilation via `compile-file`, which compiles `.habu` source files to `.hfasl` (Habu Fast Load) binary files.

## FASL Format

FASL = Fast Load format. Binary representation of compiled code for efficient loading.

### File Structure

```
Magic:    "HFASL\0\0\0"  (8 bytes)
Version:  u32            (format version, currently 1)
Sections: []Section

Section:
  Type:   u8            (1=code, 2=constants, 3=symbols, 4=packages)
  Length: u32           (section data length in bytes)
  Data:   [u8]          (section-specific data)
```

### Sections

**Code Section (type=1):**
- Bytecode instructions
- Function metadata (arity, closure vars)
- Debug info (line numbers, file name)

**Constants Section (type=2):**
- Fixnums (63-bit signed)
- Strings (UTF-8 encoded)
- Floating-point numbers
- Complex numbers
- Rationals
- Circular structure handling via back-references

**Symbols Section (type=3):**
- Symbol names (interned during load)
- Package associations
- Symbol properties

**Packages Section (type=4):**
- Package definitions
- Use-package declarations
- Export lists

## Compilation Process

### Phase 1: Parse and Analyze
1. Read source file as S-expressions
2. Parse each top-level form
3. Identify compile-time vs load-time evaluation

### Phase 2: Separate Compile-Time Forms
- `eval-when` with `:compile-toplevel` - execute during compilation
- `defmacro` - define macros for use in same file
- `defconstant` - define constants for compile-time folding

### Phase 3: Compile Load-Time Forms
- `defun`, `defvar`, `defclass` - compile to bytecode
- Lambda expressions - compile closures
- Top-level expressions - compile for side effects

### Phase 4: Serialize to FASL
1. Build constant pool (deduplicate constants)
2. Emit bytecode with constant indices
3. Handle circular structures via back-references
4. Write sections to binary file

## Load Process

`load` function supports both `.habu` (source) and `.hfasl` (compiled):

1. Detect file type by extension or magic bytes
2. If `.hfasl` is newer than `.habu`, prefer compiled version
3. Deserialize sections from FASL
4. Intern symbols in target packages
5. Reconstruct objects from constant pool
6. Execute top-level bytecode

## Compile-Time vs Load-Time

**Compile-time evaluation:**
- `(eval-when (:compile-toplevel) ...)` - runs during compilation only
- `defmacro` - macro available in same compilation unit
- `defconstant` - value available for constant folding

**Load-time evaluation:**
- `(eval-when (:load-toplevel) ...)` - runs when loading FASL
- `defun`, `defvar`, `defclass` - definitions created at load time
- Top-level expressions - side effects occur at load time

**Both:**
- `(eval-when (:compile-toplevel :load-toplevel) ...)` - runs at both times

## Circular References

FASL handles circular structures (e.g., `(setf (cdr x) x)`):

1. First occurrence: assign ID, serialize structure
2. Subsequent occurrences: emit back-reference to ID
3. During load: reconstruct with forward references, patch after all objects allocated

## External Format

`load` supports `:external-format` keyword for character encoding:
- Default: `:utf-8`
- Supported: `:utf-8`, `:ascii`, `:latin-1`

## Implementation Files

- `src/interp/repl.zig` - FASL fallback handling during `load` (`.fasl` / `.hfasl`)
- `src/compiler/compile.zig` - File compiler (compile-file)
- `src/runtime/primitives/io.zig` - Enhanced load with FASL support

## Future Enhancements

- Compression (gzip, zstd)
- Incremental compilation (only recompile changed definitions)
- Cross-compilation (compile on one platform, load on another)
- Debug symbol stripping for production builds
- AOT compilation (FASL → native machine code)
