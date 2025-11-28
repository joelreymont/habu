# Habu Self-Hosting Status

**Last Updated**: November 28, 2025

## Current Achievement: Partial Self-Hosting ✓

The Habu compiler can now **compile itself** to a native ARM64 executable:

```bash
$ sbcl --script compiler-driver.lisp bootstrap/compiler.lisp /tmp/habu-compiler
Compiled 612400 bytes (with markers)
Created: /tmp/habu-compiler (1.6MB Mach-O executable)
Success!
```

This is a major milestone: the compiler processes its own 5329-line source code, generates 612KB of ARM64 machine code, and produces a valid Mach-O executable.

## What Works

### Compilation Pipeline (SBCL-hosted)
1. **Reader**: Native Habu reader (`nc-read`) parses Lisp source
2. **Compiler**: Transforms source to IR (`nc-compile`)
3. **Optimizer**: 6 nanopass optimizations (let/progn flattening, constant folding, etc.)
4. **Codegen**: Generates ARM64 machine code (`nc-codegen`)
5. **Linker**: Creates Mach-O executables with dynamic linking (`macho-linker.lisp`)

### Generated Executables (SBCL-free)
- Native ARM64 code (no bytecode, no VM)
- Dynamic linking to libSystem.B.dylib
- Inline heap allocation (bump pointer)
- Inline cons/car/cdr (no runtime calls)
- Inline symbols with compile-time interning
- Full closure support with environment capture

### Test Results
- 77/77 native Mach-O tests pass
- Tail-recursive and non-tail-recursive functions work correctly
- Programs up to 5300+ lines compile successfully
- Complex nested expressions handled via nanopass flattening

## What Doesn't Work Yet

### Generated Compiler Executable
The 1.6MB executable produced by self-compilation **crashes with SIGSEGV** when run.

**Root Cause**: The bootstrap compiler source (`bootstrap/compiler.lisp`) uses SBCL-specific features that compile to function calls, but these functions don't exist in the native runtime:

1. **File I/O**: `with-open-file`, `file-position`, `write-byte`, `read-sequence`
2. **Formatting**: `format`, `princ`, `terpri`, `write-to-string`
3. **System**: `sb-ext:run-program` (for codesigning)
4. **Package**: `intern` (with package argument), `find-package`

When these calls are compiled to native code, they generate `BL` instructions to non-existent addresses, causing crashes.

## Path to Full Self-Hosting

There are two approaches:

### Approach 1: Minimal Native Compiler (Pragmatic)

Create a simplified compiler that can run natively and compile simple programs:

**Required Features**:
- Native file I/O using `sys-open`, `sys-read`, `sys-write`, `sys-close`
- Native string operations (concat, number-to-string)
- Native reader (already have `nc-read`)
- Simplified Mach-O linker (subset for simple programs)
- No verbose output (remove all `format`/`princ` calls)

**Limitations**:
- Can only compile programs without user-defined macros (no `defmacro` support)
- Limited error messages
- No optimization flags or debugging output

**Effort**: ~2-3 days

### Approach 2: Full Native Compiler (Comprehensive)

Port the entire bootstrap compiler to use only Habu-native features:

**Required Work**:
1. Implement native file I/O functions:
   - `native-read-file`: wrapper around sys-open/sys-read/sys-close
   - `native-write-file`: wrapper around sys-open/sys-write/sys-close
   - `native-write-bytes`: low-level binary writing

2. Implement native formatting:
   - `native-concat`: string concatenation (already have)
   - `native-number-to-string`: number formatting (already have)
   - `native-format`: subset of CL format (critical directives only)

3. Rewrite Mach-O linker in Habu-only code:
   - Remove all SBCL dependencies from `macho-linker.lisp`
   - Use native byte manipulation instead of `write-byte` etc.
   - ~2000 lines of careful porting

4. Handle packages natively:
   - Implement native `intern` without package system
   - Use global symbol table

**Effort**: ~2-3 weeks

## Recommended Path Forward

**Short term** (Approach 1): Create a minimal native compiler that can compile itself in a limited mode. This proves the full self-hosting concept without requiring extensive porting work.

**Long term** (Approach 2): Gradually port more features to native code, expanding the capabilities of the self-hosted compiler until it matches the SBCL-hosted version.

## Technical Details

### Why Self-Compilation Works
1. **Nanopass flattening**: Reduces IR depth from 100+ to ~10 levels
2. **Increased temp slots**: 64 → 480 slots (4KB stack frames)
3. **Two-pass compilation**: Handles mutual recursion
4. **Closure optimization**: Inline environment management

### Why Generated Compiler Crashes
The compilation process generates native code for **all** function calls in the source, including SBCL functions. When the native executable runs, it tries to call these functions, but they don't exist in the native runtime.

Example:
```lisp
;; In bootstrap/compiler.lisp:
(with-open-file (f path :direction :io ...)
  ...)

;; Compiles to ARM64:
BL #x12345  ; Call to with-open-file - but this address is invalid!
```

### Required Runtime Functions
For the native compiler to work, we need native implementations of:
- `sys-read-file`: Read entire file to string
- `sys-write-file`: Write string to file
- `sys-write-bytes`: Write byte vector to file
- String manipulation (already have most)
- Number formatting (already have)

## Milestones Achieved

- ✓ Non-tail-recursive functions work (Bug #22 fixed)
- ✓ Large program compilation works (Bug #23 fixed)
- ✓ Nanopass architecture implemented
- ✓ Compiler compiles itself (partial self-hosting)
- ✗ Generated compiler runs natively (full self-hosting) - PENDING

## Next Steps

1. Implement `sys-read-file` and `sys-write-file` native functions
2. Remove `format`/`princ` calls from critical path (use simple string-concat)
3. Create minimal Mach-O linker using only native file I/O
4. Test minimal compiler: can it compile factorial.lisp?
5. Iterate: compile minimal compiler with itself

The first working native compilation (even if limited) will be a huge milestone.
