# Habu Self-Hosting Lisp Compiler - Context

**Last Updated**: December 2, 2025
**Milestone**: Stage 1 compiles and runs (642KB); testing Stage 1 → Stage 2

## Current Status

**Test Results**: 76/76 pass
**Stage 1**: Compiles and runs (642KB binary with full compiler + GC runtime)

## Current Session (December 2, 2025)

### GC Trigger in SBCL cons-ir Handler (FIXED)

**Problem**: Stage 1 crashes with SIGBUS at heap boundary (32MB) when running `deliver-file`.

**Root Cause**: The `cons-ir` handler in `compiler-sbcl.lisp` performs inline heap allocation but does NOT include GC trigger code. When many cons operations happen (e.g., in while loops building lists), the heap fills up without triggering GC.

**Fix**: Added GC trigger check to cons-ir handler in `compiler-sbcl.lisp:3145-3158`:
```lisp
(arm64:ldr 9 27 :offset 16)       ; x9 = from_end [x27+16]
(arm64:cmp 28 9)                  ; compare x28, from_end
(arm64:b.lo 2)                    ; skip if x28 < from_end
(list '(:call-fn GC-COLLECT))     ; bl gc_collect
```

### GC-COPY Branch Offsets (FIXED)

**Problem**: GC-COPY function had wrong branch offsets for nil/fixnum checks. The branches to "return unchanged" were landing in the copy loop instead of at `ret`.

**Root Cause**: The instruction offsets in `gc.lisp` were wrong (40, 38, 34, 32 instead of 14, 12, 9, 7).

**Fix**: Corrected branch offsets in `gc.lisp:126-137`:
- cbz for fixnum: 40 → 14
- b.eq for nil: 38 → 12
- b.lo for < from_start: 34 → 9
- b.hs for >= from_end: 32 → 7

### Stage 1 Self-Compilation (IN PROGRESS)

**Current Blocker**: Crash in `LIFT-ALL-LABELS-TO-DEFUNS` when running `deliver-file`.
- x0 contains string bytes (ASCII "(defun-") treated as pointer
- This is a reader/compiler bug, not GC-related
- Crash happens very early before significant heap allocation

### Reader Fix for Symbols with Dots (FIXED)

**Problem**: Symbols like `arm64:b.lo` were being parsed as dotted pairs `(arm64:b . lo)` instead of single symbols.

**Root Cause**: Both SBCL and native readers treated `.` after a symbol start as dotted-pair syntax.

**Fix**: Modified BOTH readers (compiler-sbcl.lisp AND reader.lisp):
1. Added `.` (#x2E) to `symbol-char-p` / `symbol-char?` function
2. Modified `read-list-elems` to only treat `.` as dotted pair when standalone (followed by whitespace, `)`, or EOF)

Files changed:
- `compiler-sbcl.lisp:515-518` - Added dot to symbol-char-p
- `compiler-sbcl.lisp:614-632` - Modified read-list-elems for dot handling
- `reader.lisp` - Same fixes for native reader

### Stage 1 Build from ASDF Sources (WORKING)

Stage 1 now builds correctly from ASDF-managed source files:
- Binary size: 757KB (756992 bytes)
- Exit code: 42 (expected)
- Uses reader conditionals via SBCL reader (`:habu` feature only)

Build command:
```lisp
(habu:deliver (concatenate 'string gc-src reader-src compiler-src
               optimize-src codegen-src macho-utils-src "(sys-exit 42)")
              "/tmp/habu_stage1_asdf")
```

### Symbol State / GC Offset Conflict (FIXED)

**Problem**: Stage 1 crashed with SIGBUS (exit 138) when symbol state storage conflicted with GC globals.

**Root Cause**: Symbol state was stored at offsets [x27+16] and [x27+24] which conflicted with GC's `from_end` and `half_heap_size` globals at the same offsets.

**Fix**: Moved symbol state to new offsets after GC globals:
- Symbol counter: [x27+48] (was [x27+16])
- Symbol table: [x27+56] (was [x27+24])
- Heap data: [x27+64] (was [x27+48])

Files changed:
- `gc.lisp:37-45` - Updated offset constants
- `gc.lisp:379-429` - Updated gc-heap-init-code comments
- `codegen.lisp:2063-2084` - Updated primitive implementations
- `macho.lisp:882-918` - Updated wrapper to use offsets 48/56 and bump x28 by 64

**Result**: Stage 1 with `(sys-exit 42)` now runs correctly (872KB binary).

### Stage 1 Self-Compilation (IN PROGRESS)

**Status**: Stage 1 with arg-checking main crashes with SIGBUS (exit 138)

**Simple Test Works**: `/tmp/habu_stage1_fixed` with `(sys-exit 42)` exits correctly with code 42.

**Complex Main Crashes**: When adding arg-checking logic:
```lisp
(let ((args (get-cmdline-args)))
  (if (< (length args) 3)
      (sys-exit 1)
      (progn (deliver-file (nth 1 args) (nth 2 args)) (sys-exit 0))))
```

**Crash Details**:
- Crashes in DELIVER-FILE function at address 0x10001962c
- x28 register shows corrupted high bits: 0x10000001004c8230
- Crash address 0x1020c8000 is exactly at heap boundary (32MB from base)

**Investigation Needed**: The crash happens even with no arguments (should exit with 1). Either:
1. The arg-checking logic is wrong
2. Something in `get-cmdline-args` or `length` corrupts x28
3. The if-else branching has an issue

### Stack Slot Collision Bug (FIXED)

**Symptom**: Crashes when 2-param function calls another 2-param function inside nested lets.

**Root Cause**: Codegen collision between param slots and spill slots.
- `x20 = sp + 0x388` (environment frame base)
- Param 0 stored at `x20-0` = `sp + 0x388`
- Temp spill also using `sp + 0x388`
- Spill overwrites param 0, causing nil to be passed to callee

**Fix**: Changed spill-end from `#x340` to `#x440` in `codegen.lisp:240` to give more space between spill slots and parameter slots.

### SIGILL Bug - BL Placeholder Misalignment (FIXED)

**Symptom**: Stage 1 binary crashed with SIGILL (exit 132) at undefined instruction.

**Root Cause**: The flatten functions emitted only 3 placeholder zeros for each 4-byte BL instruction:
```lisp
;; BUG: Only 3 zeros for 4-byte BL instruction
(cons 0 (cons 0 (cons 0 (cons marker acc))))
```

This caused 1-byte misalignment per call site, accumulating to large offsets.

**Fix**: Changed 5 locations in `codegen.lisp` to emit 4 zeros and 4 skip counts:
- `flatten-code-keep-markers-and-calls`: 3 emit locations (3→4 zeros)
- `flatten-code-keep-markers`: 2 emit locations (3→4 zeros)
- `flatten-all-calls`: 2 skip counts (3→4)
- `flatten-extern-calls`: 2 skip counts (3→4)
- Also fixed `compiler-sbcl.lisp:335` and `compiler.lisp:1951`

**Related**: Fixed `deliver-v2` to use `count-actual-bytes` instead of `length` for accurate size calculation.

## Architecture

### File Structure
```
bootstrap/
  compiler-sbcl.lisp  - SBCL bootstrap compiler
  compiler.lisp       - Habu compiler (no SBCL dependencies)
  optimize.lisp       - Optimization passes (TCO)
  codegen.lisp        - ARM64 code generator
  gc.lisp             - Garbage collector (Cheney's copying GC)
  macho.lisp          - Mach-O linker (#+sbcl versions)
  macho-utils.lisp    - Mach-O utilities (#-sbcl native versions)
  reader.lisp         - Habu reader
arm64/
  asm.lisp            - ARM64 instruction encoders
```

### Tagged Value Representation
- Fixnum: `value << 4`, tag 0
- Cons: `pointer | 1`
- Symbol: `pointer | 2`
- Vector: `pointer | 3`
- String: `pointer | 4`
- Closure: `pointer | 5`
- Nil: `0x06` (tag 6)

### ARM64 Register Usage
- x0-x7: Arguments and return value
- x20: Environment frame base
- x24: Closure environment pointer
- x26: Code base register
- x27: GC globals base
- x28: Heap bump pointer

## Key Features Implemented

- **TCO**: Nanopass architecture, transforms self-tail-calls to loop-ir/continue-ir
- **GC**: Cheney's copying collector (32MB semispaces), triggers after allocations
- **Closures**: Full support including nested and multi-capture
- **While loops**: True iteration without stack growth
- **Function symbols**: LC_SYMTAB for lldb debugging

## Known Limitations

1. Max 8 arguments per function
2. 64KB file limit for native-read-file
3. No reader conditionals in native mode
4. Inlining disabled (variable capture bug)

## Slash Commands

Available commands for Habu development:

**Build & Test:**
- `/habu-build-test` - Compile and test workflow
- `/habu-run-tests [pattern]` - Run test suite
- `/habu-stage <N|verify>` - Self-compilation stages

**Debugging:**
- `/habu-debug <binary>` - Debug crashes with lldb
- `/habu-analyze <error>` - Structured error analysis
- `/habu-disasm <binary> [function]` - Disassemble binaries

**Inspection:**
- `/habu-ir <source>` - Inspect compiler IR
- `/habu-compare <bin1> <bin2>` - Compare binaries
- `/habu-hexdump <binary> [range]` - Hex dump with annotations
- `/habu-profile <binary> [duration]` - Profile running binary

**System:**
- `/habu-load` - Load compiler via ASDF

## Debugging

- Exit 132 = SIGILL (illegal instruction - check code alignment/branch targets)
- Exit 139 = SIGSEGV (check stack/spill)
- Exit 137 = SIGKILL (codesign issue on macOS)
- Use `--dynamic-space-size 4096` for large compilations

## Simplification Roadmap

### Vision

**Ultimate Goal**: A fully self-hosting Common Lisp compiler that:
- Generates native ARM64 code (x86_64 planned)
- Matches or exceeds SBCL performance on ARM64
- Implements full Common Lisp specification
- Requires no external Lisp system after bootstrap

### Phases

1. **Self-Hosting** (current focus)
   - Stage 1: SBCL compiles Habu -> native binary
   - Stage 2: Stage 1 compiles Habu -> native binary
   - Stage 3: Stage 2 compiles Habu -> native binary (fixed point)

2. **SBCL Independence**
   - Native eval (minimal subset for macros)
   - Native reader conditionals
   - Standalone build system

3. **Performance Parity**
   - Complete TAC pipeline and register allocator
   - Re-enable inlining (fix variable capture bug)
   - Constant folding, dead code elimination

4. **Full CL Spec**
   - CLOS
   - Conditions and restarts
   - Packages
   - Multiple values

### Current Architecture Issues

- Two codegen files: `arm64/codegen-sbcl.lisp` (SBCL bootstrap) and `bootstrap/codegen.lisp` (both modes)
- 95+ reader conditionals in codegen.lisp
- Duplicate functions between `macho.lisp` and `macho-utils.lisp`

### Planned Changes

1. Keep `arm64/codegen-sbcl.lisp` (needed for SBCL bootstrap)
2. Merge `macho-utils.lisp` into `macho.lisp`
3. Reduce reader conditional count over time
4. Remove dead code (backup files, unused packages)
