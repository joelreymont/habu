# Habu Self-Hosting Lisp Compiler - Context

**Last Updated**: December 3, 2025
**Milestone**: Stage 1 compiles and runs (~1.1MB); `&key` support added

## Current Status

**Test Results**: 44 passed, 0 failed, 4 skipped (via ASDF)
**Stage 1**: Compiles and runs (1,101,728 bytes with &key support)

## Current Session (December 3, 2025)

### Changes Made This Session

1. **Removed ARM64 wrapper functions**
   - Removed all wrapper functions from codegen.lisp (add-imm, sub-imm, etc.)
   - Now using arm64:* intrinsics directly with keyword arguments
   - Fixed confusing variable names (cbz-instr -> skip-if-zero, etc.)
   - Updated AGENTS.md with branch offset convention documentation

2. **Fixed string= return value bug**
   - string-equal-ir was returning 0 (fixnum zero) for false
   - In Habu's tagged value system, 0 is truthy (fixnum 0), nil is 6
   - All string comparisons incorrectly appeared equal
   - Fixed in both codegen.lisp and compiler-sbcl.lisp to return 6 (nil)

3. **ASDF-based test infrastructure**
   - Updated `bootstrap/habu.asd` with `habu/tests` system
   - Created `bootstrap/test-harness.lisp` with HABU-TEST package
   - Created `tests/test-core.lisp` with 37 core compiler tests
   - Created `tests/test-keyword-args.lisp` with 6 keyword tests
   - Created `tests/test-packages.lisp` with package tests
   - Run via: `(asdf:test-system :habu)`
   - Test harness tracks totals across all suites

2. **Updated AGENTS.md with testing documentation**
   - Added ASDF test commands and best practices
   - Documented test organization and file naming
   - Added key points about avoiding `(load ...)` in tests

3. **Keyword argument (`&key`) support in bootstrap compiler**
   - Added `parse-lambda-list` to split params at `&key`
   - Added `keyword-to-param-name` to convert `:FOO` keyword to `"FOO"` string
   - Added `find-keyword-position` for locating keyword in specs list
   - Added `rewrite-keyword-call` to convert keyword calls to positional
   - Modified `compile-defun` to handle `&key` params as additional positional
   - Modified `sys:compile` to always rewrite calls when function has `&key`
     (even if call doesn't use keywords, to fill in default values)

4. **Fixed reader to properly handle keywords**
   - Modified `read-sym` to intern keywords into KEYWORD package
     (was interning `:offset` as `|:OFFSET|` in HABU package)
   - Added package-qualified symbol handling with fallback to HABU package
     for unknown packages (needed for user-defined packages in source)

5. **Removed legacy test files**
   - Removed `tests/test_keyword_args.lisp` (superseded by ASDF version)
   - Removed `tests/test_native_packages.lisp` (superseded by ASDF version)
   - Removed `tests/test-setup.lisp` (superseded by ASDF system)
   - Removed legacy compatibility section from test-harness.lisp

### Architecture Notes

**ARM64 Wrapper Functions**: The previous wrapper functions in codegen.lisp
have been simplified to just delegate to `arm64:*` functions. The `arm64/asm.lisp`
file contains all ARM64 encoding logic, and reader conditionals (`#+sbcl`/`#-sbcl`)
in that file handle the differences between SBCL and native mode.

**Package Handling**: Two different paths:
- SBCL bootstrap: `read-sym` handles keywords and package-qualified symbols
  with fallback to HABU package for unknown packages
- Native mode: `reader.lisp` tracks packages via `*current-package*` and
  qualifies symbols during interning

### Stage 1 Status

**Current State**: Stage 1 (~1.1MB) builds and runs correctly.

**Working**:
- `(sys-exit N)` - works
- `(native-read-file ...)` - works
- String operations (string=, string-length, etc.) - work after fix

**Still Not Working**:
- `(habu-read content pos)` - crashes (SIGSEGV) during file parsing
  - Crash in LAMBDA-187 (part of labels construct in habu-read)
  - Trying to dereference raw string data as a pointer
  - Address contains source text like " b))\n(ad"
- `(compile-forms ...)` - crashes (blocks self-hosting)

**Next Steps**:
- Debug habu-read crash (likely labels compilation issue)
- Test Stage 1 → Stage 2 compilation once reader works

## Previous Session (December 2, 2025)

### Previous Session Issues (For Reference)

### Docstring Handling in Defuns (FIXED)

**Problem**: Docstrings in defun bodies were being compiled as code, wasting ~164KB in Stage 1 binary.

**Root Cause**: `compile-all-defuns` (compiler.lisp) and `compile-defuns` (compiler-sbcl.lisp) extracted body with `(cdddr f)` but didn't skip docstrings. A form like `(defun foo (x) "doc" body)` would compile `(progn "doc" body)` instead of just `body`.

**Fix**: Added `skip-docstring` helper function in both files:
```lisp
(defun skip-docstring (body-forms)
  "Skip docstring if present (string as first body element with more forms)"
  (if (and (stringp (car body-forms)) (cdr body-forms))
      (cdr body-forms)
      body-forms))
```

Files changed:
- `compiler.lisp:492-496` - Added skip-docstring
- `compiler.lisp:507` - Use skip-docstring in compile-all-defuns
- `compiler-sbcl.lisp:4621-4625` - Added skip-docstring
- `compiler-sbcl.lisp:4636` - Use skip-docstring in compile-defuns

**Result**: Stage 1 binary reduced from 1,052,480 to 888,320 bytes (~164KB savings).

### GC Triggers in SBCL Heap Allocation Handlers (FIXED)

**Problem**: Stage 1 crashes with SIGBUS at heap boundary (32MB) when running `deliver-file`.

**Root Cause**: The heap allocation handlers in `compiler-sbcl.lisp` (`cons-ir`, `make-vector-ir`, `make-string-from-vector-ir`) perform inline heap allocation but do NOT include GC trigger code. When many allocations happen, the heap fills up without triggering GC.

**Fix**: Added GC trigger check to all three handlers in `compiler-sbcl.lisp`:
- `cons-ir` handler (lines 3145-3158)
- `make-vector-ir` handler (lines 3437-3445)
- `make-string-from-vector-ir` handler (lines 3578-3587)

Each trigger follows the pattern:
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

### Temp Slot Limit Increase (FIXED)

**Problem**: Stage 1 build failed with "Too many temp slots: 472" when compiling gc.lisp.

**Root Cause**: The `list` special form in the compiler expands to nested `cons-ir`:
```lisp
(list a b c) -> (cons-ir a (cons-ir b (cons-ir c (nil-ir))))
```
Each nested cons uses 2 temp slots during codegen. With gc.lisp's long instruction lists (80+ elements in some append calls), this exceeded the 480-slot limit.

**Quick Fix**: Increased temp slot limit from #xF00 (480 slots) to #x2000 (1016 slots) in `compiler-sbcl.lisp:797`.

**TODO**: Optimize `list` compilation to avoid deep nesting (use flat list-ir or evaluate elements first).

### Wrapper-Size Mismatch (FIXED)

**Problem**: Simple programs compiled with `deliver-v2` crashed with SIGSEGV (exit 139) at address 0.

**Root Cause**: `codegen.lisp` expected a 21-instruction (84 bytes) wrapper, but `macho.lisp`'s `wrap-bytecode-with-heap-for-imports` generates a 19-instruction (76 bytes) wrapper. This mismatch caused BL instructions to jump to wrong locations.

**Fix**: Updated `wrapper-size` from 84 to 76 in `codegen.lisp`:
- Line 2616: `deliver-v2` function
- Line 2670: `deliver` function
- Line 2776: `write-symbol-map` function

Also updated the symbol map comment PC offset from `0x100000454` to `0x10000044C`.

**Result**: Simple programs like `(+ 1 2)` now work correctly. Tests pass: 9/10 (closure test fails due to pre-existing limitation).

### Stage 1 Self-Compilation (IN PROGRESS)

**Current Blocker**: Stack overflow during file compilation.

When Stage 1 tries to compile a file via `deliver-file`, it crashes with SIGSEGV (exit 139) due to stack overflow in `SKIP-WS`:
- Error: `EXC_BAD_ACCESS (code=2, address=0x16f603990)` - write access violation on stack
- Each function frame uses 2KB (`sub sp, sp, #0x800`) - see `codegen.lisp:2337`
- With 8MB default stack, only ~4000 nested calls possible
- Reader/compiler call chain exhausts stack when processing files

**Stack Frame Issue**:
- Native codegen: `#x800` (2048 bytes) per frame - `codegen.lisp:2337`
- SBCL codegen: `#xFF0` (4080 bytes) per frame - `codegen-sbcl.lisp:62`
- Both are excessively large for simple functions

**Potential Fixes**:
1. Reduce stack frame size (requires careful analysis of actual needs)
2. Implement proper tail call optimization for reader functions
3. Convert recursive functions to iterative in reader.lisp
4. Increase default stack size (workaround, not solution)

**Also Missing**: `get-cmdline-args` function is not implemented in the native compiler.
- Stage 1 main code cannot parse command-line arguments
- Current workaround: Hardcode source/output paths in the main function

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
  asm.lisp            - ARM64 instruction encoders (canonical API)
```

### ARM64 Instruction API

The canonical ARM64 API is in `arm64/asm.lisp`. It uses keyword arguments for instruction variants:

```lisp
;; Arithmetic
(arm64:add rd rn rm)              ; ADD register
(arm64:add rd rn imm :imm t)      ; ADD immediate
(arm64:sub rd rn rm)              ; SUB register
(arm64:sub rd rn imm :imm t)      ; SUB immediate
(arm64:subs rd rn imm :imm t)     ; SUBS (subtract and set flags)

;; Memory
(arm64:ldr rt rn :offset off)     ; LDR with offset
(arm64:str rt rn :offset off)     ; STR with offset
(arm64:ldr-reg rt rn rm :shift 3) ; LDR with register offset and shift
(arm64:ldrb rt rn offset)         ; LDRB byte load
(arm64:ldrb-post rt rn imm)       ; LDRB with post-increment
(arm64:strb-post rt rn imm)       ; STRB with post-increment

;; Shifts
(arm64:lsl rd rn shift :imm t)    ; LSL immediate
(arm64:lsr rd rn shift :imm t)    ; LSR immediate

;; Branches
(arm64:b offset)                  ; B (offset in instructions, not bytes)
(arm64:cbz rt offset)             ; CBZ
(arm64:b.lt offset)               ; B.LT conditional
(arm64:b.ge offset)               ; B.GE conditional

;; Bitwise
(arm64:and* rd rn mask :imm t)    ; AND with immediate mask
(arm64:orr-imm rd rn imm)         ; ORR with small immediate (1,3,7,15)
```

**IMPORTANT**: Always use the `arm64:` intrinsics directly. The wrapper functions in `codegen.lisp` (`add-imm`, `ldr-offset`, etc.) are deprecated and cause confusion. New code should use the keyword-argument API.

**DO NOT** add new wrapper functions with `#+sbcl` / `#-sbcl` in codegen.lisp. Instead, add the reader conditionals directly in `arm64/asm.lisp` intrinsics. The goal is to eventually replace all existing wrappers with direct intrinsic calls.

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
