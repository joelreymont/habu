# Habu Self-Hosting Lisp Compiler - Context

**Last Updated**: December 1, 2025
**Milestone**: Stage 1 compiles and runs; working on Stage 1 → Stage 2

## Current Status

The Habu compiler compiles programs with:
- Expressions: literals, arithmetic, comparisons
- Control flow: if, cond, when, unless, progn, and, or, not, setq
- Bindings: let, let*
- Functions: defun, labels, lambda, funcall, (function name)
- Closures: captured variables, higher-order functions
- Data: cons, car, cdr, list, quote, setcar, setcdr
- Predicates: null, consp, numberp, symbolp, stringp, vectorp
- Strings: string-length, string-ref, string-concat
- Symbols: symbol-name, make-symbol-from-string
- Vectors: make-vector, vector-ref, vector-set, vector-length
- File I/O: sys-open, sys-read, sys-write, sys-close, native-read-file
- System: sys-exit, get-intern-table, set-intern-table

**Test Results**: 76/76 pass (comprehensive, self-hosting, edge cases)

## Current Session (December 1, 2025)

### Stage 1 Status

Full Stage 1 compiler (reader + compiler + codegen + macho-utils) works:
- Source size: ~170KB
- Native binary: ~67KB (was 8.7MB before zerofill fix!)
- Compiles and runs: PASS (exit 42)
- read-all, compile-forms, compile-program: all work natively

### Key Improvements This Session

1. **Native Garbage Collector (NEW)**
   - Cheney's copying collector with two semispaces (32MB each)
   - GC globals segment for from_start, from_end, to_start, to_end
   - gc_copy and gc_collect in ARM64 assembly (gc.lisp)
   - GC triggers inserted after cons, string, closure allocations
   - gc-trigger-code generates inline check + conditional call to GC-COLLECT
   - Unified `deliver` function now includes GC runtime code

2. **Function symbols in LC_SYMTAB**
   - Mach-O binaries now include function symbols for lldb debugging
   - `nm <binary>` shows all function names and addresses
   - `lldb -o "disassemble -n ADD"` works directly on Habu binaries
   - Implemented in macho.lisp: build-symbol-table-with-locals, etc.
   - deliver-v3 now passes fnoffs to write function symbols

2. **Slash commands for development (NEW)**
   - Created 6 commands in .claude/commands/
   - /habu-build-test, /habu-debug, /habu-analyze, /habu-run-tests
   - /habu-disasm (new), /habu-stage (new)
   - Updated AGENTS.md with command documentation

3. **Mach-O zerofill for heap (FIXED)**
   - Was writing 64MB of zeros to executable file
   - Now uses S_ZEROFILL section type - no file data, OS zeroes on demand
   - File size reduced from 8.7MB to 67KB for simple programs

4. **Added `while` loop construct**
   - Supports `(while test body...)` for true iteration without stack growth
   - Added compile-while in compiler.lisp
   - Added while-ir codegen in codegen.lisp
   - Fixed branch offset bug (+8 not +4 to exit past backward branch)

5. **Stack overflow fix (PARTIAL)**
   - read-list-elems was the critical function - recursed per list element
   - FIXED: Rewrote read-list-elems to use iterative `while` loop
   - Added SBCL `while` macro for compatibility with native version
   - Remaining: read-sym-chars, upcase-string-iter recurse per char (less critical)

6. **TCO (Tail Call Optimization) Implemented**
   - Nanopass architecture: IR transformation + code emission
   - Pass 1: `apply-tco-to-function` (optimize.lisp) transforms self-tail-calls to `loop-ir`/`continue-ir`
   - Pass 2: `codegen` handles loop-ir/continue-ir, emits `:tco-branch` markers
   - Pass 3: `resolve-tco-branches` converts markers to backward B instructions
   - Fixed `code-size` to count `:tco-branch` markers as 4 bytes
   - Tested: countdown(100000) works without stack overflow (would need 200MB stack without TCO)

7. **Register Allocation Architecture (NEW)**
   - Created `bootstrap/reg-alloc.lisp` with 5-nanopass pipeline
   - TAC (Three-Address Code) format with virtual registers
   - Liveness analysis via backward dataflow
   - Linear scan allocation to x9-x15
   - Full documentation in source file header (150 lines)
   - Remaining: implement tac-codegen for ARM64 emission

8. **Native resolve-calls function (NEW)**
   - Added `#-sbcl` version of `resolve-calls` to codegen.lisp
   - Handles `:call-fn`, `:tail-call-fn`, `:loop-start`, `:loop-continue` markers
   - Uses arm64 intrinsics (arm64:bl, arm64:b) for branch emission
   - Essential for native compiler's compile-program function

9. **resolve-tco-branches marker preservation (FIXED)**
   - Bug: resolve-tco-branches was flattening `:call-fn` markers as nested lists
   - This turned `(:call-fn FACTORIAL)` into loose `:call-fn` and `FACTORIAL` elements
   - Fix: Added `marker-p` predicate to preserve call/extern markers as single items
   - Core tests now pass: basic, function call, recursive factorial, nested calls

10. **Fixed duplicate function definitions (IN PROGRESS)**
    - Added `#-sbcl` guards to functions in reader.lisp, compiler.lisp, codegen.lisp, optimize.lisp, macho-utils.lisp
    - These files provide native (non-SBCL) versions of functions already defined in compiler-sbcl.lisp
    - ISSUE: macho.lisp had real implementations wrongly guarded with `#-sbcl` (fixed 2 of them)
    - ISSUE: codegen.lisp has duplicate definitions in `#+sbcl` block AND with `#-sbcl` guards
    - Remaining: cond-eq/ne/lt/ge/le/gt and cbz are defined twice in codegen.lisp

### ASDF Warnings - FIXED

All "redefining" warnings have been fixed:
1. `wrap-bytecode-with-heap-for-imports` - Removed stub from compiler-sbcl.lisp (real impl in macho.lisp)
2. `write-macho-executable-with-imports-and-heap` - Removed stub from compiler-sbcl.lisp
3. `cond-eq`, `cond-ne`, `cond-lt`, `cond-ge`, `cond-le`, `cond-gt`, `cbz` - Removed duplicates from codegen.lisp #+sbcl block (already defined in compiler-sbcl.lisp)

**Delivery Functions:**
- SBCL: Use `habu:deliver` (in compiler-sbcl.lisp) - 1-arg lift-lambdas
- Native: Use `deliver-v3` (in codegen.lisp, #-sbcl guard) - 2-arg lift-lambdas
- Reader compilation: WORKS (exit 42 test passes)

### Previous Bug Fixes

1. **and-imm silent failure (CRITICAL FIX)**
   - `and-imm` with unsupported immediates silently generated NOP
   - Caused heap misalignment in string-concat (crash at n=3+ recursive calls)
   - Fix: Added proper ARM64 logical immediate encoding for alignment masks
   - Masks supported: ~3, ~7, ~15, ~31 (now with correct immr=64-N rotation)
   - Now errors on unsupported immediates instead of silent NOP

2. **make-string-from-vector list handling (FIXED)**
   - Function crashed when given a list (tag 1) instead of vector (tag 3)
   - Branch offset calculation was correct but and-imm for tag check was NOP
   - Root cause was and-imm issue above

3. **nil/0 representation conflict (FIXED)**
   - nil and fixnum 0 had same representation (0), causing `(null 0)` = t
   - Fix: nil now has tag 6 (0x06), fixnum 0 is 0x00
   - Updated codegen.lisp, compiler.lisp

4. **Symbol registration for native code** (prev session)
   - Refactored `ensure-symbols-registered` into 18 small helpers
   - Added comma/unquote handling, empty symbol protection

5. **Octal literals in macho-utils.lisp**
   - Changed `#o755` to `#x1ED` (Habu reader doesn't support octal)

## File Structure

```
bootstrap/
  compiler-sbcl.lisp  - SBCL bootstrap compiler (5400+ lines)
  compiler.lisp       - Habu compiler (no SBCL dependencies)
  optimize.lisp       - Optimization passes (TCO)
  codegen.lisp        - ARM64 code generator (accumulator model)
  gc.lisp             - Garbage collector (Cheney's copying GC)
  reg-alloc.lisp      - Register allocation nanopasses
  macho.lisp          - Mach-O linker (#+sbcl versions)
  macho-utils.lisp    - Mach-O utilities (#-sbcl native versions)
  reader.lisp         - Habu reader

arm64/
  asm.lisp            - ARM64 instruction encoders
```

## Key Functions

```lisp
;; Compile and deliver program
(habu:deliver-v3 "(defun f (x) (* x 2)) (sys-exit (f 21))" "/tmp/out")

;; Individual steps
(read-all source-string)           ; Parse to S-expressions
(compile-program forms)            ; Compile to IR + defuns
(codegen ir rtaddrs fnoffs td)     ; Generate ARM64 code
(build-macho bytes imports)        ; Create Mach-O executable
```

## Tagged Value Representation

- Fixnum: `value << 4`, tag 0
- Cons: `pointer | 1`
- Symbol: `pointer | 2`
- Vector: `pointer | 3`
- String: `pointer | 4`
- Closure: `pointer | 5`
- Nil: `0x06` (tag 6) - distinct from fixnum 0

## ARM64 Register Usage

- x0-x7: Arguments and return value
- x20: Environment frame base
- x24: Closure environment pointer
- x26: Code base register
- x27: GC globals base (from_start, from_end, to_start, to_end)
- x28: Heap bump pointer

## Stack Frame Layout

```
sp+0x10:  x19, x20 (callee-saved)
sp+0x20:  x21, x22
sp+0x30:  x23, x24
sp+0x40:  temp slots (td*8)
sp+0x180: environment base
sp+0x240: spill area (td*64)
sp+0x3F0: x29 (fp), x30 (lr)
```

## Self-Hosting Status

**Achieved:**
- deliver-v3 compiles programs to native ARM64
- Closures work (4+ levels nested, 6+ captures)
- All 76 tests pass
- Stage 1 compiles and runs correctly

**Next Steps:**
1. Get Stage 1 to output Stage 2 binary
2. Verify fixed-point (Stage N == Stage N+1)

## Roadmap

See `docs/plans/MASTER_ROADMAP.md` for detailed implementation plan.

### Priority 1: Stack Overflow Fix - DONE
1. Rewrote read-list-elems to use `while` loop (iterative)
2. Implemented TCO (see item 6 in Key Improvements)

### Priority 2: TCO Implementation (Nanopass) - DONE
1. `apply-tco-to-function` identifies and transforms self-tail-calls
2. `codegen` emits `:tco-branch` markers, resolved in `resolve-tco-branches`
3. Tested with 100,000 recursive calls without stack overflow

### Priority 3: DWARF5 Debug Info
1. Generate line number tables for lldb debugging
2. Function symbol info for stack traces
3. Critical for debugging Stage 1/2 crashes

### Priority 4: Register Allocator - ARCHITECTURE DONE
Implemented in `bootstrap/reg-alloc.lisp` as 5 nanopasses:
1. **ir-to-tac**: Tree IR → Three-Address Code (linear, virtual registers)
2. **compute-liveness**: Backward dataflow analysis for live ranges
3. **compute-intervals**: Liveness info → (vreg, start, end) tuples
4. **linear-scan**: Intervals → allocation map (vreg → x9-x15 or spill)
5. **tac-codegen**: TAC + allocation → ARM64 (TODO)

Remaining: Implement tac-codegen to replace current accumulator codegen

### Priority 5: Native Garbage Collector - IMPLEMENTED
Cheney's copying collector with two semispaces:
1. **GC globals** in dedicated segment (from_start, from_end, to_start, to_end)
2. **gc_copy**: Copy single object to to-space, leave forwarding pointer
3. **gc_collect**: Flip semispaces, copy roots, scan Cheney queue
4. **GC triggers**: Inserted after all heap allocations (cons, string, closures)
5. See `docs/runtime/GC_NATIVE.md` for full documentation

**Allocation sites with GC triggers:**
- String literals (codegen.lisp:866)
- Cons cells (codegen.lisp:1324)
- Lambda-ref closures (codegen.lisp:1940, 1959)
- fn-ref-ir closures (codegen.lisp:1982)
- make-vector (codegen.lisp:1460)

**Remaining:** Test under GC stress, add triggers to build-captures cons chain

### Priority 6: Common Lisp `loop` Macro
1. Full CL loop spec implementation
2. Enables more idiomatic Lisp code

## Common Operations

```bash
# Compile and run a program
sbcl --load bootstrap/compiler-sbcl.lisp --load bootstrap/macho.lisp \
     --load bootstrap/reader.lisp --load bootstrap/compiler.lisp \
     --load bootstrap/codegen.lisp --load bootstrap/macho-utils.lisp \
     --eval '(habu:deliver-v3 "(sys-exit 42)" "/tmp/test")' --quit
/tmp/test && echo $?  # Should output 42
```

## Known Limitations

1. Max 8 arguments per function
2. 64KB file limit for native-read-file
3. No macros (uses reader macros)

## Debugging

- Exit 139 = SIGSEGV (check stack/spill)
- Exit 137 = SIGKILL (codesign issue on macOS)
- Use `--dynamic-space-size 4096` for large compilations
