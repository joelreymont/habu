# Habu Self-Hosting Lisp Compiler - Context

**Last Updated**: November 30, 2025
**Milestone**: Stage 1 can read, parse, and compile source files natively

## Current Status

The Habu compiler can compile programs with:
- Expressions: literals, arithmetic (+,-,*,/,mod), comparisons (=,<,>,<=,>=)
- Control flow: if, cond, when, unless, progn, and, or, not, setq
- Bindings: let, let*
- Functions: defun, labels, lambda, funcall, (function name)
- Closures: captured variables, higher-order functions
- Data: cons, car, cdr, cadr, caddr, list, quote, setcar, setcdr
- Predicates: null, consp, numberp, symbolp, stringp, vectorp
- Strings: string-length, string-ref, string-concat, string literals
- Symbols: symbol-name, make-symbol-from-string
- Vectors: make-vector, vector-ref, vector-set, vector-length
- File I/O: sys-open, sys-read, sys-write, sys-close, native-read-file
- System: sys-exit, get-intern-table, set-intern-table

**Test Results (76/76 pass):**
```
Comprehensive (43): add, mul, let, let*, if, defun, recursive, fact, fib, cond, when,
    unless, progn, and, or, not, cons/car/cdr, list, null, consp, numberp, comparisons,
    mod, labels, closures, higher-order functions

Self-hosting (15): mini-compiler, closure-compiler, symtab, stack-eval, tree-sum,
    cps-fact=120, map-reduce, nested-closures, four-level-closure, multi-closure,
    closure-captures-closure, five-captures, accumulator, mutual-closure, church-numerals

Edge cases (18): negative numbers, zero handling, deeply nested arithmetic, long cons
    chains, many let bindings, tail recursion, boolean chains, comparison chains,
    6-8 argument functions, 6+ captures, triple-lambda, call-twice, compose-lambdas
```

## Latest Session (November 30, 2025)

### Critical Bug Fix: STP Offset Encoding Overflow

**Root cause: STP/LDP 7-bit signed offset couldn't encode 0x3F0 (1008 bytes)!**

The prologue/epilogue used `stp x29, x30, [sp, #0x3F0]` to save fp/lr at the end of the 0x400 byte frame. But STP's immediate is 7-bit signed, scaled by 8:
- Range: -512 to +504 bytes
- 0x3F0 (1008) / 8 = 126 = 0x7E
- 0x7E in 7-bit signed = -2 (MSB set)
- Actual encoded offset: -2 × 8 = **-16 bytes**

So `stp x29, x30, [sp, #0x3F0]` actually stored at `sp - 16`, outside the frame!

This "worked" accidentally because sp-16 wasn't being overwritten. But when trying to fix to sp+0, the Stage 1 read-all test crashed because something else in the code overwrites sp+0..sp+15.

**Fix:** Use STR/LDR instead of STP/LDP for fp/lr:
- STR uses 12-bit unsigned offset (range 0 to 32760 bytes)
- `str x29, [sp, #0x3F0]` correctly encodes offset 1008
- `str x30, [sp, #0x3F8]` correctly encodes offset 1016

```lisp
;; Prologue
(str-offset 29 31 #x3F0)        ;; Save fp at sp+0x3F0
(str-offset 30 31 #x3F8)        ;; Save lr at sp+0x3F8

;; Epilogue
(ldr-offset 29 31 #x3F0)        ;; Restore fp from sp+0x3F0
(ldr-offset 30 31 #x3F8)        ;; Restore lr from sp+0x3F8
```

**Results:**
- All 15 basic tests: PASS
- Stage 1 read-all test: PASS (172KB source → 8.7MB native)
- Stage 1 now correctly reads files, parses S-expressions, and returns list length

### File I/O and String Handling Fixes

Added syscall codegen for file I/O operations:
- `sys-open-ir`: calls `_open(path, flags, mode)`
- `sys-write-ir`: calls `_write(fd, buf, len)`
- `sys-read-ir`: calls `_read(fd, buf, len)`
- `sys-close-ir`: calls `_close(fd)`

**Bug fixes:**

1. **64-bit load-addr**: `load-addr` only handled up to 48 bits, but packed string data (8 bytes) can require full 64-bit values. Added 4th movk instruction for 64-bit addresses.

2. **Null terminator**: String literals weren't null-terminated, causing C functions like `open()` to read garbage. Added automatic null byte to `gen-string-lit`.

3. **AND immediate encoding for tag clearing**: Fixed `(and-imm rd rn 1 60 3)` → `(and-imm rd rn 1 60 61)` for correct `~7` mask. The encoding `(imms=60, immr=3)` produced `0xE3...` instead of `0xF8...`.

**Results:**
- sys-open /dev/null: PASS
- sys-write stdout: PASS
- sys-write to file: PASS
- Stage 1 compiler: 170KB source → 8.7MB native, runs correctly (exit 42)

### Critical Bug Fix: Heap Alignment in make-vector

**Root cause: wrong AND (immediate) encoding caused heap corruption!**

The bug pattern: creating a closure corrupted outer let-bound vectors, even without calling the closure.

Investigation revealed:
- `make-vector` uses `(and-imm 1 1 1 #x3C #x3B)` for 16-byte alignment
- This produced mask `0xFFFFFFFFFFFFFFE3` (clears bits 2,3,4)
- Correct mask should be `0xFFFFFFFFFFFFFFF0` (clears bits 0,1,2,3)
- With wrong mask: `(16+15) & 0xE3 = 3` instead of 16 bytes allocated
- Subsequent heap allocations (closure cons cells) overwrote the vector!

**Fix:** Changed `(and-imm 1 1 1 #x3C #x3B)` to `(and-imm 1 1 1 59 60)`:
- N=1 (64-bit), imms=59 (60 ones), immr=60 (rotate left by 4)
- Produces correct mask `0xFFFFFFFFFFFFFFF0`

The ARM64 logical immediate encoding:
- Generate (imms+1) consecutive 1s, then rotate right by immr
- Old: imms=60, immr=59 gave wrong pattern `0xFFFFFFFFFFFFFFE3`
- Wrong attempt: immr=4 gave `0xF0FFFFFFFFFFFFFF` (rotates right, not left!)
- Correct: imms=59, immr=60 (rotate right by 60 = rotate left by 4)

### buffer-to-string-ir and native-read-file

Added `buffer-to-string-ir` to pure codegen for `native-read-file` support.

The implementation copies bytes from a sys-read buffer to a new string:
1. Evaluate buf and len arguments
2. Allocate string on heap with 16-byte alignment
3. Copy bytes in a loop using ldrb/strb
4. Tag result with string tag (0x4)

Used correct AND encoding `(and-imm 4 4 1 59 60)` for ~15 mask.

**Results:**
- native-read-file test: PASS (reads "hello", returns string-length=5)
- make-vector, vector-set/ref: PASS
- Stage 1 compiler (166KB): compiles and runs correctly

### Pure compile-program for Native Stage 1

Added `#-sbcl` version of `compile-program` in compiler.lisp for native use.

**Problem:** The SBCL version calls `reset-symbol-table`, which crashes in native
code because `*symbol-state*` is only defined with `#+sbcl` and never initialized.

**Fix:** Pure version skips reset-symbol-table and just calls compile-forms:
```lisp
#-sbcl
(defun compile-program (forms)
  (let* ((result (compile-forms forms))
         (defuns (car result))
         (mir (cadr result)))
    (cons mir defuns)))
```

**Stage 1 Self-Hosting Tests:**
- native-read-file: PASS (reads source from file)
- read-all: PASS (parses source to S-expressions)
- compile-forms: PASS (compiles to IR)
- compile-program: PASS (full compilation pipeline)

### Stage 1 Verification

Full Stage 1 compiler (reader + compiler + codegen + macho-utils) compiles and runs:
- Source size: 165KB (14KB reader + 59KB compiler + 89KB codegen + 2KB macho-utils)
- Native binary: 8.7MB
- Compilation time: ~1 second
- Test: `(+ 40 2)` returns 42 correctly

### Previous Fix: Stack Frame Layout (Spill/LR Collision)

**Root cause of nested let + labels crashes found and fixed!**

The crash pattern was: 1 let + labels works, 2 lets + labels crashes, 3 lets works.

Investigation revealed:
- `compile-labels` transforms `(go 42)` to `(funcall go FNTAB 42)` with TWO args
- With 2 nested lets + labels, td reaches 5 for the funcall
- spill-base(5) = 0x240 + 5*64 = 0x380
- Arg 1 stored at sp+0x388, which is where lr was saved!

Old prologue saved lr at sp-0x80 BEFORE frame allocation:
- After `sub sp, sp, #0x400`, lr was at sp + 0x388

**Fix:** Save fp/lr INSIDE the frame at sp+0x3F0 (after frame allocation):
```
Frame layout after fix:
sp+0x10:  x19, x20 (callee-saved)
sp+0x20:  x21, x22
sp+0x30:  x23, x24
sp+0x40:  temp slots (td*8)
sp+0x180: environment base (x20)
sp+0x240: spill area (td*64)
sp+0x3F0: x29 (fp), x30 (lr)  <- moved inside frame
```

### Previous Fix: Wrapper ADR Offset

**Root cause of ALL closure failures found and fixed!**

The wrapper code in `bootstrap/macho.lisp` had an incorrect ADR offset:
- ADR instruction at offset 36 was using `ADR x26, 40` → x26 = 76
- But code starts at offset 72 (18 instructions × 4 bytes)
- This caused x26 (code base register) to be 4 bytes too high
- Every funcall jumped to wrong address (1 instruction past actual function)

**Fix:** Changed `ADR 26 40` to `ADR 26 36` (72 - 36 = 36)

### Closure Tests Now Pass

After the ADR fix, all closure patterns work correctly:
- Lambda with no captures: PASS
- Lambda capturing let-bound variable: PASS
- Lambda capturing function parameter: PASS
- Labels capturing variables: PASS
- Nested closures: PASS
- Higher-order functions: PASS

### Stage 1 Compilation Complete

Full Stage 1 compiler compiles to native and runs:
1. reader.lisp compiles successfully
2. compiler.lisp compiles successfully
3. codegen.lisp compiles successfully
4. Combined (reader + compiler + codegen) compiles and runs: PASS

### Missing Comparison Handlers Fix

Added missing `cmp-le` and `cmp-ge` handlers in `bootstrap/codegen.lisp`:
- `<=` comparisons were not being generated (no handler for `cmp-le` IR)
- `>=` comparisons were also missing (no handler for `cmp-ge` IR)
- This caused recursive functions using `<=` to always take the then-branch
- Fixed by adding handlers using `cond-le` (13) and `cond-ge` (10) condition codes

### ARM64 Intrinsics Standardization

Refactored `bootstrap/macho.lisp` to use `arm64` package intrinsics:
- Removed duplicate local `arm64-*` functions
- Now uses `arm64:add`, `arm64:sub`, `arm64:ldr`, etc. with keyword args
- Single source of truth for instruction encoding in `arm64/asm.lisp`

### Previous Session Progress

1. **Pipe symbol fix** in reader.lisp - `|SYS:FOO|` style symbols
2. **fnoffs size calculation fix** - iterate until offset table stabilizes
3. **string-equal/assoc fix** - rewrote without closures

### Lambda Lifting and Closure Support

Added complete lambda lifting to `bootstrap/codegen.lisp`:

1. **Lambda Lifting**
   - `gensym-lambda`: generates unique names (LAMBDA-1, LAMBDA-2, etc.)
   - `lift-lambdas`: extracts lambda-ir nodes from IR
   - `lift-lambdas-from-defuns`: processes all defun bodies
   - `lambdas-to-defuns`: converts lambdas to defun format

2. **Closure Codegen**
   - `lambda-ref`: creates closure (fn-offset . captured-env) on heap
   - `fn-ref-ir`: for `(function name)` form
   - `funcall-ir`: loads closure, extracts fn-offset/env, calls with BLR

3. **Bug Fixes**
   - Spill slot collision: nested calls now use td-based spill areas
   - Each nesting level gets 64 bytes via `spill-base`
   - Added `get-tag` IR handler for type predicates
   - Added `and-imm` instruction encoder
   - Added `setq-ir` codegen for variable assignment (when/unless)
   - Added `mod` operator using a - (a/b)*b formula
   - **Nested lambda free-var analysis**: `find-free-vars` now descends into nested lambdas
   - **Lifted lambda capture loading**: `gen-capture-loads` loads captured values from x24
   - **Capture order fix**: removed erroneous `reverse` in `build-captures`

## File Structure

```
bootstrap/
  compiler-sbcl.lisp  - SBCL-hosted bootstrap compiler (5400+ lines)
  compiler.lisp       - Habu compiler (no SBCL dependencies)
  codegen.lisp        - ARM64 code generator
  macho.lisp          - Mach-O linker (SBCL version)
  macho-utils.lisp    - Mach-O utilities
  reader.lisp         - Habu reader
  optimize.lisp       - Nanopass optimizer

arm64/
  asm.lisp            - ARM64 instruction encoders
  codegen-sbcl.lisp   - Full ARM64 codegen with SBCL helpers
```

## Key Functions

### Compilation Pipeline

```lisp
;; Compile and deliver program
(habu:deliver-v3 "(defun f (x) (* x 2)) (sys-exit (f 21))" "/tmp/out")

;; Individual steps
(read-all source-string)           ; Parse to S-expressions
(compile-program-v3 forms)         ; Compile to IR + defuns
(codegen ir rtaddrs fnoffs td)     ; Generate ARM64 code
(build-macho bytes imports)        ; Create Mach-O executable
```

### Tagged Value Representation

- Fixnum: `value << 4`, tag 0
- Cons: `pointer | 1`
- Symbol: `pointer | 2`
- Vector: `pointer | 3`
- String: `pointer | 4`
- Closure: `pointer | 5`

### ARM64 Register Usage

- x0-x7: Arguments and return value
- x20: Environment frame base
- x24: Closure environment pointer
- x26: Code base register (for native executables)
- x28: Heap bump pointer

### Stack Frame Layout

```
sp+0:     saved fp/lr
sp+16:    saved x19/x20
sp+32:    saved x21-x24
sp+64:    temp slots (8-byte stride)
sp+0x240: spill slots (td-based offset: +64 per nesting level)
sp+0x180: environment variables
```

## Self-Hosting Status

**Achieved:**
- deliver-v3 compiles programs to native ARM64
- No SBCL dependencies in generated code
- Closures work: nested (4+ levels), multi-capture (6+), closure-captures-closure
- Higher-order: map, reduce, compose, apply-twice, church numerals
- CPS patterns work correctly (cps-fact = 120)
- All 15 self-hosting tests pass including complex compiler patterns

**Next Steps:**
1. Test compiler compiling itself
2. Verify fixed-point (Stage N == Stage N+1)
3. Complete FASL separate compilation

## Common Operations

```bash
# Run tests
sbcl --dynamic-space-size 4096 --script /tmp/test_basic.lisp

# Compile and run a program
sbcl --load bootstrap/compiler-sbcl.lisp --load bootstrap/macho.lisp \
     --load bootstrap/reader.lisp --load bootstrap/compiler.lisp \
     --load bootstrap/codegen.lisp --load bootstrap/macho-utils.lisp \
     --eval '(habu:deliver-v3 "(sys-exit 42)" "/tmp/test")' --quit
/tmp/test && echo $?  # Should output 42
```

## Known Limitations

1. **Max 8 arguments** per function in deliver-v3
2. **64KB file limit** for native-read-file (heap constraint)
3. **No macros** in compiler (uses reader macros for quote forms)

## Debugging

- Exit 139 = SIGSEGV (check stack frame, spill slots)
- Exit 137 = SIGKILL (check codesign on macOS)
- Use `--dynamic-space-size 4096` for large compilations

## Session History Reference

The full session history is preserved in SESSION.md (append-only log).
Key milestones:
- Nov 30: fnoffs size fix - compiler.lisp (78 defuns) compiles to native (commit 989bb6d)
- Nov 30: Pipe symbol handling in reader
- Nov 30: Closure bugs fixed - nested free-vars, capture loading, capture order (commit 6142a5f)
- Nov 30: Lambda lifting + closure support complete (commit ff63ccc)
- Nov 30: Defun support fixed
- Nov 29: Codegen created
- Nov 28: Partial self-hosting (compiler compiles itself to 1.6MB)
- Nov 27: Native file I/O via libSystem
- Nov 26: Native Mach-O linker created
