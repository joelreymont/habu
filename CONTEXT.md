# Habu Self-Hosting Lisp Compiler - Context

**Last Updated**: November 30, 2025
**Milestone**: Self-hosting primitives complete - setcar, strings, symbols

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
- System: sys-exit

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
- Nov 30: Closure bugs fixed - nested free-vars, capture loading, capture order (commit 6142a5f)
- Nov 30: Lambda lifting + closure support complete (commit ff63ccc)
- Nov 30: Defun support fixed
- Nov 29: Codegen created
- Nov 28: Partial self-hosting (compiler compiles itself to 1.6MB)
- Nov 27: Native file I/O via libSystem
- Nov 26: Native Mach-O linker created
