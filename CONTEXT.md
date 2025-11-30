# Habu Self-Hosting Lisp Compiler - Context

**Last Updated**: November 30, 2025
**Milestone**: 43 comprehensive + 7/8 self-hosting tests pass - closure bugs fixed

## Current Status

The pure Habu compiler can compile programs with:
- Expressions: literals, arithmetic (+,-,*,/,mod), comparisons (=,<,>,<=,>=)
- Control flow: if, cond, when, unless, progn, and, or, not, setq
- Bindings: let, let*
- Functions: defun, labels, lambda, funcall, (function name)
- Closures: captured variables, higher-order functions
- Data: cons, car, cdr, cadr, caddr, list, quote
- Predicates: null, consp, numberp, symbolp, stringp, vectorp
- Strings: string-length, string-ref
- Vectors: make-vector, vector-ref, vector-set, vector-length
- File I/O: sys-open, sys-read, sys-write, sys-close, native-read-file
- System: sys-exit

**Test Results (43/43 pass):**
```
Basic: add, mul, let, let-star, let-nested, if-true/false, defun, recursive, fact=120, fib=55
Self-hosting: mini-interp, mutual-rec, higher-order, closure, deep-rec
Closures: simple-closure, simple-lambda, inline-lambda
Additional: cond, when, unless, progn, and, or, not, cons/car/cdr/cadr/list
           null, consp, numberp, sub, div, mod, comparisons, labels
```

## Latest Session (November 30, 2025)

### Lambda Lifting and Closure Support

Added complete lambda lifting to `bootstrap/codegen-pure.lisp`:

1. **Lambda Lifting**
   - `pure-gensym-lambda`: generates unique names (LAMBDA-1, LAMBDA-2, etc.)
   - `pure-lift-lambdas`: extracts lambda-ir nodes from IR
   - `pure-lift-lambdas-from-defuns`: processes all defun bodies
   - `pure-lambdas-to-defuns`: converts lambdas to defun format

2. **Closure Codegen**
   - `lambda-ref`: creates closure (fn-offset . captured-env) on heap
   - `fn-ref-ir`: for `(function name)` form
   - `funcall-ir`: loads closure, extracts fn-offset/env, calls with BLR

3. **Bug Fixes**
   - Spill slot collision: nested calls now use td-based spill areas
   - Each nesting level gets 64 bytes via `pure-spill-base`
   - Added `get-tag` IR handler for type predicates
   - Added `pure-and-imm` instruction encoder
   - Added `setq-ir` codegen for variable assignment (when/unless)
   - Added `mod` operator using a - (a/b)*b formula
   - **Nested lambda free-var analysis**: `pure-find-free-vars` now descends into nested lambdas
   - **Lifted lambda capture loading**: `pure-gen-capture-loads` loads captured values from x24
   - **Capture order fix**: removed erroneous `pure-reverse` in `pure-build-captures`

## File Structure

```
bootstrap/
  compiler.lisp       - Main SBCL-hosted compiler (5400+ lines)
  compiler-pure.lisp  - Pure Habu compiler (no SBCL dependencies)
  codegen-pure.lisp   - Pure ARM64 code generator
  macho.lisp          - Mach-O linker (SBCL version)
  macho-pure.lisp     - Pure Mach-O linker
  reader-pure.lisp    - Pure Habu reader
  optimize.lisp       - Nanopass optimizer

arm64/
  asm.lisp            - ARM64 instruction encoders
  codegen-sbcl.lisp   - Full ARM64 codegen with SBCL helpers
```

## Key Functions

### Compilation Pipeline

```lisp
;; Compile and deliver program
(habu:pure-deliver-v3 "(defun f (x) (* x 2)) (sys-exit (f 21))" "/tmp/out")

;; Individual steps
(pure-read-all source-string)           ; Parse to S-expressions
(pure-compile-program-v3 forms)         ; Compile to IR + defuns
(pure-codegen ir rtaddrs fnoffs td)     ; Generate ARM64 code
(pure-build-macho bytes imports)        ; Create Mach-O executable
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
- Pure-deliver-v3 compiles programs to native ARM64
- No SBCL dependencies in generated code
- Closures and higher-order functions work (including nested closures)
- Mutual recursion and labels work
- CPS patterns work correctly (cps-fact = 120)
- Self-hosting tests: 7/8 pass (mini-compiler, closure-compiler, symtab, tree-sum, cps-fact, map-reduce, nested-closures)

**Next Steps:**
1. Test pure compiler compiling itself
2. Verify fixed-point (Stage N == Stage N+1)
3. Complete FASL separate compilation

## Common Operations

```bash
# Run tests
sbcl --dynamic-space-size 4096 --script /tmp/test_pure_basic.lisp

# Compile and run a program
sbcl --load bootstrap/compiler.lisp --load bootstrap/macho.lisp \
     --load bootstrap/reader-pure.lisp --load bootstrap/compiler-pure.lisp \
     --load bootstrap/codegen-pure.lisp --load bootstrap/macho-pure.lisp \
     --eval '(habu:pure-deliver-v3 "(sys-exit 42)" "/tmp/test")' --quit
/tmp/test && echo $?  # Should output 42
```

## Known Limitations

1. **Max 8 arguments** per function in pure-deliver-v3
2. **64KB file limit** for native-read-file (heap constraint)
3. **No macros** in pure compiler (uses reader macros for quote forms)

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
- Nov 29: Pure codegen created
- Nov 28: Partial self-hosting (compiler compiles itself to 1.6MB)
- Nov 27: Native file I/O via libSystem
- Nov 26: Native Mach-O linker created
