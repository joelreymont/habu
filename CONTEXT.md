# Habu Self-Hosting Lisp Compiler - Context

**Last Updated**: November 30, 2025
**Milestone**: LAMBDA/CLOSURE SUPPORT COMPLETE - All 19 pure-deliver-v3 tests pass!

## Current Status

The pure Habu compiler can compile programs with:
- Expressions: literals, arithmetic (+,-,*,/,mod), comparisons (=,<,>,<=,>=)
- Control flow: if, cond, when, unless, progn, and, or, not
- Bindings: let, let*
- Functions: defun, labels, lambda, funcall, (function name)
- Closures: captured variables, higher-order functions
- Data: cons, car, cdr, cadr, caddr, list, quote
- Predicates: null, consp, numberp, symbolp, stringp, vectorp
- Strings: string-length, string-ref
- Vectors: make-vector, vector-ref, vector-set, vector-length
- File I/O: sys-open, sys-read, sys-write, sys-close, native-read-file
- System: sys-exit

**Test Results (19/19 pass):**
```
Basic: add=30, mul=42, let=42, let-star=5, let-nested=42, if-true/false=42
       defun=42, recursive=42, fact=120, fib=55
Self-hosting: mini-interp=23, mutual-rec=1, higher-order=12, closure=42, deep-rec=55
Closures: simple-closure=42, simple-lambda=42, inline-lambda=42
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
- Closures and higher-order functions work
- Mutual recursion and labels work

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
- Nov 30: Lambda lifting + closure support complete (commit ff63ccc)
- Nov 30: Defun support fixed
- Nov 29: Pure codegen created
- Nov 28: Partial self-hosting (compiler compiles itself to 1.6MB)
- Nov 27: Native file I/O via libSystem
- Nov 26: Native Mach-O linker created
