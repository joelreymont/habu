# Habu Self-Hosting Bootstrap Workflow

## Overview

The Habu compiler bootstraps to self-hosting using SBCL as the host compiler. The workflow is:

1. **SBCL loads Habu compiler** (written in Lisp)
2. **Habu compiler generates ARM64 machine code**
3. **Machine code is executed via JIT or written to executable**
4. **Eventually: Habu compiles itself**

## Current Status (November 22, 2025)

✅ **WORKING**: SBCL can load and run the Habu ARM64 compiler
✅ **WORKING**: Compiler generates correct ARM64 machine code
✅ **WORKING**: Runtime addresses are automatically discovered
✅ **TODO**: Execute generated code and verify results
✅ **TODO**: Self-compilation (Habu compiles Habu)

## Quick Start

### Compile a Simple Expression

```bash
sbcl --load run-habu.lisp --eval "(in-package :habu-sbcl)" \
     --eval "(compile-to-arm64 '(+ 2 3))" \
     --eval "(sb-ext:quit)"
```

Output:
```
[READY] Compiler definitions loaded in SBCL environment.
[HEXDUMP]
FD 7B BF A9 00 64 9D D2 FD 7B C1 A8 C0 03 5F D6
```

This is 16 bytes of ARM64 code that computes (+ 2 3) = 5 (tagged as 0x50).

### Files

- **run-habu.lisp**: Main entry point, loads compiler in SBCL
- **habu-arm64-codegen-sbcl.lisp**: SBCL-compatible Habu compiler
- **sbcl-habu-shim.lisp**: Compatibility shims for SBCL

### Architecture

```
┌────────────────────────────────────────┐
│ SBCL (Host Lisp)                        │
│  ├─ Loads: run-habu.lisp                │
│  ├─ Loads: habu-arm64-codegen-sbcl.lisp │
│  └─ Provides: compile-to-arm64          │
└────────────────────────────────────────┘
               │
               ▼
┌────────────────────────────────────────┐
│ Habu Compiler (Lisp code)               │
│  ├─ Parses Habu Lisp expressions        │
│  ├─ Generates IR                         │
│  ├─ Emits ARM64 machine code             │
│  └─ Returns: byte array                  │
└────────────────────────────────────────┘
               │
               ▼
┌────────────────────────────────────────┐
│ ARM64 Machine Code                      │
│  ├─ Function prologue                    │
│  ├─ Generated instructions               │
│  ├─ Function epilogue                    │
│  └─ Ready to execute                     │
└────────────────────────────────────────┘
```

## Bootstrap Process

### Phase 1: SBCL Compilation (CURRENT)

```lisp
;; Load the compiler
(load "run-habu.lisp")
(in-package :habu-sbcl)

;; Compile an expression
(compile-to-arm64 '(+ 2 3))
;; Returns: #(253 123 191 169 0 100 157 210 253 123 193 168 192 3 95 214)
```

### Phase 2: Compile Larger Programs

```lisp
;; Compile a function
(compile-to-arm64 '(defun factorial (n)
                     (if (= n 0)
                         1
                         (* n (factorial (- n 1))))))
```

### Phase 3: Self-Compilation

```lisp
;; Habu compiler compiles itself
(compile-to-arm64
  (read-file "habu-arm64-codegen.lisp"))
```

### Phase 4: Fixed Point

```
Stage 0: SBCL compiles Habu compiler → habu₀
Stage 1: habu₀ compiles Habu compiler → habu₁
Stage 2: habu₁ compiles Habu compiler → habu₂
Verify: habu₁ == habu₂ (bytecode identical)
```

When habu₁ == habu₂, we have achieved **self-hosting**!

## Runtime Integration

The compiler needs addresses of runtime functions (cons, car, cdr, etc.):

```lisp
;; Runtime addresses are automatically discovered:
[RUNTIME-ADDRS] ((HABU_CONS . 4336118560)
                 (HABU_CAR . 4336110636)
                 (HABU_CDR . 4336110728))
```

These are embedded in generated code for function calls.

## Next Steps

1. **Test JIT execution** of generated code
2. **Compile complete programs** (not just expressions)
3. **Self-compile** the Habu compiler
4. **Achieve fixed point** (compiler compiles itself identically)

## Current Limitations

- JIT execution needs testing/debugging
- Only expressions compile (not full programs yet)
- Self-compilation not yet attempted
- No executable generation (Mach-O/ELF)

## Success Criteria

- [x] SBCL loads Habu compiler
- [x] Compiler generates ARM64 code
- [x] Runtime addresses discovered
- [ ] Generated code executes correctly
- [ ] Compiler compiles itself
- [ ] Fixed point achieved

**Status**: ~75% complete to self-hosting!
