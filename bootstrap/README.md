# Habu Bootstrap Directory

**Note:** This directory previously contained a hand-written C-based bootstrap compiler. That approach has been deprecated in favor of the correct Lisp-based bootstrap path.

## Correct Bootstrap Approach

Per `AGENTS.md`: "Bootstrapping should be done in Lisp, compiled by SBCL. You do not use C for anything but the tiny C runtime, there should be no C backends!"

### The Lisp-Based Bootstrap Path

```
Stage 0: SBCL Host Compiler
  ├─ Load habu-arm64-codegen.lisp in SBCL
  ├─ Compile Habu programs → ARM64 machine code
  ├─ Link with tiny C runtime (runtime/*.c)
  └─ Execute via habu-jit.c helper

Stage 1: Partial Self-Hosting
  ├─ Compile core compiler functions with SBCL-hosted compiler
  ├─ Link compiled ARM64 code with runtime
  └─ Create habu-compiler-stage1

Stage 2: Full Self-Hosting
  ├─ Stage1 compiles entire compiler → Stage2
  ├─ Stage2 compiles entire compiler → Stage3
  ├─ Verify: Stage2 == Stage3 (fixed point)
  └─ Self-hosting achieved!
```

## Key Files

**Lisp Compiler:**
- `../habu-arm64-codegen.lisp` - Main compiler (generates ARM64 directly)
- `../habu-arm64-codegen-sbcl.lisp` - SBCL stub for testing
- `../habu-repl.lisp` - REPL implementation
- `../stdlib.lisp` - Standard library

**Tiny C Runtime (only C we use):**
- `../runtime/runtime.c` - Core runtime (cons, car, cdr, GC, etc.)
- `../runtime/gc.c` - Garbage collector
- `../runtime/io.c` - I/O operations
- `../runtime/lineedit.c` - Line editing for REPL
- `../runtime/region.c` - Memory management
- `../habu-jit.c` - Tiny JIT helper (mmap/mprotect/execute)

## Running the Compiler

### In SBCL (Development)

```bash
# Load the compiler
sbcl --load habu-arm64-codegen-sbcl.lisp

# Compile a simple program
(defvar *code* (compile-to-arm64 '(+ 5 7)))

# Execute via JIT (if habu-jit helper is built)
# (Implementation in progress)
```

### Self-Hosted (Future)

```bash
# Stage 1 compiler compiles programs
./habu-stage1 program.lisp

# Stage 2+ (self-hosting achieved)
./habu program.lisp
```

## What Was Removed

The following C-based bootstrap implementation was removed as it contradicts the project's design:
- `primitives.c` - Hand-written ARM64 primitives
- `encoders.c` - ARM64 instruction encoders
- `ir-generation.c` - IR generation in C
- `code-generation.c` - Code generation in C
- `reader.c` - S-expression reader in C
- `runtime-minimal.c` - Minimal runtime
- `habu-bootstrap.c` - Main driver
- `tests/*.c` - C-based tests
- `Makefile` - Build system for C compiler

All test files preserved as `.lisp` files in this directory and tests/.

## Documentation

See `../CLEANUP_AND_SELF_HOSTING_PLAN.md` for:
- Detailed rationale for removing C bootstrap
- Complete roadmap to self-hosting (2-3 weeks)
- Full Lisp spec implementation plan (2-6 months)

## Timeline

- **Phase 0:** Cleanup (COMPLETE)
- **Phase 1:** SBCL bootstrap (1-2 days)
- **Phase 2:** Runtime integration (2-3 days)
- **Phase 3:** Language completeness (1-2 weeks) - let bindings, closures, macros
- **Phase 4:** Self-compilation (3-5 days)

**Total to self-hosting: 2-3 weeks**

## Design Principles

1. **No C backends** - Generate ARM64 machine code directly
2. **Tiny C runtime only** - Only for system calls and low-level operations
3. **Lisp-based bootstrap** - SBCL compiles Habu compiler
4. **Self-hosting goal** - Compiler compiles itself
5. **Full Lisp spec** - Eventually ANSI Common Lisp compliant

---

**For the complete implementation plan, see:**
- `../CLEANUP_AND_SELF_HOSTING_PLAN.md`
- `../AGENTS.md` - Project constraints and guidelines
- `../CONTEXT.md` - Current session status
