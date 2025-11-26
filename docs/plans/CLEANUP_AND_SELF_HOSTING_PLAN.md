# Habu Cleanup and Self-Hosting Plan

**Date:** November 22, 2025
**Goal:** Remove unnecessary C files, establish pure Lisp-based bootstrap path, and define roadmap to self-hosting with full Lisp spec

---

## Current State Analysis

### What We Have

1. **Lisp-based compiler** (`habu-arm64-codegen.lisp`):
   - Generates ARM64 machine code directly
   - 97% complete for core features
   - Runs in SBCL during bootstrap
   - Designed to eventually run in native Habu

2. **C-based bootstrap compiler** (`bootstrap/*.c`):
   - Hand-written ARM64 code generators
   - Primitives, encoders, IR generation, code generation
   - 73/73 tests passing
   - **NOT THE INTENDED PATH** per AGENTS.md

3. **Tiny C runtime** (`runtime/*.c`):
   - runtime.c: Core runtime functions (cons, car, cdr, etc.)
   - gc.c: Garbage collector
   - io.c: I/O operations
   - lineedit.c: Line editing for REPL
   - region.c: Memory regions
   - **THIS IS THE ONLY C WE SHOULD KEEP**

4. **JIT helper** (`habu-jit.c`):
   - Tiny helper for mmap/mprotect/execute
   - Optional, provides JIT execution
   - **KEEP THIS**

### The Problem

According to `AGENTS.md`:
- "You do not use C for anything but the tiny C runtime, there should be no C backends!"
- "Bootstrapping should be done in Lisp, compiled by SBCL."

We have:
- A full C-based bootstrap compiler in `bootstrap/*.c`
- Many test C files in root directory
- Old C backend code
- This contradicts the stated approach

---

## Cleanup Plan

### Files to Remove

#### 1. C-Based Bootstrap Compiler (bootstrap/*.c)
```
bootstrap/primitives.c
bootstrap/encoders.c
bootstrap/ir-generation.c
bootstrap/code-generation.c
bootstrap/reader.c
bootstrap/runtime-minimal.c
bootstrap/habu-bootstrap.c
bootstrap/tests/test-*.c
```

**Reason:** The bootstrap path should be Lisp-based (SBCL compiling Habu compiler), not C-based.

#### 2. Root Directory Test C Files
```
test-*.c (all test files in root)
debug-*.c (all debug files)
habu-rec.c
habu-prog.c
habu-extended.c
habu-enhanced.c
habu-exec.c (replaced by Lisp-based approach)
demo-all-features.c
standalone-compiler.c
ir-to-asm*.c
jit-executor.c
bytes-to-executable.c (utility, can keep if needed)
```

**Reason:** Tests should be Lisp-based, not C-based.

#### 3. Generated Binaries
```
bootstrap/habu-bootstrap
bootstrap/test-*
bootstrap/tests/test-*
*.o files
All executables from test-*.c files
```

**Reason:** Generated artifacts, not source code.

#### 4. Old Backend Code (if any remains)
```
c-backend.lisp references in bootstrap/
```

**Reason:** We generate machine code, not C.

### Files to Keep

#### Tiny C Runtime (runtime/)
```
runtime/runtime.c    ✓ Core runtime functions
runtime/gc.c         ✓ Garbage collector
runtime/io.c         ✓ I/O operations
runtime/lineedit.c   ✓ REPL line editing
runtime/region.c     ✓ Memory regions
runtime/habu.h       ✓ Header file
runtime/object.h     ✓ Object definitions
runtime/*.o          ✗ (generated, will be rebuilt)
```

#### JIT Helper
```
habu-jit.c           ✓ Tiny JIT mmap/exec helper
```

#### All Lisp Files
```
habu-arm64-codegen.lisp          ✓ Main compiler
habu-arm64-codegen-sbcl.lisp     ✓ SBCL stub for testing
habu-repl.lisp                   ✓ REPL
stdlib.lisp                      ✓ Standard library
All test-*.lisp files            ✓ Lisp-based tests
runtime/*.lisp                   ✓ Runtime library in Lisp
```

---

## Correct Bootstrap Path: Lisp-Based

### Stage 0: SBCL Host Compiler

```
┌─────────────────────────────────────┐
│  1. Start with SBCL                  │
│                                       │
│  2. Load habu-arm64-codegen.lisp     │
│     - Compiles Lisp → IR → ARM64     │
│     - Runs in SBCL environment       │
│     - Uses SBCL's features           │
│                                       │
│  3. Compile simple Habu programs     │
│     - Generate ARM64 bytecode        │
│     - Link with tiny C runtime       │
│     - Execute via JIT                │
└─────────────────────────────────────┘
```

### Stage 1: Self-Compile Core Functions

```
┌─────────────────────────────────────┐
│  1. Use SBCL-based compiler to       │
│     compile core compiler functions  │
│     - compile-expr                   │
│     - codegen-expr                   │
│     - ARM64 encoders                 │
│                                       │
│  2. Link compiled functions with     │
│     tiny C runtime                   │
│                                       │
│  3. Create habu-compiler-stage1      │
│     - Partially self-hosted          │
│     - Can compile simple programs    │
└─────────────────────────────────────┘
```

### Stage 2: Full Self-Hosting

```
┌─────────────────────────────────────┐
│  1. habu-compiler-stage1 compiles    │
│     entire habu-arm64-codegen.lisp   │
│     → habu-compiler-stage2           │
│                                       │
│  2. habu-compiler-stage2 compiles    │
│     habu-arm64-codegen.lisp          │
│     → habu-compiler-stage3           │
│                                       │
│  3. Verify fixed point:              │
│     stage2 bytecode == stage3        │
│                                       │
│  4. 🎉 SELF-HOSTING ACHIEVED          │
└─────────────────────────────────────┘
```

---

## Detailed Self-Hosting Roadmap

### Phase 0: Cleanup (1-2 hours)

**Tasks:**
1. Remove all unnecessary C files
2. Update Makefile to only build tiny runtime + JIT helper
3. Update .gitignore for *.o, binaries
4. Clean build artifacts
5. Commit: "Remove C-based bootstrap, keep only tiny runtime"

**Success Criteria:**
- Only runtime/*.c and habu-jit.c remain
- Project builds tiny runtime library
- All Lisp files intact

### Phase 1: Establish SBCL Bootstrap (1-2 days)

**Tasks:**
1. Verify habu-arm64-codegen.lisp loads in SBCL
2. Fix any SBCL compatibility issues:
   - Hex literals (#x not 0x)
   - Reader macros
   - Platform differences
3. Create run-compiler.lisp:
   - Loads compiler
   - Compiles test programs
   - Generates ARM64 bytecode files
4. Link generated bytecode with runtime
5. Execute via habu-jit helper

**Success Criteria:**
- Can compile simple programs in SBCL
- Generated ARM64 code executes correctly
- Tests pass: arithmetic, conditionals, functions

**Current Status:** ~80% done
- Compiler loads in SBCL (via habu-arm64-codegen-sbcl.lisp)
- Some functionality working
- Needs: real codegen (not stub)

### Phase 2: Runtime Integration (2-3 days)

**Tasks:**
1. Export all runtime functions from runtime.c:
   - cons, car, cdr, set-car!, set-cdr!
   - make-vector, vector-ref, vector-set!
   - make-string, string-ref, string-length
   - make-symbol, symbol-name, intern
   - print, read, write
   - GC hooks
2. Create runtime-addresses.lisp:
   - Load runtime library
   - Get function addresses
   - Build address table
3. Thread runtime addresses through compiler:
   - Pass to compile-to-arm64
   - Use in BLR instructions
   - All runtime calls use real addresses
4. Test end-to-end with real runtime

**Success Criteria:**
- All runtime functions accessible from compiled code
- cons/car/cdr work via generated code
- Strings, vectors, symbols work
- GC integrates with compiled code

### Phase 3: Language Completeness (1-2 weeks)

#### Milestone 3.1: Missing Core Features

**Let bindings with stack:**
- Implement stack-based variable storage
- Generate LDR/STR for locals
- Test nested let, let*

**Function calls and recursion:**
- Calculate BL offsets (DONE per CONTEXT.md)
- Handle varargs
- Test factorial, fibonacci

**Closures:**
- Closure representation (code ptr + environment)
- Free variable capture
- Environment chain
- Test nested closures, higher-order functions

#### Milestone 3.2: Control Flow

**Advanced control:**
- block/return-from (non-local exits)
- tagbody/go (labeled jumps)
- catch/throw (exception handling)
- unwind-protect (cleanup)

**Code generation:**
- Label tables
- Exit point tracking
- Stack unwinding

#### Milestone 3.3: Macros

**Macro system:**
- defmacro
- macroexpand, macroexpand-1
- quasiquote, unquote, unquote-splicing
- Macro table in environment

**Test:**
- when, unless, and, or as macros
- Rewrite cond using macros

### Phase 4: Self-Compilation (3-5 days)

#### Milestone 4.1: Compile Compiler Functions

**Tasks:**
1. Start with simple functions:
   - encode-word
   - arm64-movz, arm64-add, etc.
2. Compile IR generation:
   - compile-expr
   - compile-let
   - compile-if
3. Compile code generation:
   - codegen-expr
   - codegen-binary-op
   - codegen-if, codegen-let

**Success Criteria:**
- Each compiler function compiles successfully
- Compiled functions produce same output as SBCL version
- Tests pass for compiled compiler functions

#### Milestone 4.2: Partial Self-Hosting

**Tasks:**
1. Link compiled compiler functions with runtime
2. Create habu-compiler-stage1 executable
3. Use stage1 to compile test programs
4. Verify output matches SBCL-compiled version

**Success Criteria:**
- stage1 can compile simple programs
- Factorial, fibonacci compile and run
- Output identical to SBCL-compiled version

#### Milestone 4.3: Full Bootstrap

**Tasks:**
1. Use stage1 to compile entire habu-arm64-codegen.lisp
2. Generate habu-compiler-stage2
3. Use stage2 to compile habu-arm64-codegen.lisp
4. Generate habu-compiler-stage3
5. Binary diff: verify stage2 == stage3

**Success Criteria:**
- Fixed point achieved
- Compiler can compile itself repeatedly
- Generated binaries bit-identical
- 🎉 **SELF-HOSTING COMPLETE**

### Phase 5: Full Lisp Spec (2-6 months)

#### Data Structures (2-3 weeks)

**Symbols:**
- Symbol table, intern
- Property lists
- Symbol-value, symbol-function

**Strings:**
- UTF-8 support
- String operations
- Efficient representation

**Vectors:**
- Adjustable arrays
- Fill pointers
- Multi-dimensional arrays

**Hash tables:**
- eq, eql, equal, equalp
- Rehashing
- Performance

**Packages:**
- defpackage, in-package
- export, import, use-package
- Symbol visibility

#### Numeric Tower (2-3 weeks)

**Integer types:**
- fixnum (current)
- bignum (arbitrary precision)
- Auto-promotion

**Rational numbers:**
- ratio type
- Exact fractions
- Auto-reduction

**Floating point:**
- single-float, double-float
- IEEE 754 compliance
- Special values (NaN, ±Inf)

**Complex numbers:**
- Complex representation
- Arithmetic
- Conversions

**Generic arithmetic:**
- Type coercion rules
- Mixed-type operations
- Numeric predicates

#### I/O System (1-2 weeks)

**Streams:**
- Stream protocol
- File streams
- String streams
- Bidirectional streams

**Reader:**
- Read-time evaluation
- Reader macros
- Dispatch macros
- Radix control (*read-base*)

**Printer:**
- Print-object protocol
- Circular structure detection
- Pretty printer
- *print-circle*, *print-level*, *print-length*

**Format:**
- Format directives
- ~A, ~S, ~D, ~F, etc.
- Iteration ~{...~}
- Conditionals ~[...~]

#### Condition System (1-2 weeks)

**Conditions:**
- Condition hierarchy
- define-condition
- signal, error, warn

**Handlers:**
- handler-bind
- handler-case
- ignore-errors

**Restarts:**
- restart-case
- restart-bind
- invoke-restart

#### CLOS Basics (2-4 weeks)

**Classes:**
- defclass
- make-instance
- Slot access

**Generic functions:**
- defgeneric
- defmethod
- Method combination

**Method dispatch:**
- Specializers
- Method precedence
- Effective methods

#### Optimization (2-3 weeks)

**Compiler optimizations:**
- Constant folding
- Dead code elimination
- Inlining (small functions)
- Tail call optimization (DONE)

**Register allocation:**
- Liveness analysis
- Graph coloring
- Spill handling

**Peephole:**
- Instruction combining
- Redundant load/store elimination
- Branch optimization

---

## Implementation Priorities

### Critical Path to Self-Hosting (2-3 weeks)

1. **Phase 0:** Cleanup (1-2 hours)
2. **Phase 1:** SBCL bootstrap (1-2 days)
3. **Phase 2:** Runtime integration (2-3 days)
4. **Phase 3.1:** Let bindings, recursion (2-3 days)
5. **Phase 3.1:** Closures (2-3 days)
6. **Phase 4:** Self-compilation (5-7 days)

**Total: 2-3 weeks to self-hosting**

### Post-Self-Hosting Features

Everything in Phase 5 can be done after self-hosting:
- Numeric tower
- Full I/O
- Condition system
- CLOS
- Optimization

---

## Architecture: Pure Lisp with Tiny C Runtime

### What Runs Where

**In SBCL (Stage 0):**
- habu-arm64-codegen.lisp (compiler)
- Generates ARM64 bytecode
- Uses SBCL for list processing, etc.

**In Generated ARM64 Code:**
- Compiled Habu programs
- Eventually: compiled compiler itself
- Calls tiny C runtime for:
  - Memory allocation (cons, make-vector, etc.)
  - GC
  - I/O
  - Line editing

**Tiny C Runtime (runtime/):**
- Minimal implementation
- Only what can't be done in Lisp:
  - mmap, mprotect (memory management)
  - System calls (read, write, open, etc.)
  - Platform-specific code
- Everything else in Lisp!

**No C Backend:**
- Compiler generates ARM64 directly
- No intermediate C code
- Machine code → JIT execute → results

---

## Testing Strategy

### Unit Tests (Lisp)
- test-arithmetic.lisp
- test-comparisons.lisp
- test-control-flow.lisp
- test-let-bindings.lisp
- test-functions.lisp
- test-closures.lisp

### Integration Tests (Lisp)
- Compile + execute simple programs
- Verify results
- Test with real runtime

### Bootstrap Tests
- Stage1 compiles test suite
- Stage2 compiles test suite
- Results identical

### Compliance Tests
- ANSI Common Lisp test suite
- Track coverage
- Document deviations

---

## Success Metrics

### Immediate (Phase 0-2)
- [ ] Only tiny runtime C code remains
- [ ] Lisp compiler generates working ARM64 code
- [ ] Runtime integration complete
- [ ] Test suite passes

### Short-term (Phase 3-4)
- [ ] All core language features work
- [ ] Compiler can compile itself
- [ ] Fixed point achieved
- [ ] Self-hosting complete

### Long-term (Phase 5)
- [ ] Full numeric tower
- [ ] Complete I/O system
- [ ] Condition system
- [ ] CLOS subset
- [ ] 80%+ ANSI CL compliance

---

## Timeline Summary

| Phase | Duration | Goal |
|-------|----------|------|
| 0. Cleanup | 1-2 hours | Remove C backend artifacts |
| 1. SBCL Bootstrap | 1-2 days | Compiler runs in SBCL, generates ARM64 |
| 2. Runtime Integration | 2-3 days | Full runtime function access |
| 3. Language Complete | 1-2 weeks | Let, closures, macros, control flow |
| 4. Self-Hosting | 3-5 days | Compiler compiles itself |
| **SELF-HOSTING TOTAL** | **2-3 weeks** | **Fixed point achieved** |
| 5. Full Spec | 2-6 months | Numeric tower, I/O, CLOS, optimization |

---

## Next Steps

1. **Confirm approach:** Review this plan, adjust as needed
2. **Start cleanup:** Remove unnecessary C files
3. **Verify SBCL path:** Ensure compiler loads and works
4. **Runtime integration:** Get all runtime functions accessible
5. **Close remaining gaps:** Let bindings, closures
6. **Self-compile:** Achieve fixed point
7. **Celebrate:** Self-hosting achieved!
8. **Full spec:** Work through Phase 5 systematically

---

**The key insight:** We already have 97% of what we need in the Lisp compiler. The C-based bootstrap was a detour. Remove it, focus on the Lisp path, and we're 2-3 weeks from self-hosting.
