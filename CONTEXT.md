# Current Session Context

**Date**: November 22, 2025
**Session Duration**: Current session
**Status**: MAJOR REFACTORING - Removed C-based bootstrap, established pure Lisp path to self-hosting

---

## Current Session (November 22, 2025)

### Session Summary

**CRITICAL DISCOVERY**: The project had two conflicting bootstrap paths:
1. C-based bootstrap compiler (bootstrap/*.c) - 73/73 tests passing
2. Lisp-based compiler (habu-arm64-codegen.lisp) - 97% complete

**Per AGENTS.md**: "You do not use C for anything but the tiny C runtime, there should be no C backends!"

The C-based bootstrap was a detour. Correct path: SBCL compiles Habu compiler → generates ARM64 → tiny C runtime.

### Completed Tasks

1. **Architecture Analysis** (COMPLETED)
   - Read AGENTS.md, CONTEXT.md, all planning docs
   - Identified dual bootstrap paths
   - Confirmed Lisp-based approach is correct
   - C bootstrap contradicts project requirements

2. **Created Comprehensive Cleanup Plan** (COMPLETED)
   - Document: CLEANUP_AND_SELF_HOSTING_PLAN.md
   - Identifies all C files to remove
   - Defines correct Lisp-based bootstrap path
   - Roadmap: 2-3 weeks to self-hosting
   - Full spec: 2-6 months post-self-hosting

3. **Cleanup Implementation** (COMPLETED)
   - Removed bootstrap/*.c (entire C compiler) ✓
   - Removed test-*.c in root (50+ test files) ✓
   - Removed debug-*.c files ✓
   - Removed habu-*.c (except habu-jit.c) ✓
   - Kept only: runtime/*.c + habu-jit.c ✓
   - Updated bootstrap/README.md ✓
   - Removed bootstrap Makefiles ✓
   - Updated .gitignore ✓

4. **Phase 1 Bootstrap Infrastructure** (COMPLETED)
   - SBCL stub compiler loads and works ✓
   - Generates ARM64 bytecode (16 bytes for literals) ✓
   - Tiny C runtime built (runtime/*.c) ✓
   - JIT helper built (libhabu-jit.dylib/so) ✓
   - Runtime address printer working (27 functions exported) ✓
   - Created end-to-end pipeline:
     * compile-and-save.lisp: Compile Lisp → ARM64 bytecode file
     * run-bytecode: Execute bytecode via JIT
   - First successful JIT execution: literal 42 executes and returns ✓

### Key Insights

**What We Keep:**
- `runtime/runtime.c` - Core runtime (cons, car, cdr, GC)
- `runtime/gc.c` - Garbage collector
- `runtime/io.c` - I/O operations
- `runtime/lineedit.c` - REPL line editing
- `runtime/region.c` - Memory management
- `habu-jit.c` - Tiny JIT helper (mmap/mprotect/execute)
- All `.lisp` files

**What We Removed:**
- `bootstrap/*.c` - Hand-written C compiler (NOT the intended path)
- `bootstrap/tests/*.c` - C-based tests
- All `test-*.c` in root directory
- All `debug-*.c` files
- `habu-rec.c`, `habu-prog.c`, `habu-exec.c`, `standalone-compiler.c`
- Generated binaries and object files

**The Correct Bootstrap Path:**

```
Stage 0: SBCL Host
  ├─ Load habu-arm64-codegen.lisp in SBCL
  ├─ Compile Lisp programs → ARM64 machine code
  ├─ Link with tiny C runtime
  └─ Execute via habu-jit.c helper

Stage 1: Partial Self-Hosting
  ├─ Compile core compiler functions with SBCL
  ├─ Link compiled functions
  └─ Create habu-compiler-stage1

Stage 2: Full Self-Hosting
  ├─ Stage1 compiles entire compiler → Stage2
  ├─ Stage2 compiles entire compiler → Stage3
  ├─ Verify: Stage2 == Stage3 (fixed point)
  └─ 🎉 SELF-HOSTING ACHIEVED
```

### Timeline to Self-Hosting

**Phase 0: Cleanup** (1-2 hours) ✅ COMPLETE
- Remove C backend artifacts
- Keep only tiny runtime
- Update build system

**Phase 1: SBCL Bootstrap** (1-2 days) ✅ INFRASTRUCTURE COMPLETE
- Verify compiler loads in SBCL ✓
- Generate ARM64 code ✓
- Runtime integration - IN PROGRESS
  * Runtime built and exports 27 functions ✓
  * Need to enhance stub compiler for functional code

**Phase 2: Language Gaps** (1-2 weeks)
- Stack-based let bindings
- Closures
- Macros
- Control flow (block/return, catch/throw)

**Phase 3: Self-Compile** (3-5 days)
- Compile compiler functions
- Bootstrap stages
- Fixed point

**Total: 2-3 weeks to self-hosting**

### Phase 1 Bootstrap Progress

**Completed:**
1. ✓ SBCL stub compiler loads and works
2. ✓ Generates ARM64 bytecode (16 bytes for literals)
3. ✓ Tiny C runtime built (runtime/*.c)
4. ✓ JIT helper built (libhabu-jit.dylib/so)
5. ✓ Runtime address printer working (27 functions exported)
6. ✓ Created end-to-end pipeline:
   - compile-and-save.lisp: Compile Lisp → ARM64 bytecode file
   - run-bytecode: Execute bytecode via JIT
7. ✓ First successful JIT execution: literal 42 executes and returns

**Issues Found:**
- Stub compiler returns untagged values (42 instead of 42 << 4)
- Stub generates same code for all expressions (placeholder)
- Need to enhance stub or use real compiler

### Next Steps

**Immediate (Next Session):**
1. Enhance SBCL stub compiler to generate functional code
2. Implement proper fixnum tagging in generated code
3. Add runtime function calls (cons, car, cdr)
4. Test arithmetic operations
5. Test conditionals
6. Expand to closures, let bindings, macros

**End-to-End Pipeline Working:**
```bash
# Compile Lisp expression to ARM64 bytecode
$ sbcl --script compile-and-save.lisp "42"
Compiling: 42
Wrote 16 bytes to output.bin

# Execute bytecode via JIT
$ ./run-bytecode output.bin
Read 16 bytes of ARM64 bytecode
Executing bytecode...
Raw result: 0x2a (42)
```

### Files Created This Session

**Planning and Documentation:**
- `CLEANUP_AND_SELF_HOSTING_PLAN.md` - Comprehensive cleanup and roadmap

**Build and Execution Infrastructure:**
- `test-sbcl-compiler.lisp` - Test suite for SBCL stub compiler
- `compile-and-save.lisp` - Compile Lisp → ARM64 bytecode file
- `run-bytecode.c` / `run-bytecode` - JIT executor for bytecode files
- `output.bin` - Sample compiled bytecode (literal 42)

### Files Modified This Session

- `CONTEXT.md` - Updated with cleanup and bootstrap progress (this file)
- `bootstrap/README.md` - Documented Lisp-based bootstrap path
- `.gitignore` - Added bootstrap binaries

### Files Removed This Session

**C-Based Bootstrap Compiler:**
- `bootstrap/primitives.c`, `encoders.c`, `ir-generation.c`, `code-generation.c`
- `bootstrap/reader.c`, `runtime-minimal.c`, `habu-bootstrap.c`
- `bootstrap/Makefile`, `bootstrap/tests/Makefile`
- All `bootstrap/test-*.c` and `bootstrap/tests/test-*.c` files

**Root Test/Debug C Files (50+ files):**
- All `test-*.c` files (factorial, conditionals, loops, functions, etc.)
- All `debug-*.c` files
- `habu-rec.c`, `habu-prog.c`, `habu-enhanced.c`, `habu-extended.c`, `habu-exec.c`
- `standalone-compiler.c`, `demo-all-features.c`
- `ir-to-asm.c`, `ir-to-asm-v2.c`, `jit-executor.c`, `bytes-to-executable.c`

**Generated Artifacts:**
- All `*.o` files throughout project
- All bootstrap executables and test binaries

**Total removed:** ~100+ C files and binaries (kept only 17 essential C files)

---

## Current Status Summary

**Architecture:** Pure Lisp with Tiny C Runtime
- Compiler: habu-arm64-codegen.lisp (Lisp)
- Generates: ARM64 machine code directly
- Runtime: 6 C files (gc, io, lineedit, region, runtime, habu-jit)
- Total C code: ~2000 lines (was ~22,000)

**Infrastructure Status:**
- ✅ SBCL compiler loads
- ✅ Generates ARM64 bytecode
- ✅ Runtime built (27 functions exported)
- ✅ JIT execution working
- ⚠️ Stub compiler needs enhancement for functional code

**Test Results (November 22, 19:30):**
- End-to-end pipeline: ✅ Working
- Arithmetic: ✅ (+, -, *) all working with fixnum tagging
- Comparisons: ✅ (=) working, returns tagged boolean
- Conditionals: ✅ (if) with correct branch logic
- Runtime calls: ✅ (cons, car, cdr) via function table pattern
- JIT execution: ✅ Proper ARM64 ABI calling convention

**Progress to Self-Hosting:**
- Phase 0 (Cleanup): 100% ✅
- Phase 1 (Bootstrap): 100% ✅ **COMPLETE!**
  * ✅ SBCL compiler loads and compiles
  * ✅ ARM64 code generation (all core features)
  * ✅ Runtime integration (function table pattern)
  * ✅ JIT execution working
- Phase 2 (Language): 0%
  * ⏳ Stack-based let bindings
  * ⏳ Closures with free variables
  * ⏳ defun and function calls
- Phase 3 (Self-compile): 0%

**Overall: ~30% to self-hosting** (1-2 weeks estimated)

---

## Reference Documents

- `CLEANUP_AND_SELF_HOSTING_PLAN.md` - Complete roadmap
- `AGENTS.md` - Project constraints and guidelines
- `SELF_HOSTING_AND_COMPLIANCE_PLAN.md` - Overall plan (outdated, based on C bootstrap)
- `MANUAL_BOOTSTRAP_PLAN.md` - Detailed implementation plan (outdated, based on C bootstrap)
- `bootstrap/README.md` - Current correct bootstrap approach

---

**Last Updated**: November 22, 2025, 19:30 PST
**Status**: Phase 1 COMPLETE ✅ - All features working (arithmetic, conditionals, runtime calls)
**Next Session**: Expand language features (let bindings, closures, defun)

## Latest Progress (November 22, 19:00-19:30) - Runtime Calls Fixed! 🎉

### Problem Identified
Runtime function calls were crashing due to **Address Space Layout Randomization (ASLR)**:
- Bytecode was pre-compiled with runtime addresses from SBCL's address space
- `run-bytecode` loaded runtime library at different addresses
- Generated code called invalid addresses → bus error (exit code 138)

### Solution: Runtime Function Table Pattern
Implemented proper JIT calling convention using runtime function tables:

1. **Modified run-bytecode.c** (run-bytecode.c:23-27, 113-116, 120-121)
   - Changed signature: `int64_t (*compiled_fn_t)(void** runtime_table)`
   - Created global runtime table with function pointers
   - Passes table as first argument (x0) to compiled code

2. **Enhanced ARM64 Instruction Set**
   - Fixed LDR encoder for parametric memory loads (habu-arm64-codegen-sbcl.lisp:110-116)
   - Fixed STP/LDP encoders for proper offset mode (habu-arm64-codegen-sbcl.lisp:132-149)
   - Added ADD/SUB immediate instructions for stack management

3. **Updated Code Generation**
   - New prologue/epilogue saves x19 (callee-saved register) (habu-arm64-codegen-sbcl.lisp:436-449)
   - Moves runtime table pointer (x0) to x19 for preservation
   - Runtime calls now load function pointers from table via LDR (habu-arm64-codegen-sbcl.lisp:306-336):
     ```lisp
     cons: LDR x9, [x19, #0]  ; table[0] = habu_cons
     car:  LDR x9, [x19, #8]  ; table[1] = habu_car
     cdr:  LDR x9, [x19, #16] ; table[2] = habu_cdr
     ```

4. **Calling Convention (ARM64 AAPCS64 compliant)**
   - Entry: x0 = runtime table pointer
   - Prologue: Reserve 32 bytes, save x29/x30/x19/x20, move x0→x19
   - Body: Load function pointers from [x19 + offset], call via BLR
   - Epilogue: Restore registers, adjust stack, return

### Test Results - ALL PASSING ✅
```bash
$ ./run-bytecode test-cons.bin
Raw result: 0xad3410011 (46493925393)
Result is a cons cell ✓

$ ./run-bytecode test-car.bin
Raw result: 0x2a0 (672)
Untagged fixnum: 42 ✓

$ ./run-bytecode test-cdr.bin
Raw result: 0x630 (1584)
Untagged fixnum: 99 ✓
```

### Key Insight
This is the **correct pattern for JIT compilers**:
- Generated code cannot embed absolute addresses (ASLR breaks them)
- Pass runtime functions via tables/contexts as arguments
- Use callee-saved registers to preserve runtime context
- This matches how real JIT engines (V8, LuaJIT, etc.) work

---

## Progress Summary (November 22, 18:30-18:45)

### Completed This Session ✅
1. **Fixed Conditional Branches** (habu-arm64-codegen-sbcl.lisp:260-273)
   - Issue: Branch offsets were incorrect
   - Fix: Use B.NE (branch if not equal) with correct offset calculation
   - Result: (if 1 42 99) → 42 ✓, (if 0 42 99) → 99 ✓

2. **Implemented Runtime Function Calls**
   - Added ARM64 encoders:
     - MOVK (Move with Keep) for 64-bit address loading (habu-arm64-codegen-sbcl.lisp:171-180)
     - BLR (Branch with Link Register) for function calls (habu-arm64-codegen-sbcl.lisp:182-186)
     - arm64-load-addr helper (4-instruction sequence: MOVZ + 3×MOVK) (habu-arm64-codegen-sbcl.lisp:188-197)

   - Added IR nodes and codegen for cons/car/cdr:
     - compile-expr: cons → cons-call, car → car-call, cdr → cdr-call (habu-arm64-codegen-sbcl.lisp:406-426)
     - codegen-expr: Loads runtime addresses and calls via BLR (habu-arm64-codegen-sbcl.lisp:303-342)

   - Fixed symbol package issue:
     - Runtime addresses must be interned in :habu-sbcl-codegen package (test-runtime-calls.lisp:13)
     - Symbols are package-sensitive in runtime-lookup

3. **Enhanced run-bytecode.c**
   - Added habu_init() call to initialize GC (run-bytecode.c:91)
   - Added runtime header include (run-bytecode.c:14)
   - Enhanced result printing for cons cells (run-bytecode.c:100-104)

### Current Issues 🐛
1. **Runtime Call Crashes**
   - Symptom: Exit code 138 (bus error) when executing cons/car/cdr
   - Bytecode generation: ✓ Working (52-72 bytes generated correctly)
   - Address loading: ✓ Correct (verified with debug-addr-loading.lisp)
   - Likely cause: ARM64 ABI/calling convention issues
     - Stack alignment (must be 16-byte aligned before BLR)
     - Register preservation (X29/X30 link register handling)
     - Argument passing convention

2. **Next Steps**:
   - Debug with lldb to identify exact crash location
   - Check ARM64 procedure call standard (AAPCS64)
   - May need to adjust prologue/epilogue or calling sequence

### Test Results
```bash
# Working: Arithmetic, Comparisons, Conditionals
(+ 5 7) → 12 ✓
(- 10 3) → 7 ✓
(* 6 7) → 42 ✓
(= 5 5) → 1 ✓
(= 5 7) → 0 ✓
(if 1 42 99) → 42 ✓
(if 0 42 99) → 99 ✓

# Compiles but crashes at runtime:
(cons 42 99) → 52 bytes generated, crashes with exit 138 ✗
(car (cons 42 99)) → 72 bytes generated, crashes with exit 138 ✗
(cdr (cons 42 99)) → 72 bytes generated, crashes with exit 138 ✗
```

---

## Progress Summary (November 22, 16:00-16:30)

### Implemented and Working ✅
1. **ARM64 Encoders**: add, sub, mul, lsl, lsr, mov, cmp, cset, b, b-cond
2. **Arithmetic Operations**:
   - Addition: (+ 5 7) → 12 ✓
   - Subtraction: (- 10 3) → 7 ✓
   - Multiplication: (* 6 7) → 42 ✓
3. **Fixnum Tagging**: All values properly tagged (value << 4)
4. **Comparisons**: (= 5 5) → 1 ✓
