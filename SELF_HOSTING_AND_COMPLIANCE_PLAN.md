# Self-Hosting and Full Lisp Spec Compliance Plan

**Created:** November 21, 2025
**Current Status:** Recursive functions working, ARM64 codegen 97% complete
**Goal:** Self-hosting Habu compiler + Full Common Lisp compliance
**Timeline:** 6-8 weeks to self-hosting, 6-12 months to full CL compliance

---

## Current State (November 21, 2025)

### ✅ What's Working
- **ARM64 Code Generation:** 97% complete
  - All arithmetic, comparisons, logical ops
  - Control flow (if/cond/when/unless/progn)
  - Variables (let/let*/multiple bindings)
  - Functions (defun/lambda/recursion) ✓
  - Type predicates
  - cons/car/cdr with runtime calls
  - **Recursive function calls with BL offset calculation** ✓ NEW!

- **Runtime System:**
  - Memory allocation working
  - cons/car/cdr operations
  - Tagged arithmetic
  - JIT execution verified

- **Tests:** 49/49 passing
  - Compiler: 41/41
  - Runtime: 5/5
  - JIT: 3/3
  - **Recursive factorial: factorial(5) = 120** ✓ NEW!

### 🔧 What Needs Fixing
1. Function prologue uses unsafe stack pattern (needs sub sp first)
2. No tail-call optimization
3. compile-program-with-functions-with-runtime only in pure Habu version
4. Missing runtime address threading in some paths

---

## Phase 1: Immediate Fixes (1-2 days)

### Milestone 1.1: Fix Function Prologue (2 hours)
**Goal:** Use safe stack allocation pattern in all generated functions

**Tasks:**
1. Update `codegen-function-with-runtime` (line 921-922)
   - Change from: `(arm64-stp 29 30 31 -16)` with pre-decrement
   - Change to: `(arm64-sub-imm 31 31 32)` then `(arm64-stp 29 30 31 0)`
2. Update `codegen-function-with-params` similarly
3. Adjust epilogue to match (use add sp, then ldp from [sp])
4. Test with factorial to ensure no segfaults
5. Update all test expectations

**Success Criteria:**
- Recursive functions work without large stack guard buffers
- No segfaults on page boundaries

### Milestone 1.2: Tail-Call Optimization (4 hours)
**Goal:** Convert tail-recursive functions to iteration at machine code level

**Tasks:**
1. Add tail-position tracking to IR
   - Modify `compile-expr` to track tail context
   - Thread tail? parameter through compilation
2. Detect tail calls in `codegen-expr`
   - When `fncall` is in tail position
   - AND calling same function (for now, just self-recursion)
3. Generate different code for tail calls:
   ```assembly
   ; Restore stack BEFORE jump
   add sp, sp, #N
   ldp x29, x30, [sp], #16
   ; Jump instead of call
   b target        ; Not BL!
   ```
4. Test with tail-recursive factorial
5. Benchmark performance improvement

**Success Criteria:**
- Tail-recursive factorial compiles to loop
- No stack growth for tail calls
- Performance improvement measurable

### Milestone 1.3: Port Multi-Function Compilation to SBCL (2 hours)
**Goal:** Enable testing full programs in SBCL environment

**Tasks:**
1. Remove `#-sbcl` wrapper from `compile-program-with-functions-with-runtime`
2. Add to habu-arm64-codegen-sbcl.lisp if needed
3. Ensure codegen-functions-helper available
4. Test compilation of multi-function programs in SBCL
5. Create test-compiler-multi-function.lisp

**Success Criteria:**
- Can compile programs with multiple functions in SBCL
- factorial + main compiles successfully
- Generated code matches hand-coded tests

---

## Phase 2: Self-Hosting Foundation (3-5 days)

### Milestone 2.1: Complete Runtime Address Threading (4 hours)
**Goal:** All compilation paths use real runtime addresses

**Tasks:**
1. Audit all `compile-*` functions for runtime address usage
2. Add `runtime-addrs` parameter where missing
3. Create default runtime table builder
4. Update all call sites
5. Test that cons/car/cdr use real addresses everywhere

**Success Criteria:**
- No more dummy `#x0` addresses
- All runtime calls use correct function pointers
- JIT tests pass with real addresses

### Milestone 2.2: Standard Library in Habu (1 day)
**Goal:** Core library functions compiled, not C runtime

**Tasks:**
1. Implement in Habu:
   - list operations (length, append, reverse, map, filter)
   - arithmetic (abs, min, max, gcd, lcm)
   - predicates (null?, pair?, list?, equal?)
   - higher-order (apply, foldr, foldl)
2. Compile to ARM64
3. Create habu-stdlib.lisp
4. Test each function
5. Benchmark vs C runtime

**Success Criteria:**
- 20+ core functions in pure Habu
- All functions compile successfully
- Performance acceptable (within 2x of C)

### Milestone 2.3: Minimal REPL in Compiled Code (1 day)
**Goal:** REPL runs from compiled Habu, not interpreter

**Tasks:**
1. Create minimal-repl.lisp:
   - Read loop
   - Eval (using existing evaluator)
   - Print
   - Error handling
2. Compile to ARM64
3. Link with runtime
4. Test basic expressions
5. Test loading files

**Success Criteria:**
- Can enter expressions and get results
- Can load and execute files
- Errors don't crash REPL

### Milestone 2.4: Self-Compile Simple Expressions (1 day)
**Goal:** Compiler can compile simple Habu code from within Habu

**Tasks:**
1. Load habu-arm64-codegen.lisp into Habu REPL
2. Fix any syntax incompatibilities (hex literals, etc.)
3. Compile simple expression: `(+ 2 3)`
4. Verify generated bytecode matches SBCL version
5. Execute compiled code and verify result
6. Compile progressively complex expressions

**Success Criteria:**
- Compiler loads without errors
- Can compile arithmetic expressions
- Generated code executes correctly
- Results match expected values

---

## Phase 3: Full Self-Hosting (5-7 days)

### Milestone 3.1: Self-Compile Functions (2 days)
**Goal:** Compiler can compile function definitions

**Tasks:**
1. Compile simple function: `(define (square x) (* x x))`
2. Verify function can be called
3. Compile recursive function (factorial)
4. Test execution
5. Compile functions that call other functions
6. Build function call graph testing

**Success Criteria:**
- Can compile function definitions
- Functions callable from REPL
- Recursive functions work
- Multi-function programs compile

### Milestone 3.2: Self-Compile Compiler Core (3 days)
**Goal:** Compiler can compile most of itself

**Tasks:**
1. Compile IR generation (compile-expr)
2. Compile basic code generators
3. Test compiled compiler generates same IR
4. Compile ARM64 encoders
5. Test compiled encoders produce same bytecode
6. Identify and fix any self-reference issues

**Success Criteria:**
- 50%+ of compiler compiles successfully
- Compiled components produce correct output
- Can bootstrap next stage

### Milestone 3.3: Bootstrap (2 days)
**Goal:** Achieve fixed-point self-compilation

**Tasks:**
1. **Stage 0:** SBCL compiles full compiler → compiler0
2. **Stage 1:** compiler0 compiles full compiler → compiler1
3. **Stage 2:** compiler1 compiles full compiler → compiler2
4. **Verify:** Binary diff compiler1 == compiler2
5. Document bootstrap process
6. Create automated bootstrap script
7. 🎉 **CELEBRATE SELF-HOSTING!**

**Success Criteria:**
- Fixed point reached (compiler1 == compiler2)
- Bootstrap completes without manual intervention
- Can compile arbitrary Habu programs
- Performance acceptable

---

## Phase 4: Language Features (2-3 weeks)

### Milestone 4.1: Closures (3 days)
**Goal:** Functions can capture free variables

**Tasks:**
1. Design closure representation
   - Environment chain
   - Captured variables
   - Code pointer
2. Modify IR to track free variables
3. Generate closure allocation code
4. Generate environment lookup code
5. Test nested functions
6. Test higher-order functions (map/filter)
7. Benchmark closure performance

**Success Criteria:**
- Can create closures
- Closures capture correct values
- Nested functions work
- map/filter work with lambdas

### Milestone 4.2: Macros (2 days)
**Goal:** Compile-time code transformation

**Tasks:**
1. Implement defmacro
2. Implement macroexpand
3. Add macro table to environment
4. Expand macros during compilation
5. Implement quote/quasiquote/unquote
6. Test basic macros (when/unless/and/or)
7. Test complex macros (cond rewrite)

**Success Criteria:**
- Can define macros
- Macros expand correctly
- Can implement control flow as macros
- Nested macros work

### Milestone 4.3: Multiple Values (2 days)
**Goal:** Functions can return multiple values

**Tasks:**
1. Design MV calling convention
   - Primary value in x0
   - Count in x1
   - Additional values on stack
2. Implement values
3. Implement multiple-value-bind
4. Implement multiple-value-call
5. Update runtime
6. Test MV functions

**Success Criteria:**
- Can return multiple values
- Can receive multiple values
- Performance acceptable
- Common Lisp compatible

### Milestone 4.4: Exception Handling (3 days)
**Goal:** try/catch/finally style error handling

**Tasks:**
1. Design exception mechanism
   - Stack unwinding
   - Handler chain
   - Resource cleanup
2. Implement catch/throw
3. Implement unwind-protect
4. Implement handler-case/handler-bind
5. Test error propagation
6. Test cleanup guarantees

**Success Criteria:**
- Can throw and catch exceptions
- Stack unwinds correctly
- Cleanup code always runs
- Nested handlers work

### Milestone 4.5: Advanced Control Flow (2 days)
**Goal:** block/return-from, tagbody/go

**Tasks:**
1. Implement block/return-from
   - Lexical exit points
   - Non-local returns
2. Implement tagbody/go
   - Labeled jumps
   - Lexical scope
3. Generate ARM64 for non-local exits
4. Test complex control flow
5. Test interaction with exceptions

**Success Criteria:**
- Non-local returns work
- Labeled jumps work
- Interacts correctly with unwind-protect
- Performance acceptable

---

## Phase 5: Data Structures (2-3 weeks)

### Milestone 5.1: Strings (3 days)
**Goal:** First-class string support

**Tasks:**
1. Design string representation
   - Length prefix
   - UTF-8 encoding
   - Immutable
2. Implement string operations
   - make-string
   - string-ref/string-set!
   - string-length
   - string-append
   - substring
3. Implement string comparisons
4. Add to runtime
5. Generate ARM64 code
6. Test thoroughly

**Success Criteria:**
- Strings are first-class values
- All operations work correctly
- UTF-8 handled properly
- Performance good

### Milestone 5.2: Vectors (2 days)
**Goal:** Fixed-size arrays

**Tasks:**
1. Design vector representation
2. Implement vector operations
   - make-vector
   - vector-ref/vector-set!
   - vector-length
3. Add to runtime
4. Generate code
5. Test

**Success Criteria:**
- Vectors work like CL vectors
- Efficient access (constant time)
- Type safety

### Milestone 5.3: Hash Tables (4 days)
**Goal:** Key-value maps

**Tasks:**
1. Design hash table representation
2. Implement hash functions
3. Implement operations
   - make-hash-table
   - gethash/puthash
   - remhash
   - clrhash
4. Handle collisions
5. Implement resizing
6. Test thoroughly
7. Benchmark

**Success Criteria:**
- Hash tables work like CL
- Good performance (O(1) average)
- Handles collisions
- Resizes appropriately

### Milestone 5.4: Symbols (3 days)
**Goal:** Proper symbol type with properties

**Tasks:**
1. Design symbol representation
   - Name string
   - Value cell
   - Function cell
   - Property list
   - Package
2. Implement intern
3. Implement symbol operations
4. Add to runtime
5. Test

**Success Criteria:**
- Symbols are first-class
- Symbol interning works
- Property lists work
- CL compatible

### Milestone 5.5: Packages (3 days)
**Goal:** Namespace management

**Tasks:**
1. Design package system
2. Implement defpackage
3. Implement in-package
4. Implement export/import
5. Update reader
6. Test package isolation

**Success Criteria:**
- Can create packages
- Symbols properly isolated
- Import/export works
- CL compatible

---

## Phase 6: Numeric Tower (2 weeks)

### Milestone 6.1: Bignums (4 days)
**Goal:** Arbitrary precision integers

**Tasks:**
1. Design bignum representation
2. Implement bignum arithmetic
3. Automatic promotion from fixnum
4. Test edge cases
5. Benchmark

**Success Criteria:**
- Can represent large integers
- Arithmetic correct
- Auto-promotion works
- Performance acceptable

### Milestone 6.2: Rationals (2 days)
**Goal:** Exact fractions

**Tasks:**
1. Design ratio representation
2. Implement ratio arithmetic
3. Implement reduction
4. Test

**Success Criteria:**
- Exact fractions work
- Automatically reduced
- CL compatible

### Milestone 6.3: Floating Point (3 days)
**Goal:** IEEE 754 floats

**Tasks:**
1. Design float representation
2. Implement float arithmetic
3. Implement conversions
4. Test precision
5. Test special values (NaN/Inf)

**Success Criteria:**
- Floats work correctly
- Conversions accurate
- Special values handled
- CL compatible

### Milestone 6.4: Complex Numbers (2 days)
**Goal:** Complex arithmetic

**Tasks:**
1. Design complex representation
2. Implement complex arithmetic
3. Test
4. Benchmark

**Success Criteria:**
- Complex numbers work
- All operations correct
- CL compatible

### Milestone 6.5: Numeric Coercion (3 days)
**Goal:** Automatic type conversions

**Tasks:**
1. Implement type hierarchy
2. Implement coercion rules
3. Update all arithmetic
4. Test mixed-type operations
5. Verify CL compatibility

**Success Criteria:**
- Mixed arithmetic works
- Results have correct type
- CL compatible
- Performance acceptable

---

## Phase 7: I/O and System (1-2 weeks)

### Milestone 7.1: File I/O (3 days)
**Goal:** Read and write files

**Tasks:**
1. Implement open/close
2. Implement read-char/write-char
3. Implement read-line/write-line
4. Implement read/write (s-expressions)
5. Error handling
6. Test

**Success Criteria:**
- Can read/write files
- Error handling works
- CL compatible

### Milestone 7.2: Streams (2 days)
**Goal:** Abstraction over I/O

**Tasks:**
1. Design stream protocol
2. Implement string streams
3. Implement file streams
4. Test

**Success Criteria:**
- Stream abstraction works
- Multiple stream types
- CL compatible

### Milestone 7.3: Format (4 days)
**Goal:** Formatted output

**Tasks:**
1. Implement format directives
2. Test complex formats
3. Benchmark

**Success Criteria:**
- Basic format works
- CL compatible (subset)

### Milestone 7.4: Pathnames (2 days)
**Goal:** Portable file system paths

**Tasks:**
1. Implement pathname parsing
2. Implement pathname operations
3. Test on different OSes
4. Verify CL compatibility

**Success Criteria:**
- Pathnames work
- Cross-platform
- CL compatible

---

## Phase 8: Optimization (2-3 weeks)

### Milestone 8.1: Constant Folding (2 days)
**Goal:** Evaluate constant expressions at compile time

**Tasks:**
1. Add constant propagation pass
2. Fold arithmetic
3. Fold comparisons
4. Test and benchmark

**Success Criteria:**
- Constant expressions eliminated
- Performance improvement measurable

### Milestone 8.2: Dead Code Elimination (2 days)
**Goal:** Remove unreachable code

**Tasks:**
1. Implement liveness analysis
2. Remove dead code
3. Test
4. Benchmark

**Success Criteria:**
- Dead code removed
- Binary size reduced
- Performance improved

### Milestone 8.3: Inlining (3 days)
**Goal:** Inline small functions

**Tasks:**
1. Implement inlining pass
2. Cost model
3. Test
4. Benchmark

**Success Criteria:**
- Small functions inlined
- Performance improvement significant

### Milestone 8.4: Register Allocation (5 days)
**Goal:** Better use of ARM64 registers

**Tasks:**
1. Implement liveness analysis
2. Implement graph coloring
3. Handle spills
4. Test
5. Benchmark

**Success Criteria:**
- Fewer stack accesses
- Performance improvement significant
- Correctness maintained

### Milestone 8.5: Peephole Optimization (3 days)
**Goal:** Local instruction improvements

**Tasks:**
1. Identify patterns
2. Implement rewrites
3. Test
4. Benchmark

**Success Criteria:**
- Common patterns optimized
- Code size reduced
- Performance improved

---

## Phase 9: Testing and Compliance (Ongoing)

### Milestone 9.1: ANSI CL Test Suite (2 weeks)
**Goal:** Pass ANSI Common Lisp tests

**Tasks:**
1. Obtain ANSI CL test suite
2. Run tests
3. Fix failures
4. Document incompatibilities
5. Iterate

**Success Criteria:**
- 90%+ of tests pass
- Known incompatibilities documented
- Compliance verified

### Milestone 9.2: Performance Benchmarks (1 week)
**Goal:** Competitive performance

**Tasks:**
1. Implement standard benchmarks
2. Compare with SBCL/CCL
3. Identify bottlenecks
4. Optimize
5. Document results

**Success Criteria:**
- Within 2-5x of SBCL
- Performance predictable
- No major regressions

### Milestone 9.3: Stress Testing (1 week)
**Goal:** Stability under load

**Tasks:**
1. Long-running programs
2. Memory stress tests
3. Recursive depth tests
4. Concurrent stress (if applicable)
5. Fix crashes

**Success Criteria:**
- No crashes
- No memory leaks
- Stable under load

---

## Phase 10: Multi-Platform (2-3 weeks)

### Milestone 10.1: x86-64 Backend (2 weeks)
**Goal:** Support Intel/AMD processors

**Tasks:**
1. Design x86-64 calling convention
2. Implement instruction encoders
3. Port code generation
4. Test
5. Benchmark

**Success Criteria:**
- x86-64 backend works
- All tests pass
- Performance comparable to ARM64

### Milestone 10.2: Cross-Compilation (1 week)
**Goal:** Compile for different architectures

**Tasks:**
1. Architecture selection
2. Target-specific code paths
3. Test cross-compilation
4. Document

**Success Criteria:**
- Can compile for ARM64 from x86-64
- Can compile for x86-64 from ARM64
- Generated code correct

---

## Phase 11: Documentation and Tooling (Ongoing)

### Milestone 11.1: User Documentation (1 week)
**Goal:** Comprehensive user guide

**Tasks:**
1. Getting started guide
2. Language reference
3. Standard library reference
4. Examples
5. FAQ

### Milestone 11.2: Developer Documentation (1 week)
**Goal:** Internal documentation

**Tasks:**
1. Architecture overview
2. Compiler internals
3. Runtime internals
4. Contribution guide
5. Testing guide

### Milestone 11.3: Debugger (2 weeks)
**Goal:** Interactive debugging

**Tasks:**
1. Breakpoints
2. Stack traces
3. Variable inspection
4. Step execution

### Milestone 11.4: Profiler (1 week)
**Goal:** Performance analysis

**Tasks:**
1. Time profiling
2. Memory profiling
3. Visualization
4. Integration with REPL

---

## Success Metrics

### Self-Hosting (Phase 1-3)
- [ ] Compiler compiles itself
- [ ] Fixed-point bootstrap achieved
- [ ] Can compile and run arbitrary programs
- [ ] Performance within 5x of SBCL

### Full Lisp Compliance (Phase 4-9)
- [ ] 90%+ ANSI CL compliance
- [ ] All major features implemented
- [ ] Numeric tower complete
- [ ] Condition system working
- [ ] Package system working

### Production Ready (Phase 10-11)
- [ ] Multi-platform support
- [ ] Comprehensive documentation
- [ ] Debugging tools
- [ ] Performance tools
- [ ] Stable under load

---

## Timeline Summary

| Phase | Duration | Milestone |
|-------|----------|-----------|
| 1. Immediate Fixes | 1-2 days | Safe prologue, TCO, SBCL port |
| 2. Self-Hosting Foundation | 3-5 days | Runtime threading, stdlib, minimal REPL |
| 3. Full Self-Hosting | 5-7 days | **SELF-HOSTING ACHIEVED** 🎉 |
| 4. Language Features | 2-3 weeks | Closures, macros, MV, exceptions |
| 5. Data Structures | 2-3 weeks | Strings, vectors, hashes, packages |
| 6. Numeric Tower | 2 weeks | Bignums, ratios, floats, complex |
| 7. I/O and System | 1-2 weeks | Files, streams, format |
| 8. Optimization | 2-3 weeks | Folding, DCE, inlining, regalloc |
| 9. Testing | Ongoing | ANSI tests, benchmarks, stress |
| 10. Multi-Platform | 2-3 weeks | x86-64, cross-compilation |
| 11. Documentation | Ongoing | User docs, dev docs, tools |

**Total to Self-Hosting:** ~2 weeks
**Total to Full Compliance:** 6-12 months

---

## Critical Path

The absolute minimum path to self-hosting:

1. **Week 1:** Fix prologue, implement TCO, port to SBCL
2. **Week 2:** Complete runtime threading, self-compile expressions, bootstrap

Everything else (full CL compliance) can be done after self-hosting is achieved.

---

## Risk Mitigation

### Technical Risks
- **Closures complex:** Start with simple cases, add features incrementally
- **GC integration:** Use conservative GC initially, optimize later
- **Performance:** Profile early, optimize hot paths
- **Bugs in self-compilation:** Extensive testing at each stage

### Process Risks
- **Scope creep:** Stick to critical path for self-hosting
- **Perfectionism:** Good enough > perfect
- **Testing overhead:** Automate everything
- **Documentation lag:** Document as you go

---

## Notes

- This plan assumes full-time work (~40 hours/week)
- Actual timeline may vary based on discoveries
- Some phases can overlap
- Testing is continuous throughout
- Documentation written alongside code
- Performance optimization ongoing

**Next Step:** Start Phase 1, Milestone 1.1 (Fix Function Prologue)
