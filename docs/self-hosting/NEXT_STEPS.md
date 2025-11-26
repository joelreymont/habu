# Habu Lisp - Next Implementation Steps

## Overview

This document provides a detailed roadmap for the next major features to implement in the Habu Lisp compiler. It builds on the current state (128 passing tests, 60+ operators) and identifies the critical path to a full Common Lisp implementation.

## Current State (2025-11-18)

### Implemented Features
- **Compiler**: 60+ operators including arithmetic, comparison, bitwise, predicates
- **Control Flow**: if, cond, case, when, unless, progn
- **Functions**: lambda, defun (inline expansion)
- **Variables**: let, let*, setq, incf, decf
- **Macros**: defmacro with compile-time expansion
- **Runtime**: Memory management, GC, symbols, strings, arrays (all in Common Lisp)
- **REPL**: Interactive interpreter with readline, history, completion
- **Tests**: 128 compiler tests, 166 runtime tests (100% pass rate)
- **Architectures**: x86_64 and ARM64 code generation

### Known Limitations
1. **No runtime integration**: cons/car/cdr work in REPL but not in compiled code
2. **No tail-call optimization**: Deep recursion causes stack overflow
3. **Named-let not working**: Lambda expressions can't be compiled standalone
4. **Fixnum-only arithmetic**: No floating point or bignums
5. **No self-hosting**: Compiler written in SBCL, not Habu

## Priority 1: Runtime Integration (CRITICAL)

### The Challenge

The compiler generates standalone machine code that cannot currently allocate heap objects. The runtime (memory allocator, GC, cons cells) is written in Common Lisp and runs in SBCL.

**Problem**: How can compiled machine code create cons cells and manipulate heap-allocated data?

### Solution Approaches

#### Option A: Inline Allocation (Quick Win)
Generate machine code that directly manipulates the heap without calling Common Lisp functions.

**Pros**:
- Self-contained compiled code
- No FFI complexity
- Best performance

**Cons**:
- Must compile entire runtime to machine code
- Complex GC coordination
- Large implementation effort

**Implementation**:
```asm
; Inline cons allocation (x86_64)
mov rax, [heap-free-ptr]    ; Get current heap pointer
add rax, 24                  ; Check if space for cons (16 bytes + 8 header)
cmp rax, [heap-limit]        ; Compare with heap limit
jge .call_gc                 ; If out of space, trigger GC

mov rbx, [heap-free-ptr]     ; Allocate
add qword [heap-free-ptr], 24

; Write header (size=16, tag=cons)
mov qword [rbx], 0x11        ; 16 | 0x1 (cons tag)

; Write car and cdr
mov rax, <car-value>
mov [rbx + 8], rax
mov rax, <cdr-value>
mov [rbx + 16], rax

; Return tagged pointer
mov rax, rbx
or rax, 0x1                  ; Add cons tag
```

**Next Steps**:
1. Implement inline `cons` allocation without GC
2. Add `car` and `cdr` as memory reads
3. Test with simple list operations
4. Add GC triggering for OOM cases
5. Compile critical runtime functions (allocate, basic GC)

#### Option B: FFI to Common Lisp (Pragmatic)
Generate machine code that calls back into SBCL runtime functions.

**Pros**:
- Reuse existing runtime code
- Incremental development
- Can test each piece

**Cons**:
- Not standalone
- FFI overhead
- Complex calling convention
- Requires running within SBCL

**Implementation**:
1. Use SBCL's alien FFI to expose runtime functions
2. Generate machine code that calls these functions
3. Manage calling convention (System V AMD64 ABI)

**Challenge**: The compiled code is currently generated as byte arrays, not as SBCL functions. To call back into SBCL, we'd need to:
- Execute compiled code within SBCL using alien functions
- Or create a hybrid execution model

#### Option C: Standalone Runtime in C (Long-term)
Implement the runtime in C and link with compiled code.

**Pros**:
- True standalone binaries
- Predictable performance
- Standard toolchain

**Cons**:
- Maintain two codebases (Lisp + C)
- Lose some Lisp flexibility
- Large initial effort

### Recommended Approach: Hybrid

**Phase 1**: Inline allocation (no GC)
- Implement inline `cons` allocation
- Implement inline `car`/`cdr` reads
- Simple bump allocator (no collection)
- Test with non-GC workloads

**Phase 2**: Add GC support
- Detect heap exhaustion
- Compile GC mark/sweep to machine code
- Test with allocation-heavy workloads

**Phase 3**: Optimize
- Profile hot paths
- Inline critical operations
- Add write barriers for generational GC

**Phase 4**: Self-hosting
- Rewrite runtime in Habu Lisp
- Bootstrap compiler compiles itself
- Full standalone system

### Code to Write

1. **Memory layout**: Document exact heap structure
2. **Allocation function**: Machine code for `runtime-cons`
3. **Access functions**: Machine code for `runtime-car`/`runtime-cdr`
4. **GC trigger**: Detect OOM and handle gracefully
5. **Tests**: Extensive tests for heap operations

### Success Criteria
- `(cons 1 2)` compiles and allocates on heap
- `(car (cons 1 2))` returns 1 in compiled code
- `(list 1 2 3)` creates proper linked list
- No memory leaks after GC
- All existing tests still pass

## Priority 2: Tail-Call Optimization (CRITICAL)

### The Challenge

Recursive functions consume stack space proportional to recursion depth. Without TCO, deep recursion causes stack overflow.

**Example Problem**:
```lisp
(defun countdown (n)
  (if (zerop n)
      0
      (countdown (1- n))))

(countdown 100000)  ; Stack overflow!
```

**With TCO**:
```lisp
; Same function, but compiled with TCO
(countdown 100000)  ; Returns 0, constant stack space
```

### Implementation Strategy

#### Step 1: Detect Tail Position

A call is in tail position if it's the last operation before returning:

**Tail positions**:
- Body of lambda/defun
- Both branches of `if`
- Last expression in `progn`
- Body of `let` (after bindings)
- Each clause result in `cond`

**NOT tail positions**:
- Argument to another function: `(+ 1 (foo x))`
- Continuation: `(1+ (foo x))`

**Implementation**:
```lisp
(defstruct compile-ctx
  (tail-p nil)        ; Are we in tail position?
  (fn-name nil)       ; Current function name
  (fn-label nil))     ; Entry point label for self-calls
```

Update all code generation to track tail position:
- `emit-if`: Mark both branches as tail if parent is tail
- `emit-progn`: Mark last expression as tail
- `emit-let`: Mark body as tail
- `emit-call`: If tail and self-call, emit jump instead of call

#### Step 2: Self-Recursive TCO

Generate tail calls as jumps for self-recursion:

**Current (non-tail)**:
```asm
; Evaluate arg
mov rax, <arg>
push rax

; Call function (inline expansion)
<function-body>

; Cleanup
add rsp, 8
ret
```

**Tail-call version**:
```asm
; Evaluate new arguments
mov rax, <new-arg>

; Update stack frame (reuse existing frame)
mov [rsp], rax

; Jump to start instead of call
jmp .function_start
```

**Benefits**:
- No stack growth
- Same performance as iteration
- Enables functional programming patterns

#### Step 3: Mutual Recursion

Support tail calls between different functions (more complex):

```lisp
(defun even (n)
  (if (zerop n) t (odd (1- n))))

(defun odd (n)
  (if (zerop n) nil (even (1- n))))
```

**Challenge**: Need to know target function address at compile time.

### Code to Write

1. **Context tracking**: Add tail-position flag to compiler
2. **Tail detection**: Identify tail positions in all expression types
3. **Self-TCO**: Implement jump-based self-recursive calls
4. **Tests**: Deep recursion tests (100,000+ iterations)

### Success Criteria
- `(countdown 1000000)` runs in constant stack space
- Factorial with accumulator works for large numbers
- All existing tests still pass
- Performance comparable to iterative loops

## Priority 3: Named-Let (Enables Local Recursion)

### The Challenge

Named-let provides local recursive loops:

```lisp
(let loop ((n 10) (acc 0))
  (if (zerop n)
      acc
      (loop (1- n) (+ acc n))))  ; Recursive call to 'loop'
```

**Current state**: Fails with "Lambda expression cannot be compiled standalone"

### Implementation Strategy

Named-let is syntax sugar for a self-recursive lambda:

```lisp
; Named-let syntax
(let loop ((n 10) (acc 0))
  body)

; Expands to:
((lambda (loop)
   (funcall loop loop 10 0))
 (lambda (loop n acc)
   body))
```

**Alternative**: Implement as special case in compiler
- Detect named-let pattern in parser
- Generate code with jump-based loop
- Avoid lambda compilation issue

**Implementation**:
1. Add named-let detection in `parse`
2. Create IR node type for named-let
3. Generate loop with backward jump
4. Add tests for countdown, accumulator, Fibonacci patterns

### Success Criteria
- `(let loop ((n 5)) (if (zerop n) 0 (loop (1- n))))` compiles
- Named-let can be nested
- Works with TCO for constant stack space
- All existing tests still pass

## Priority 4: Additional Operators

### Easy Wins (Fixnum-only)

These operators work with current architecture:

1. **`rem`**: Remainder (similar to mod but different for negatives)
   - Already mostly implemented (idiv gives remainder)

2. **`floor`**, **`ceiling`**, **`truncate`**, **`round`**: Rounding
   - For fixnums, these are mostly identity or simple operations

3. **`gcd`**, **`lcm`**: Number theory
   - Useful algorithms, can be implemented as defun

4. **`expt`**: Integer exponentiation
   - For small positive exponents, can unroll or loop

### Requires Floating Point

1. **`sqrt`**, **`sin`**, **`cos`**, **`tan`**: Math functions
   - Need x87 FPU or SSE instructions
   - Requires float type support

2. **`random`**: Random numbers
   - Need RNG state
   - Can use system calls

## Priority 5: Better Test Framework

### Current State
- 128 compiler tests using test-harness.lisp
- Tests check compilation success, not execution correctness
- No actual execution validation for most tests

### Improvements Needed

1. **Execution Validation**
   - Actually run compiled code
   - Compare results with expected values
   - Requires loading compiled code into SBCL or using alien FFI

2. **Property-Based Testing**
   - Generate random test cases
   - Check properties hold
   - Example: `(+ a b) = (+ b a)` for all a, b

3. **Performance Benchmarks**
   - Measure compilation speed
   - Measure execution speed
   - Track code size
   - Compare with SBCL, other Lisps

4. **Coverage Analysis**
   - Which operators are tested?
   - Which code paths are exercised?
   - Edge cases coverage

### Implementation
1. Create benchmark framework (see BENCHMARK_SPEC.md)
2. Add execution tests using SBCL alien FFI
3. Implement QuickCheck-style property testing
4. Track metrics over time

## Timeline Estimate

### Week 1-2: Runtime Integration Phase 1
- Inline cons allocation
- Inline car/cdr
- Tests for list operations
- **Outcome**: Compiled code can create and manipulate lists

### Week 3-4: Tail-Call Optimization
- Add tail-position tracking
- Implement self-recursive TCO
- Tests for deep recursion
- **Outcome**: Recursive algorithms run in constant space

### Week 5-6: Named-Let & More Features
- Implement named-let
- Add more operators (rem, floor, etc.)
- Improve error messages
- **Outcome**: More complete Lisp feature set

### Week 7-8: GC Integration
- Compile GC to machine code
- Add OOM detection
- Test allocation-heavy workloads
- **Outcome**: Full memory management in compiled code

### Week 9-10: Testing & Polish
- Better test framework
- Performance benchmarks
- Bug fixes
- Documentation updates
- **Outcome**: Production-ready compiler v1.0

## References

- **RUNTIME_INTEGRATION.md**: Detailed runtime integration design
- **TCO_DESIGN.md**: Tail-call optimization design
- **FULL_LISP_PLAN.md**: Complete roadmap to full Lisp
- **BENCHMARK_SPEC.md**: Performance testing framework
- **SESSION_CONTEXT.md**: Current session state and accomplishments

## Notes for Future Sessions

1. **Start with runtime integration**: This is the most critical blocker
2. **TCO is close second**: Needed for real programs
3. **Named-let unlocks patterns**: Many algorithms need local recursion
4. **Test constantly**: Don't break existing functionality
5. **Document everything**: Future sessions need context
6. **Commit frequently**: Small, focused commits with clear messages

## Questions to Answer

1. Should we implement inline allocation or FFI first?
2. How do we test compiled code execution?
3. What's the minimum viable GC for compiled code?
4. Should named-let be a special form or expand to lambda?
5. When should we start self-hosting efforts?

---

**Last Updated**: 2025-11-18
**Session**: claude/habu-read-markdown-01TyZUStKoi7uEHenU5E28VZ
