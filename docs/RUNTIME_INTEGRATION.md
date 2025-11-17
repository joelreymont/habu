# Runtime Integration Design

## Overview

This document describes how to integrate the Habu Lisp runtime (heap allocator, GC, cons cells) with the compiler's generated machine code.

## Current State

### Compiler
- Generates standalone x86_64 and ARM64 machine code
- Compiles expressions to byte arrays
- Works with fixnums (tagged with 4-bit tag)
- No heap allocation or GC

### Runtime
- Written in Common Lisp
- Heap allocator with bump allocation
- Mark-and-sweep garbage collector
- Cons cells, symbols, strings, arrays
- All heap-allocated with proper tagging

## The Challenge

**Problem**: How can compiled machine code use heap-allocated objects?

**Options**:

### Option A: Compile Runtime to Machine Code
**Pros**:
- Fully standalone binaries
- No dependencies
- Maximum performance

**Cons**:
- Complex: need to compile all of memory.lisp
- Need to handle CL→machine code for GC, allocation, etc.
- Large implementation effort

### Option B: FFI to Common Lisp Runtime
**Pros**:
- Reuse existing runtime code
- Can test incrementally
- Simpler initially

**Cons**:
- Not standalone
- Performance overhead for calls
- Complex calling convention

### Option C: Hybrid Approach (RECOMMENDED)
**Pros**:
- Start simple, evolve to standalone
- Incremental development
- Test as we go

**Cons**:
- Two implementations to maintain temporarily

## Recommended Approach: Hybrid

### Phase 1: Shared Runtime State
1. Compile expressions in context of loaded runtime
2. Access runtime heap via global variable
3. Call runtime functions from compiled code

**Implementation**:
```lisp
;; Load runtime
(load "runtime/memory.lisp")
(initialize-runtime)

;; Compile with runtime context
(compile-with-runtime '(cons 1 2))
```

### Phase 2: Generate Runtime Calls
Generate machine code that calls Common Lisp functions:

**x86_64 calling convention**:
- Arguments in: RDI, RSI, RDX, RCX, R8, R9
- Return value in: RAX
- Caller-saved: RAX, RCX, RDX, RSI, RDI, R8-R11
- Callee-saved: RBX, RSP, RBP, R12-R15

**Example: cons**:
```asm
; Evaluate car into RAX
mov rax, <car-value>
mov rdi, rax          ; First arg

; Evaluate cdr into RAX
mov rax, <cdr-value>
mov rsi, rax          ; Second arg

; Call runtime-cons
mov rax, <address of runtime-cons>
call rax

; Result in RAX
```

### Phase 3: Inline Critical Paths
For performance, inline hot paths:

**Inline cons allocation** (no GC):
```asm
; Check if heap has space
mov rax, [heap-free-ptr]
add rax, 24              ; cons cell size
cmp rax, [heap-limit]
jge .call_gc             ; Out of space

; Allocate
mov rbx, [heap-free-ptr]
add qword [heap-free-ptr], 24

; Write header
mov qword [rbx], 16      ; size
or qword [rbx], 0x1      ; tag-cons

; Write car
mov rax, <car-value>
mov [rbx + 8], rax

; Write cdr
mov rax, <cdr-value>
mov [rbx + 16], rax

; Return tagged pointer
mov rax, rbx
or rax, 0x1              ; add cons tag
```

### Phase 4: Compile Runtime Functions
Eventually compile critical runtime functions to machine code:
- `runtime-cons`
- `runtime-car`
- `runtime-cdr`
- Simple allocation (without GC)

## Implementation Plan

### Step 1: Simple Shared Runtime (THIS SESSION)
**Goal**: Get cons/car/cdr working with existing runtime

**Approach**:
1. Load runtime before compiling
2. Generate calls to `runtime-cons`, `runtime-car`, `runtime-cdr`
3. Use SBCL's alien FFI or direct function pointers

**Test**:
```lisp
(load "runtime/memory.lisp")
(initialize-runtime)

;; Returns heap-allocated cons pointer
(let ((code (compile-expression '(cons 1 2))))
  (execute-with-runtime code))
```

### Step 2: Add List Operations to Compiler
1. Parse cons, car, cdr, list
2. Generate call expressions
3. Test with runtime

### Step 3: Integration Tests
- cons creates heap objects
- car/cdr read from heap
- GC works with compiled allocations
- Multiple cons calls don't leak

### Step 4: Performance
- Benchmark call overhead
- Inline if needed
- Profile GC behavior

## Technical Details

### Calling Convention
Use System V AMD64 ABI:
- Integer/pointer args: RDI, RSI, RDX, RCX, R8, R9
- Return: RAX
- Stack alignment: 16 bytes
- Red zone: 128 bytes

### Memory Layout
```
Cons cell: [header:8][car:8][cdr:8] = 24 bytes
- header: size (16) | tag (1)
- car: tagged value
- cdr: tagged value
```

### Tagging
- Fixnum: value << 4 | 0x0
- Cons: ptr | 0x1
- Symbol: ptr | 0x2
- String: ptr | 0x3
- Array: ptr | 0x4

## Testing Strategy

1. **Unit tests**: Each operation independently
2. **Integration**: cons + car/cdr chains
3. **GC stress**: Many allocations
4. **Mixed**: Fixnums and cons cells
5. **Regression**: Ensure fixnum ops still work

## Success Criteria

✅ (cons 1 2) allocates on heap
✅ (car (cons 1 2)) returns 1
✅ (cdr (cons 1 2)) returns 2
✅ (list 1 2 3) creates linked list
✅ GC collects unreferenced cons cells
✅ All existing tests pass

## Future Work

- Compile runtime to machine code
- Fully standalone binaries
- Optimize allocation inline
- Generational GC
- Concurrent GC

## References

- System V AMD64 ABI
- SBCL alien FFI
- Appel, "Compiling with Continuations"
- Cheney, "A Nonrecursive List Compacting Algorithm"
