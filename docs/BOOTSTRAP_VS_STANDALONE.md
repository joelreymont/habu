# Bootstrap vs. Standalone: Two-Phase Approach

## The Challenge

Habu needs to be **self-hosting** (compile itself) and **standalone** (run without SBCL), but we're currently developing *inside* SBCL. This creates a chicken-and-egg problem:

- **Now**: Habu compiler runs in SBCL, generates machine code
- **Goal**: Habu compiler compiles itself, runs standalone

## Solution: Two-Phase Bootstrap

### Phase 1: Bootstrap (Current - SBCL-dependent)

**Purpose**: Develop and test Habu features inside SBCL

**List Operations**:
- Generate machine code for cons/car/cdr operations  
- **For testing**: Code calls back to Lisp runtime functions
- Uses SBCL's heap and GC during development
- NOT standalone yet, but tests the architecture

**Code Generation**:
```lisp
;; Generated code calls runtime-cons function
(cons 1 2) => machine code that calls (funcall runtime-cons 16 32)
```

**Advantages**:
- Can test heap allocation immediately  
- Leverage existing runtime/memory.lisp
- Incremental development
- Easy debugging in Lisp

### Phase 2: Standalone (Future - Self-hosting)

**Purpose**: True standalone operation

**List Operations**:
- Inline machine code for cons/car/cdr
- NO calls to SBCL runtime
- Direct memory manipulation
- Fully self-contained

**Code Generation**:
```asm
; Inline cons allocation
mov rax, [heap_free_ptr]     ; Load free pointer
add rax, 24                   ; Check space
cmp rax, [heap_limit]
jge .call_gc

mov rbx, [heap_free_ptr]     ; Allocate
mov [rbx], 0x11               ; Write header
mov [rbx+8], <car>            ; Write car
mov [rbx+16], <cdr>           ; Write cdr
add [heap_free_ptr], 24       ; Update pointer
mov rax, rbx
or rax, 1                     ; Tag as cons
```

**Requirements**:
1. Compile runtime/memory.lisp to machine code
2. Link runtime + user code into standalone binary
3. No SBCL dependencies at all

## Current Status

✅ Phase 1: Bootstrap mode implemented
- cons/car/cdr compile to machine code
- Code calls runtime functions for testing
- All 597 tests pass

⏳ Phase 2: Standalone mode (next major milestone)
- Need to compile GC to machine code
- Need to inline allocation paths
- Need linker to create standalone binaries

## Migration Path

1. **Now**: Test features in bootstrap mode
2. **Next**: Implement inline allocation (Phase 3 of RUNTIME_INTEGRATION.md)
3. **Then**: Compile runtime functions to machine code
4. **Finally**: Self-host - Habu compiles Habu

## Key Insight

**Bootstrap mode is not a compromise** - it's an essential development tool. We can't test standalone operation until we have enough of Habu working. Bootstrap mode lets us develop incrementally while keeping the standalone architecture in mind.

The machine code we generate *today* in bootstrap mode is architecturally compatible with standalone mode - we just need to replace the runtime function calls with inline code later.
