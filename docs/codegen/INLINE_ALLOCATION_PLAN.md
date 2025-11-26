# Inline Heap Allocation Implementation Plan (Phase 2)

## Overview

Phase 2 requires implementing inline heap allocation for standalone executables without FFI dependencies. Currently, `cons`, `car`, and `cdr` use FFI trampolines to call back into SBCL's runtime. We need to replace these with inline machine code that manages a heap directly.

## Current Status

### ✅ Completed
- ARM64 executable generation working on macOS
- Code signing issue resolved via system linker
- Basic compiler infrastructure for inline allocation mode
- Placeholder functions exist: `emit-inline-cons-arm64`, `emit-inline-car-arm64`, `emit-inline-cdr-arm64`

### 🚧 In Progress
- Inline heap allocation implementation

### ⏳ Pending
- Garbage collection
- Heap overflow handling
- Multi-threaded heap access

## Architecture

### Memory Layout

```
┌─────────────────────────────────────┐
│ Executable Code (__TEXT segment)   │ Read-Execute
├─────────────────────────────────────┤
│ Heap Globals (__DATA segment)      │ Read-Write
│  - heap_start:  .quad 0            │
│  - heap_ptr:    .quad 0            │
│  - heap_limit:  .quad 0            │
│  - heap_size:   .quad 1048576      │ (1MB initial)
├─────────────────────────────────────┤
│ Dynamic Heap (mmap'd at runtime)   │ Read-Write
│  - Allocated via mmap              │
│  - 1MB initially, grows as needed   │
│  - Cons cells (16 bytes each)      │
│    [car: 8 bytes][cdr: 8 bytes]    │
└─────────────────────────────────────┘
```

### Cons Cell Layout

```
Offset  Size  Field
0       8     car (tagged pointer or fixnum)
8       8     cdr (tagged pointer or fixnum)

Total: 16 bytes
```

### Pointer Tagging

```
xxxx...xxx0  Fixnum (right-shift 1 to get integer value)
xxxx...xxx1  Cons pointer (untag with AND ~1)
xxxx...x11   Symbol (future)
```

## Implementation Steps

### Step 1: Heap Initialization Trampoline

Add startup code that allocates the heap before `main`:

```assembly
.section __TEXT,__text
.globl _start
_start:
    ; Call heap_init to allocate heap via mmap
    bl _heap_init

    ; Jump to user code
    bl _main

    ; Exit with result from main
    mov x16, #1      ; sys_exit
    svc #0x80

_heap_init:
    ; mmap(NULL, heap_size, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0)
    mov x0, #0              ; addr = NULL
    adrp x1, heap_size@PAGE
    ldr x1, [x1, heap_size@PAGEOFF]  ; length = heap_size
    mov x2, #3              ; prot = PROT_READ | PROT_WRITE
    mov x3, #0x1002         ; flags = MAP_PRIVATE | MAP_ANONYMOUS
    mov x4, #-1             ; fd = -1
    mov x5, #0              ; offset = 0
    mov x16, #197           ; sys_mmap
    svc #0x80

    ; Store results in globals
    adrp x1, heap_start@PAGE
    str x0, [x1, heap_start@PAGEOFF]

    adrp x1, heap_ptr@PAGE
    str x0, [x1, heap_ptr@PAGEOFF]

    adrp x1, heap_limit@PAGE
    adrp x2, heap_size@PAGE
    ldr x2, [x2, heap_size@PAGEOFF]
    add x2, x0, x2          ; heap_start + heap_size
    str x2, [x1, heap_limit@PAGEOFF]

    ret

.section __DATA,__data
.globl heap_start, heap_ptr, heap_limit, heap_size
heap_start:  .quad 0
heap_ptr:    .quad 0
heap_limit:  .quad 0
heap_size:   .quad 1048576  ; 1MB initial heap
```

### Step 2: Inline Cons Allocation (ARM64)

```lisp
(defun emit-inline-cons-arm64 (car-code cdr-code)
  "Generate inline cons allocation for ARM64"
  (append
   ;; Evaluate car, save in x19
   car-code
   '(#xF3 #x03 #x00 #xAA)          ; mov x19, x0

   ;; Evaluate cdr, save in x20
   cdr-code
   '(#xF4 #x03 #x00 #xAA)          ; mov x20, x0

   ;; Load heap_ptr
   ;; adrp x0, heap_ptr@PAGE
   ;; This requires relocation - we'll use PC-relative addressing
   (emit-adrp-arm64 0 'heap_ptr)
   ;; ldr x0, [x0, heap_ptr@PAGEOFF]
   (emit-ldr-arm64 0 0 'heap_ptr)

   ;; Check if heap_ptr + 16 < heap_limit
   '(#x01 #x40 #x00 #x91)          ; add x1, x0, #16
   ;; Load heap_limit
   (emit-adrp-arm64 2 'heap_limit)
   (emit-ldr-arm64 2 2 'heap_limit)
   ;; Compare
   '(#x3F #x00 #x02 #xEB)          ; cmp x1, x2
   ;; If >= limit, call GC or abort
   '(#x44 #x00 #x00 #x54)          ; b.cc +8 (skip abort)
   ;; Abort for now (TODO: implement GC)
   '(#x00 #x00 #x80 #xD2)          ; mov x0, #0
   '(#x10 #x00 #x80 #xD2)          ; mov x16, #0
   '(#x01 #x10 #x00 #xD4)          ; svc #0x80 (abort)

   ;; Store car at [heap_ptr+0]
   '(#x13 #x00 #x00 #xF9)          ; str x19, [x0, #0]

   ;; Store cdr at [heap_ptr+8]
   '(#x14 #x04 #x00 #xF9)          ; str x20, [x0, #8]

   ;; Tag pointer (set low bit)
   '(#x00 #x04 #x00 #xB2)          ; orr x0, x0, #1

   ;; Update heap_ptr += 16
   '(#x01 #x40 #x00 #x91)          ; add x1, x0, #16
   '(#x00 #x04 #x00 #x92)          ; and x0, x0, #~1 (untag for storage)
   '(#x01 #x40 #x00 #x91)          ; add x1, x0, #16
   (emit-adrp-arm64 2 'heap_ptr)
   (emit-str-arm64 1 2 'heap_ptr)

   ;; Re-tag result
   '(#x00 #x04 #x00 #xB2)))        ; orr x0, x0, #1
```

### Step 3: Update Code Generator

Modify `generate-executable-via-linker` to include:
1. Heap global definitions in .data section
2. Heap initialization trampoline
3. Change entry point from `_main` to `_start`

```lisp
(defun machine-code-to-assembly (code arch)
  "Convert machine code to assembly with heap support"
  (with-output-to-string (s)
    ;; Text section
    (format s ".section __TEXT,__text~%")
    (format s ".globl _start~%~%")

    ;; Heap initialization
    (format s "_start:~%")
    (format s "    bl _heap_init~%")
    (format s "    bl _main~%")
    (format s "    mov x16, #1~%")
    (format s "    svc #0x80~%~%")

    (format s "_heap_init:~%")
    (format s "    ; TODO: mmap heap allocation~%")
    (format s "    ret~%~%")

    ;; User code
    (format s "_main:~%")
    (loop for byte in (coerce code 'list)
          do (format s "    .byte 0x~2,'0X~%" byte))
    (format s "    ret~%~%")

    ;; Data section
    (format s ".section __DATA,__data~%")
    (format s ".globl heap_start, heap_ptr, heap_limit, heap_size~%")
    (format s "heap_start:  .quad 0~%")
    (format s "heap_ptr:    .quad 0~%")
    (format s "heap_limit:  .quad 0~%")
    (format s "heap_size:   .quad 1048576~%")))
```

## Testing Strategy

### Test 1: Simple Cons
```lisp
(compile-to-executable '(cons 1 2) :output-file "test-cons")
; Should allocate cons cell and return tagged pointer
```

### Test 2: Nested Cons
```lisp
(compile-to-executable '(cons (cons 1 2) 3) :output-file "test-nested")
; Tests multiple allocations
```

### Test 3: Car/Cdr Access
```lisp
(compile-to-executable '(car (cons 5 10)) :output-file "test-car")
; Should return 5 (tagged as fixnum: 80)
```

### Test 4: List Construction
```lisp
(compile-to-executable '(cons 1 (cons 2 (cons 3 nil))) :output-file "test-list")
; Builds a linked list
```

## Challenges and Solutions

### Challenge 1: PC-Relative Addressing
**Problem**: Need to access heap globals from generated code
**Solution**: Use `adrp` + `ldr` for position-independent code
**Status**: Need to implement `emit-adrp-arm64` and `emit-ldr-arm64`

### Challenge 2: Heap Overflow
**Problem**: What happens when heap is full?
**Solution Phase 2.1**: Abort with error (simple)
**Solution Phase 2.2**: Implement garbage collection
**Solution Phase 2.3**: Grow heap with additional mmap

### Challenge 3: System Call Overhead
**Problem**: Heap initialization requires mmap syscall
**Solution**: One-time cost at startup, amortized over program lifetime
**Optimization**: Pre-allocate large heap to reduce GC frequency

### Challenge 4: Debugging
**Problem**: Hard to debug machine code allocation
**Solution**:
- Add debug prints in assembly (write syscall)
- Use lldb to inspect heap memory
- Create visualization tool for heap structure

## Performance Considerations

- **Allocation Speed**: Inline allocation is ~10-100x faster than FFI
- **Memory Usage**: 1MB initial heap, grows as needed
- **Cache Locality**: Sequential allocation improves cache performance
- **GC Overhead**: Not implemented yet, will be ~5-20% overhead

## Future Enhancements (Phase 3+)

1. **Copying GC**: Semi-space collector for compaction
2. **Generational GC**: Young/old generation split
3. **Parallel GC**: Multi-threaded garbage collection
4. **Large Object Space**: Separate allocation for big objects
5. **Write Barrier**: For concurrent GC support

## References

- [ARM64 Procedure Call Standard](https://developer.arm.com/documentation/ihi0055/latest/)
- [macOS System Calls](https://opensource.apple.com/source/xnu/xnu-7195.81.3/bsd/kern/syscalls.master)
- [mmap(2) manpage](https://man7.org/linux/man-pages/man2/mmap.2.html)

---

**Status**: Planning Complete, Ready for Implementation
**Date**: 2025-11-19
**Next Step**: Implement heap initialization trampoline
