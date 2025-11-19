# Inline Allocation - Phase 2

## Overview

Phase 2 transforms Habu from a bootstrap compiler (using SBCL FFI) to a standalone compiler that generates self-contained machine code.

**Goal:** Remove all SBCL dependencies by generating inline allocation code instead of calling FFI trampolines.

## Current Architecture (Phase 1)

```
Compiled Code
    ↓ (call FFI trampoline)
FFI Trampoline (alien-callable)
    ↓ (call Lisp function)
Habu Runtime Function (runtime/lists.lisp)
    ↓ (call SBCL functions)
SBCL Runtime (memory allocation, GC, etc.)
```

**Dependencies:**
- SBCL for memory allocation
- SBCL for GC
- SBCL's alien-callable mechanism
- Cannot run without SBCL

## Target Architecture (Phase 2)

```
Compiled Code (with inline allocation)
    ↓ (direct memory access)
Habu Heap (standalone memory management)
    ↓ (when full)
Habu GC (standalone garbage collector)
```

**No Dependencies:**
- Self-contained memory allocation
- Self-contained GC
- No FFI trampolines
- Runs without SBCL

## Implementation Strategy

### Step 1: Inline Cons Allocation

Replace FFI trampoline call with inline allocation code.

**Before (Phase 1):**
```assembly
; cons(car, cdr)
mov rdi, <car>        ; arg1
mov rsi, <cdr>        ; arg2
mov rax, <trampoline-addr>
call rax              ; Call FFI trampoline
; Result in RAX
```

**After (Phase 2):**
```assembly
; cons(car, cdr) - inline version
mov rdi, <car>        ; Save car
mov rsi, <cdr>        ; Save cdr

; Get current heap pointer
mov rax, [rel heap_ptr]

; Check if we have space (need 16 bytes for cons cell)
mov rbx, [rel heap_limit]
lea rcx, [rax + 16]
cmp rcx, rbx
jge .gc_needed

; Allocate cons cell
mov [rax], rdi        ; Store car at offset 0
mov [rax + 8], rsi    ; Store cdr at offset 8
lea rcx, [rax + 16]
mov [rel heap_ptr], rcx

; Tag pointer as cons (tag 0x1)
or rax, 1
jmp .done

.gc_needed:
; Call GC to free space
call gc_collect
; Retry allocation
mov rax, [rel heap_ptr]
mov [rax], rdi
mov [rax + 8], rsi
lea rcx, [rax + 16]
mov [rel heap_ptr], rcx
or rax, 1

.done:
; RAX contains tagged cons cell pointer
```

**Size comparison:**
- Phase 1: ~20 bytes (setup args + call)
- Phase 2: ~50-60 bytes (inline allocation + GC check)
- Trade-off: Larger code, but no FFI overhead and standalone

### Step 2: Inline Car/Cdr

**Before (Phase 1):**
```assembly
; car(cons)
mov rdi, <cons>
mov rax, <car-trampoline-addr>
call rax
```

**After (Phase 2):**
```assembly
; car(cons) - inline version
mov rax, <cons>
and rax, ~0xF         ; Remove tag (0x1)
mov rax, [rax]        ; Load car (offset 0)
```

**Much simpler!** No allocation needed, just pointer arithmetic.

### Step 3: Global Heap Variables

Need global variables accessible from generated code:

```assembly
section .data
    heap_ptr:    dq 0        ; Current heap pointer
    heap_limit:  dq 0        ; End of heap
    heap_base:   dq 0        ; Start of heap
    gc_roots:    times 256 dq 0  ; GC root stack
    gc_root_sp:  dq 0        ; Root stack pointer
```

These are initialized at program startup.

### Step 4: Initialization Code

Every standalone program needs initialization:

```assembly
section .text
global _start

_start:
    ; Allocate heap (1MB via mmap/malloc)
    mov rdi, 1048576
    call allocate_heap    ; Returns heap start in RAX

    mov [rel heap_base], rax
    mov [rel heap_ptr], rax
    add rax, 1048576
    mov [rel heap_limit], rax

    ; Initialize GC root stack
    lea rax, [rel gc_roots]
    mov [rel gc_root_sp], rax

    ; Call main function
    call main

    ; Exit with result
    mov rdi, rax
    mov rax, 60          ; sys_exit
    syscall
```

### Step 5: Garbage Collection

Implement GC in generated code (not SBCL):

```assembly
gc_collect:
    push rbp
    mov rbp, rsp

    ; 1. Mark phase - traverse from roots
    lea rsi, [rel gc_roots]
    mov rcx, [rel gc_root_sp]
    sub rcx, rsi
    shr rcx, 3           ; Number of roots

.mark_loop:
    test rcx, rcx
    jz .sweep_phase
    mov rdi, [rsi]
    call gc_mark_object
    add rsi, 8
    dec rcx
    jmp .mark_loop

.sweep_phase:
    ; 2. Sweep phase - free unmarked objects
    mov rdi, [rel heap_base]
    mov rsi, [rel heap_ptr]
    call gc_sweep

    ; 3. Compact phase - move objects to remove gaps
    call gc_compact

    pop rbp
    ret
```

## Memory Layout

### Heap Structure

```
+------------------+  <- heap_base
| Cons Cell 1      |  16 bytes
| (car + cdr)      |
+------------------+
| String 1         |  Variable size
| (header + data)  |
+------------------+
| Cons Cell 2      |  16 bytes
+------------------+
| ...              |
+------------------+  <- heap_ptr (next free)
| Unused Space     |
+------------------+  <- heap_limit
```

### Object Headers

```
Cons Cell (16 bytes):
  +0:  car (8 bytes, tagged)
  +8:  cdr (8 bytes, tagged)
  Tag: 0x1 (pointer | 1)

String (variable):
  +0:  Header (length << 8 | 0x3)
  +8:  Data bytes...
  Tag: 0x3 (pointer | 3)

Symbol (48 bytes):
  +0:  Header (0x5)
  +8:  name (8 bytes)
  +16: value (8 bytes)
  +24: function (8 bytes)
  +32: plist (8 bytes)
  +40: package (8 bytes)
  Tag: 0x5 (pointer | 5)
```

## Implementation Plan

### Phase 2.1: Inline Allocation ⭐ (Start Here)

**Goal:** Generate inline code for cons, car, cdr

1. Add global heap variables to generated code
2. Implement inline cons allocation
3. Implement inline car/cdr access
4. Add heap bounds checking
5. Add GC call on heap full
6. Test with simple programs

**Scope:** Still using SBCL for GC, just inlining allocation

### Phase 2.2: Standalone GC

**Goal:** Implement GC without SBCL

1. Implement mark phase in machine code
2. Implement sweep phase
3. Implement compaction
4. Stack scanning for roots
5. Test GC correctness

**Scope:** Complete memory management without SBCL

### Phase 2.3: Standalone Runtime

**Goal:** Remove all SBCL dependencies

1. Implement string operations inline
2. Implement symbol table in generated code
3. Implement reader/printer inline
4. Implement file I/O via syscalls
5. Test complete runtime

**Scope:** Fully standalone Habu programs

### Phase 2.4: Executable Generation

**Goal:** Generate real executables

1. ELF format for Linux (x86_64)
2. Mach-O format for macOS (ARM64)
3. Linking multiple modules
4. Symbol resolution
5. Relocation

**Scope:** Standalone .exe files

## Code Changes Required

### 1. Compiler Modifications

Add new code generation mode:

```lisp
(defparameter *allocation-mode* :ffi
  "Allocation strategy: :ffi (Phase 1) or :inline (Phase 2)")

(defun emit-cons (car-expr cdr-expr env)
  (ecase *allocation-mode*
    (:ffi
     ;; Current: Call FFI trampoline
     (emit-cons-via-ffi car-expr cdr-expr env))

    (:inline
     ;; New: Inline allocation
     (emit-cons-inline car-expr cdr-expr env))))
```

### 2. Heap Management Module

New file: `runtime/heap-inline.lisp`

```lisp
(defun emit-heap-globals (arch)
  "Generate global variables for heap management"
  (ecase arch
    (:x86_64
     (list
      "section .data"
      "heap_ptr:   dq 0"
      "heap_limit: dq 0"
      "heap_base:  dq 0"))

    (:arm64
     ;; ARM64 version
     )))

(defun emit-heap-init (arch)
  "Generate heap initialization code"
  (ecase arch
    (:x86_64
     '(; Allocate 1MB heap via mmap
       #x48 #xC7 #xC7 ...))  ; mov rdi, 1048576

    (:arm64
     ;; ARM64 version
     )))
```

### 3. Inline Allocators

New file: `compiler-inline.lisp`

```lisp
(defun emit-cons-inline (car-expr cdr-expr env)
  "Generate inline cons allocation code"
  (let ((car-code (emit-x86_64 car-expr env))
        (cdr-code (emit-x86_64 cdr-expr env)))
    (append
     ;; Evaluate car
     car-code
     '(#x48 #x89 #xC7)  ; mov rdi, rax (save car)

     ;; Evaluate cdr
     cdr-code
     '(#x48 #x89 #xC6)  ; mov rsi, rax (save cdr)

     ;; Get heap_ptr
     '(#x48 #x8B #x04 #x25) (int-to-bytes heap-ptr-addr 4)

     ;; Check space
     '(#x48 #x8B #x1C #x25) (int-to-bytes heap-limit-addr 4)
     '(#x48 #x8D #x48 #x10) ; lea rcx, [rax+16]
     '(#x48 #x39 #xD9)      ; cmp rcx, rbx
     '(#x0F #x8D) (int-to-bytes gc-offset 4)

     ;; Allocate
     '(#x48 #x89 #x38)      ; mov [rax], rdi (store car)
     '(#x48 #x89 #x70 #x08) ; mov [rax+8], rsi (store cdr)
     '(#x48 #x8D #x48 #x10) ; lea rcx, [rax+16]
     '(#x48 #x89 #x0C #x25) (int-to-bytes heap-ptr-addr 4)

     ;; Tag as cons
     '(#x48 #x83 #xC8 #x01) ; or rax, 1
     )))
```

## Testing Strategy

### Test 1: Simple Cons

```lisp
;; Test inline cons allocation
(let ((x (cons 1 2)))
  (+ (car x) (cdr x)))
; Expected: 3
```

**Verify:**
- Inline code generated (no FFI calls)
- Heap pointer advances correctly
- Tagged pointer returned
- car/cdr extract correct values

### Test 2: List Building

```lisp
;; Test multiple allocations
(list 1 2 3 4 5)
; Expected: (1 2 3 4 5)
```

**Verify:**
- Multiple cons cells allocated
- Heap pointer advances correctly
- No memory corruption

### Test 3: GC Trigger

```lisp
;; Test GC when heap is full
(let loop ((i 0) (result 0))
  (if (< i 100000)
      (loop (+ i 1) (+ result i))
      result))
```

**Verify:**
- GC triggers when heap is full
- Memory is reclaimed
- Program continues correctly

## Benefits

### Performance

- **Faster allocation:** No FFI overhead
- **Better inlining:** Compiler sees allocation code
- **Smaller binaries:** No SBCL dependency

### Portability

- **Standalone:** No runtime dependencies
- **Cross-platform:** Generate ELF, Mach-O, PE formats
- **Embeddable:** Can link into other programs

### Development

- **Simpler deployment:** Single executable
- **Easier debugging:** All code visible
- **Better optimization:** Full control over code generation

## Timeline

- **Phase 2.1 (Inline Allocation):** 1-2 weeks
- **Phase 2.2 (Standalone GC):** 2-3 weeks
- **Phase 2.3 (Standalone Runtime):** 3-4 weeks
- **Phase 2.4 (Executable Generation):** 2-3 weeks

**Total:** ~2-3 months for complete standalone operation

## Next Steps

1. ✅ Document inline allocation strategy (this file)
2. Add `*allocation-mode*` parameter to compiler
3. Implement `emit-cons-inline` for x86_64
4. Implement `emit-car-inline` and `emit-cdr-inline`
5. Add heap globals to code generation
6. Test simple cons/car/cdr operations
7. Extend to lists and other operations
8. Port to ARM64
9. Implement standalone GC
10. Generate real executables

---

**Current Status:** Phase 1 complete, starting Phase 2.1 (Inline Allocation)
