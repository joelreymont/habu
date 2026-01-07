---
title: Optimize write barrier for JIT code
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-06T06:33:23.820991+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

## Context

From GC analysis: Generational GC uses write barriers to track old→young pointers. JIT code will exercise this frequently during mutation operations (setf, rplaca, etc.).

## Goal

Make write barrier as cheap as possible for generated code - inline where feasible, branch-prediction-friendly.

## Current State

Need to audit runtime/gc.c for write barrier implementation and ensure it's:
1. Inlined (not a function call per write)
2. Fast-path friendly (common case falls through)

## Design (from HABU_GC_IMPROVEMENTS.md)

### Inline Write Barrier

```c
// In header file (habu.h or gc.h)
static inline void gc_write_barrier(void *old_obj, habu_value_t new_value) {
    // Fast path: new_value not a young pointer → return immediately
    if (!is_pointer(new_value) || is_old_gen(new_value)) {
        return;
    }
    
    // Slow path: record in remembered set
    gc_write_barrier_slow(old_obj, new_value);
}
```

### ARM64 Codegen Integration

For setcar/setcdr operations, emit barrier inline:

```asm
; setcar(cons_ptr, new_value)
; x0 = cons_ptr, x1 = new_value

; Fast check: is new_value a young pointer?
tst x1, #0x7          ; Check if pointer
bne .no_barrier        ; Not a pointer, skip barrier

; Check if young (compare against young_start/young_end)
adrp x2, young_start
ldr x2, [x2, :lo12:young_start]
cmp x1, x2
blo .no_barrier        ; Below young gen, skip

adrp x2, young_end
ldr x2, [x2, :lo12:young_end]
cmp x1, x2
bhs .no_barrier        ; Above young gen, skip

; Slow path: call runtime
bl gc_write_barrier_slow

.no_barrier:
; Actual store
str x1, [x0, #0]
```

## Tasks

1. Audit current write barrier implementation in runtime/gc.c
2. Convert to static inline if currently a function
3. Ensure fast path is branch-prediction-friendly
4. Add ARM64 inline sequence to arm64/codegen.lisp for mutation ops
5. Benchmark before/after (micro and macro benchmarks)
6. Document barrier ABI in docs/runtime/GC_NATIVE.md

## Rationale

Since GC already exports fast-path allocation pointers for inline bump allocation, doing the same for write barriers keeps JIT code tight and fast.

## Priority

Medium - Not critical initially but important for mature JIT performance

## References

- /Users/joel/Downloads/habu-jit-gc-package/Habu_GC_Analysis.md
- Section 2.3: "Make write barrier as cheap as possible"
