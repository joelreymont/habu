---
title: Design incremental GC with safepoints
status: closed
priority: 3
issue-type: feature
assignee: ""
created-at: "2025-12-06T06:33:42.847699+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

## Context

From GC analysis: runtime/gc.c is labeled "Incremental Generational GC" but currently does stop-the-world collection. The infrastructure (gray stack, gc_state enum) exists but isn't fully wired.

## Goal

Add true incremental collection via bounded-work GC steps called from safepoints in generated code.

## Design (from HABU_GC_IMPROVEMENTS.md)

### Incremental Collection API

```c
// Perform bounded GC work (collect up to budget_bytes)
void gc_collect_young_step(size_t budget_bytes);
void gc_collect_full_step(size_t budget_bytes);

// Called from safepoints
void gc_poll(void) {
    if (gc_heap->gc_needed) {
        gc_collect_young_step(4096);  // 4KB of work
    }
}
```

### Safepoint Integration

ARM64 codegen inserts safepoint calls at:
- Function returns
- Loop back-edges
- After N allocations

```asm
; Loop back-edge safepoint
.loop_start:
  ; ... loop body ...
  
  ; Safepoint poll
  bl gc_poll
  
  b .loop_start
```

### Implementation Plan

1. **Keep stop-the-world as baseline**
   - Current implementation stays canonical
   - Incremental mode is additive

2. **Add step functions**
   - gc_collect_young_step: copy N bytes, update state
   - gc_collect_full_step: mark/sweep N bytes, update state
   - Both interruptible and resumable

3. **Add gc_poll() for safepoints**
   - Checks if GC work needed
   - Calls appropriate step function
   - Minimal overhead when no GC needed

## Benefits

- Lower pause times (spread GC work across execution)
- Better interactive responsiveness
- Natural fit with JIT (safepoints = optimization points)

## Challenges

- Read/write barriers must work with partial collections
- State management complexity
- Need careful testing for correctness

## Phasing

1. **Phase 1**: Design and document safepoint ABI
2. **Phase 2**: Implement gc_poll() stub (calls current stop-the-world)
3. **Phase 3**: Add safepoint codegen to arm64/codegen.lisp
4. **Phase 4**: Implement true incremental steps
5. **Phase 5**: Tune step sizes and safepoint frequency

## Dependencies

- Current stop-the-world GC (working)
- Safepoint ABI documentation

## Enables

- Low-latency interactive REPL
- Real-time-friendly GC
- Better JIT integration (safepoints also good for OSR, debugging)

## References

- /Users/joel/Downloads/habu-jit-gc-package/Habu_GC_Analysis.md
- Section 2.4: "Prepare for true incremental collection"
- Baker's incremental copying collector
- Azul C4 collector for continuous collection ideas
