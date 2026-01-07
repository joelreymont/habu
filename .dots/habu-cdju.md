---
title: Implement precise GC stack maps
status: closed
priority: 3
issue-type: task
assignee: ""
created-at: "2025-12-05T21:02:45.775096+02:00"
closed-at: "2025-12-25 07:21:12"
close-reason: "Obsolete: Zig rewrite"
---

## Context

Habu's current GC has basic safe points at allocation sites. Precise stack maps would track exactly which registers and stack slots contain GC roots at each safe point.

## Goal

Implement precise GC metadata with stack maps for all safe points, enabling better GC and memory safety.

## Benefits

1. **Better GC**: Can move objects safely
2. **Memory safety**: Guaranteed root coverage
3. **Debugging**: Can inspect all live objects
4. **Verification**: Prove GC correctness

## Design

### Safe Points

Mark points where GC can safely interrupt:
- Function calls
- Heap allocation (`gc-alloc`)
- Loop back edges (for long-running loops)

### Stack Maps

For each safe point, record:
- Which registers contain tagged pointers
- Which stack slots contain tagged pointers
- GC uses tag bits to verify (0000 = pointer)

```scheme
(safe-point id func-id block-id instr-index)

(stack-map-entry safe-point-id
  ((reg (x 0))         ; x0 contains root
   (reg (x 1))         ; x1 contains root
   (stack-slot -16)))  ; [FP-16] contains root
```

## Implementation Tasks

1. **Pass: Insert safe points**
   - Tag all `call`, `gc-alloc` instructions
   - Add loop back edge safe points

2. **Pass: Compute liveness**
   - Backward dataflow (same as for RA)
   - Track which vregs are live at each safe point

3. **Pass: Build stack maps**
   - Post-register allocation
   - For each safe point:
     - Get live vregs
     - Look up physical location (reg or stack)
     - Filter to tagged values (exclude int64, double)
     - Record in stack map

4. **Runtime integration**
   - GC reads stack map on interrupt
   - Scans registers and stack for roots
   - Moves objects, updates pointers

## Example

```asm
;; Function with safe point at call
entry:
  mov x0, #16          ; size
  bl gc_allocate       ; <-- safe point 0
  ;; At SP-0: x30 (return addr), maybe x19-x22 if used
  ;; Stack map: ((reg (x 30)) (stack-slot -8) (stack-slot -16))
```

## Challenges

1. **Conservative roots**: Callee-saved registers may contain old values
2. **Interior pointers**: Might point inside objects (disallow or track)
3. **Uninitialized slots**: Must distinguish uninitialized from nil

## Solutions

1. **Clear registers**: Zero unused callee-saved regs on entry
2. **No interior pointers**: Only allow pointers to object start
3. **Initialize stack**: Clear stack frame in prologue

## References

- `docs/compiler-theory/NANOPASS_COMPILER_SPEC.md` - Section 7 on GC
- `docs/compiler-theory/LISPY_ENCODING.md` - Stack map encoding
- "Garbage Collection" by Jones and Lins
- SBCL GC implementation
- Go runtime stack maps

## Prerequisites

- Register allocation (habu-qrr6) - Need to know where values are
- MIR (for safe point insertion)

## Priority

**Medium** - Improves GC but not critical for performance
