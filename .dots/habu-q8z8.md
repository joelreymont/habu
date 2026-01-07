---
title: Add unboxing pass for numeric operations
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-05T21:01:37.552028+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

## Context

All values in Habu are currently tagged (4-bit tag), even in numeric-heavy loops. Unboxing eliminates tagging overhead by using raw 64-bit integers and IEEE doubles.

## Goal

Implement representation analysis and unboxing pass to use native arithmetic in inner loops.

## Expected Impact

**Performance**: 5-10x speedup on numeric loops
**Combined with type inference**: Approach C-like performance on numeric code

## Design

### Representation Classes

- `tagged` - Tagged value (current)
- `int64` - Unboxed 64-bit integer (new)
- `double` - Unboxed IEEE 754 double (new)

### Two Passes

1. **U1: Choose Representation**
   - Based on type info, decide tagged vs. unboxed
   - Fixnum → `int64` in most contexts
   - Double → `double` in most contexts
   - Force `tagged` for closures, containers, generic calls

2. **U2: Insert Box/Unbox**
   - Add explicit boxing/unboxing operations
   - Box on escape (return, store to heap)
   - Unbox on entry (parameters, loads from heap)
   - Optimize box/unbox pairs away

## Example

```lisp
;; Source
(defun sum-loop (n)
  (let ((i 0) (acc 0))
    (loop
      (if (>= i n)
          (return acc)
          (progn
            (setq acc (+ acc i))
            (setq i (+ i 1)))))))

;; With unboxing (pseudocode)
;; i, n, acc all unboxed (int64)
;; Arithmetic uses native CPU add, no tagging
;; Result boxed only at return
```

## Implementation Tasks

1. Implement `RepInfo` data structure
2. Pass U1: Choose representation based on type inference
3. Pass U2: Insert box/unbox operations
4. Update codegen to handle unboxed values
5. Box/unbox optimization (remove redundant pairs)

## Prerequisites

- **Type inference** (habu-8hdb) - Must know types before choosing representations
- MIR with register classes (GPR vs. FPR)

## Integration with JIT (NEW)

From JIT design analysis:

Unboxing works naturally with function versioning:
- Generic version: all tagged (current)
- Specialized version: unboxed based on rep-hints
- Example: `(add :rep (:fixnum :fixnum))` → unboxed int64 add

Rep-hints (habu-ggd7) guide both:
1. Type specialization (which operations)
2. Representation choice (boxed vs. unboxed)

### Phased Implementation

**Phase 1**: Unbox within function (local variables)
- Easier to implement
- Still significant wins
- No calling convention changes

**Phase 2**: Unbox across calls (requires versioning)
- Function takes/returns unboxed values
- Requires multiple versions per calling convention
- Depends on habu-07lo (function versioning)

## ARM64 Details

### Unboxed Fixnum
```asm
;; Tagged: x0 = 42 << 4 = 672
;; Unboxed: x0 = 42

;; Unbox:
asr x0, x0, #4

;; Box:
lsl x0, x0, #4
```

### Unboxed Double
```asm
;; Unbox (load from heap object):
ldr d0, [x0, #8]  ; x0 points to boxed double

;; Box (allocate and store):
;; Allocate 16-byte object
;; Store double tag
;; Store value
```

## References

- `docs/compiler-theory/NANOPASS_COMPILER_SPEC.md` - Section 4 on unboxing
- `docs/compiler-theory/LISPY_ENCODING.md` - RepInfo encoding
- SBCL unboxing, Chez Scheme's representation analysis
- /Users/joel/Downloads/habu-jit-gc-package/Habu_Codegen_and_JIT.md

## Phasing

1. **Phase 1**: Unbox fixnums in local variables
2. **Phase 2**: Unbox doubles
3. **Phase 3**: Optimize box/unbox pairs
4. **Phase 4**: Unbox across function boundaries (calling convention)

## Related Issues

- habu-8hdb: Type inference (prerequisite)
- habu-ggd7: IR representation hints (complementary)
- habu-07lo: Function versioning (enables cross-function unboxing)

## Priority

**High** - Large performance win, but depends on type inference
