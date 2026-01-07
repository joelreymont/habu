---
title: Build Control Flow Graph (CFG) infrastructure
status: closed
priority: 3
issue-type: task
assignee: ""
created-at: "2025-12-05T21:02:21.17356+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

## Context

Current compiler operates on tree-structured IR. CFG (Control Flow Graph) representation enables advanced analysis and optimization.

## Goal

Build CFG from ANF IR, representing functions as basic blocks with explicit control flow edges.

## CFG Structure

```scheme
;; Basic block
(block id label
  (instructions ...)
  (terminator ...))

;; Function with CFG
(function func-id name params entry-block-id
  (blocks ...))
```

### Terminators

- `(if cond then-block else-block)` - conditional branch
- `(jump target)` - unconditional jump
- `(return value)` - function return

### Edges

- True edge: if condition is true
- False edge: if condition is false
- Fall-through: jump, sequence

## Benefits

1. **Dataflow analysis**: Forward/backward propagation
2. **Dominance**: Understand control dependencies
3. **Loop detection**: Natural loop finding
4. **Optimization**: Dead code elimination, constant propagation
5. **Liveness**: Precise register allocation

## Implementation Tasks

1. **CFG construction**
   - Split ANF into basic blocks (at control flow)
   - Build edge list
   - Verify CFG properties

2. **CFG analysis**
   - Dominance tree
   - Post-order traversal
   - Loop detection (natural loops)

3. **Dataflow framework**
   - Forward flow (reaching definitions)
   - Backward flow (liveness)
   - Generic fixpoint solver

4. **CFG transformations**
   - Block merging
   - Dead block elimination
   - Critical edge splitting

## Example

```lisp
;; Source
(defun abs (x)
  (if (< x 0)
      (- 0 x)
      x))

;; CFG (3 blocks)
;; Block entry:
;;   (let v0 (var x))
;;   (let v1 (lit 0))
;;   (let v2 (prim '< (v0 v1)))
;;   (term (if v2 'then 'else))
;;
;; Block then:
;;   (let v3 (lit 0))
;;   (let v4 (prim '- (v3 v0)))
;;   (term (jump 'exit v4))
;;
;; Block else:
;;   (term (jump 'exit v0))
;;
;; Block exit:
;;   (param result)
;;   (term (return result))
```

## CFG Properties to Verify

- Single entry block
- All blocks reachable
- No unreachable blocks (or remove them)
- Terminators at end of blocks only

## Analysis Passes Enabled

Once CFG is built:

1. **Liveness analysis** (for register allocation)
2. **Constant propagation**
3. **Dead code elimination**
4. **Common subexpression elimination**
5. **Loop-invariant code motion**

## References

- `docs/compiler-theory/NANOPASS_COMPILER_SPEC.md` - Section 2 on CFG
- "Engineering a Compiler" by Cooper and Torczon (Chapter 8-9)
- LLVM IR documentation
- SSA book by Cytron et al.

## Prerequisites

- ANF conversion (habu-wc6e) - CFG works best with linearized IR

## Priority

**Medium** - Enables advanced optimizations but not immediately critical
