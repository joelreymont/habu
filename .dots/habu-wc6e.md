---
title: Convert to ANF (A-Normal Form) IR
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-05T21:01:59.599581+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

## Context

Habu's current IR is tree-structured, making dataflow analysis difficult. A-Normal Form linearizes expressions, making optimization passes simpler and more powerful.

## Goal

Add ANF conversion pass to transform tree IR into linear form with explicit temporaries.

## What is ANF?

Every intermediate value gets a name:

```lisp
;; Tree form (current)
(+ (* x 2) y)

;; ANF (proposed)
(let ((tmp1 (* x 2)))
  (let ((tmp2 (+ tmp1 y)))
    tmp2))
```

## Benefits

1. **Dataflow analysis**: Each variable has single definition point
2. **Optimization**: Easy to track value flow
3. **Type inference**: Natural forward propagation
4. **Code motion**: Clear dependency structure
5. **Debugging**: Easier to trace values

## Implementation

### Conversion Algorithm

```scheme
;; normalize-expr : Expr -> (ANF-Expr, Bindings)
(defun normalize-expr (expr)
  (match expr
    ;; Atomic expressions (already in ANF)
    ((var v) => (values expr '()))
    ((lit n) => (values expr '()))
    
    ;; Complex expressions (need temporaries)
    ((prim op args)
     (let ((norm-args bindings (normalize-args args)))
       (let ((tmp (gensym)))
         (values tmp
                 (append bindings
                         (list (let tmp (prim op norm-args))))))))
    ...))
```

### Output Format

Use same format as HIR in spec:

```scheme
(block 'entry "entry"
  ((let v0 (var x))
   (let v1 (lit 2))
   (let v2 (prim 'mul (v0 v1)))
   (let v3 (var y))
   (let v4 (prim 'add (v2 v3)))
   (term (return v4))))
```

## Tasks

1. Implement ANF conversion for expressions
2. Handle control flow (if, cond, loop)
3. Preserve tail calls
4. Generate fresh variable names
5. Update compiler to use ANF IR (optionally)

## Integration Strategy

### Option 1: Keep both IRs
- Frontend produces tree IR
- ANF conversion for optimization passes
- Codegen from either IR

### Option 2: Replace tree IR
- Frontend directly to ANF
- All passes use ANF
- Simpler overall

## References

- `docs/compiler-theory/NANOPASS_COMPILER_SPEC.md` - Section 2 on HIR
- `docs/compiler-theory/LISPY_ENCODING.md` - ANF encoding
- "The Essence of Compiling with Continuations" by Flanagan et al.
- Chez Scheme, Racket for ANF implementations

## Dependencies

None - this is foundational infrastructure

## Enables

- Type inference (habu-8hdb)
- Unboxing (habu-q8z8)
- CFG construction
- All advanced optimizations

## Priority

**Medium-High** - Not immediately required, but enables many optimizations
