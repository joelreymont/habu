---
title: Implement type inference for arithmetic specialization
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-05T21:01:14.453027+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

## Context

Habu currently uses fully generic arithmetic operations. Type inference would enable specialization to native operations, eliminating runtime type checks and tag manipulation.

## Goal

Implement a basic type inference pass that tracks fixnum vs. other types, enabling specialized arithmetic operations.

## Expected Impact

**Performance**: 2-5x speedup on numeric code
**Code quality**: Direct CPU arithmetic instead of generic dispatch

## Type System

Start with simple lattice:
- `fixnum` - tagged integers
- `double` - floating point (boxed)
- `pair` - cons cells
- `top` - unknown type
- `union` - union types

## Implementation Tasks

1. **Pass T1: Basic type inference**
   - Forward dataflow over CFG
   - Track types through operations
   - Handle literals, variables, primitives

2. **Pass T2: Branch refinement** (optional, later)
   - Refine types based on predicates: `(if (fixnum? x) ...)`
   - Enable conditional unboxing

3. **Pass T3: Primitive specialization**
   - Rewrite `+` → `add-fixnum` when operands are fixnums
   - Similarly for `-`, `*`, `/`, comparison ops

4. **Update codegen**
   - Generate direct ARM64 `add` for `add-fixnum`
   - No type checking, no tag manipulation in hot path

## Prerequisites

- ANF conversion (or adapt to tree IR initially)
- TypeInfo data structure (see `docs/compiler-theory/LISPY_ENCODING.md`)

## Incremental Approach

1. Start with literals and parameters
2. Add arithmetic operations
3. Extend to all primitives
4. Add control flow (if/cond)
5. Add function calls

## Integration with JIT Versioning (NEW)

From JIT design analysis:

- Type inference populates rep-hints for function arguments
- Rep-hints used by compile-specialized to generate versions
- Example: `(+ x y)` with inferred fixnum types → rep-hint `'(:fixnum :fixnum)`
- JIT dispatcher uses rep-hints to select appropriate version

### Connection to habu-ggd7

This issue provides the **input** for IR representation hints (habu-ggd7):
1. Type inference determines argument types
2. Rep-hints mechanism (habu-ggd7) stores and uses those types
3. Function versioning (habu-07lo) compiles specialized versions

## References

- `docs/compiler-theory/NANOPASS_COMPILER_SPEC.md` - Section 3 on type system
- `docs/compiler-theory/LISPY_ENCODING.md` - Type encodings
- Typed Racket, SBCL for inspiration
- /Users/joel/Downloads/habu-jit-gc-package/Habu_Codegen_and_JIT.md - Section 2.1

## Related Issues

- habu-ggd7: IR representation hints (consumes type info from this)
- habu-07lo: JIT function versioning (uses specialized types)
- habu-q8z8: Unboxing pass (depends on this)

## Priority

**High** - Major performance win with moderate complexity, foundational for JIT
