---
title: Complete linear-scan register allocation implementation
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-05T21:00:57.637337+02:00"
closed-at: "2025-12-25 07:21:12"
close-reason: "Obsolete: Zig rewrite"
---

## Context

The register allocation pass in `bootstrap/reg-alloc.lisp` is partially implemented with the nanopass architecture in place, but needs completion.

## Current Status

- TAC (Three-Address Code) framework exists
- Virtual register infrastructure present
- Accumulator model still in use (everything goes to x0)

## Goal

Replace the accumulator model with full linear-scan register allocation to eliminate excessive spilling.

## Expected Impact

**Performance**: 5-20x speedup (eliminates most stack spills)
**Code size**: Smaller functions (right-sized stack frames)

## Implementation Tasks

1. Complete `compute-liveness` pass (backward dataflow)
2. Implement `compute-intervals` (build live ranges)
3. Finish `linear-scan` allocator (assign physical registers)
4. Update `tac-codegen` to use allocation results
5. Handle register pressure and spilling gracefully

## References

- `bootstrap/reg-alloc.lisp` - Current implementation
- `docs/compiler-theory/NANOPASS_COMPILER_SPEC.md` - Section 8 on register allocation
- Standard reference: "Linear Scan Register Allocation" by Poletto and Sarkar

## Priority

**Critical** - This is the foundation for all other optimizations and provides the largest single performance improvement.
