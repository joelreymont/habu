---
title: Implement type system with CL types first
status: closed
priority: 2
issue-type: feature
assignee: ""
created-at: "2025-12-05T15:11:50.629934+02:00"
closed-at: "2025-12-25 07:21:40"
close-reason: "Obsolete: Zig rewrite"
---

Implement type system for C-like performance and FFI. Phased approach:

Phase 1 - CL Type Declarations:
- Parse (declare (type ...)) forms
- Use for optimization (unbox fixnums, inline array access)
- Honor (the type expr) for local type assertions
- Support: fixnum, (integer lo hi), (simple-array type dims), (member ...), (satisfies pred)

Phase 2 - Simple Refinements:
- Add (declare (refine expr)) for boundary predicates
- Enable bounds check elimination with relational constraints
- Check refinements at function boundaries

Phase 3 - Full Refinements (optional):
- Dependent function types
- SMT solver integration for verification
- Typed/untyped boundaries with blame tracking

Implementation should be shared nanopass in types.lisp (like expand.lisp).

See docs/type-system-analysis.md for full analysis.
