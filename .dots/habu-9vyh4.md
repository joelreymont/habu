---
title: Implement dataflow analysis nanopass for nil-check elimination
status: closed
priority: 2
issue-type: feature
assignee: ""
created-at: "2025-12-11T14:21:17.038573+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

Add a dataflow analysis pass to the compiler that tracks known-non-nil values. When a value has been checked against nil (via if, when, consp, etc.), propagate that knowledge to dominated blocks so car/cdr can skip redundant nil checks.

Key cases to handle:
1. After (if x ...) - x is known non-nil in then-branch
2. After (when x ...) - x is known non-nil in body
3. After (consp x) - x is known cons (and non-nil) in then-branch
4. Loop variables after null check - known non-nil in body
5. After (null x) returning false - x is known non-nil

Implementation approach:
- Add IR annotation for "known non-nil" values
- Propagate through dominator tree
- car/cdr codegen checks annotation before emitting nil check
- Consider using bit vectors for efficient tracking

This enables nil=0 tagging scheme to have zero overhead on car/cdr in most real code.
