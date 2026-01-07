---
title: Implement static type checking via CL type annotations
status: closed
priority: 2
issue-type: feature
assignee: ""
created-at: "2025-12-05T14:11:28.878122+02:00"
closed-at: "2025-12-25 07:21:40"
close-reason: "Obsolete: Zig rewrite"
---

Implement compile-time type checking using Common Lisp type declarations (declare, the, check-type). Like SBCL, use type annotations to catch:
- Type mismatches at call sites
- Undefined variables
- Incorrect return types
- Invalid slot accesses

CL already specifies these declarations - we just need to enforce them. Should integrate with habu-xnjq (static verification mode) to provide comprehensive compile-time error detection.

Key forms to support:
- (declare (type fixnum x))
- (the fixnum expr)
- (check-type x fixnum)
- ftype declarations for function signatures

Depends on: habu-xnjq (full static verification)
