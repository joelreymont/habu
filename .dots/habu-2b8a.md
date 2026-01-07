---
title: Make register allocator work in native Habu
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-04T23:00:45.475298+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

The register allocator (reg-alloc.lisp) is currently #+sbcl only. For self-hosting, it needs to work in native Habu (Stage 1).

Current state:
- `*use-register-allocation*` is #+sbcl only
- reg-alloc.lisp may have other SBCL dependencies

Required:
1. Add #-sbcl versions of `*use-register-allocation*` and related code
2. Verify reg-alloc.lisp compiles to native
3. Test linear-scan and tac-codegen work in native mode
4. Enable by default for better code quality and smaller stack frames
