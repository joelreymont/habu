---
title: Unify bootstrap and native compilers into single source
status: closed
priority: 2
issue-type: feature
assignee: ""
created-at: "2025-12-08T19:18:07.90116+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

Currently have 3 divergent compilers: bootstrap/compiler-sbcl.lisp, bootstrap/compiler.lisp, habu0.lisp h0-compile. They have inconsistencies (string= vs string-equal, different special form handling). 

Architecture: Create host-compat.lisp API layer with host-sbcl.lisp and host-habu0.lisp implementations. Shared core compiler calls only host API. This is how SBCL/Clasp/Chicken achieve self-hosting.

Steps:
1. Draft host-compat.lisp API (string=, char-code, aref, hash ops, I/O)
2. Create SBCL and habu0 implementations
3. Lift special-form dispatcher into shared module
4. Define shared IR
5. Add cross-host test suite
6. Delete duplicated compiler code
