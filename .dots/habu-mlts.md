---
title: Add char-at as compiler primitive
status: closed
priority: 1
issue-type: feature
assignee: ""
created-at: "2025-12-05T13:45:55.43802+02:00"
closed-at: "2025-12-05T13:51:11.22451+02:00"
close-reason: ""
---

char-at is used throughout the reader but is not a primitive. Add it to both compilers (compiler.lisp and compiler-sbcl.lisp) so it compiles to: (if (>= pos (string-length str)) 0 (string-ref str pos))
