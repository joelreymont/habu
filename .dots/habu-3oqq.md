---
title: Implement match/match-case pattern matching
status: closed
priority: 2
issue-type: feature
assignee: ""
created-at: "2025-12-05T14:41:25.654503+02:00"
closed-at: "2025-12-05T15:02:02.433281+02:00"
close-reason: ""
---

Add OCaml-style pattern matching to Habu. Not in CL spec but extremely useful for compiler code.

Syntax proposal:
```lisp
(match expr
  ((cons a b) ...)           ; destructure cons
  ((list x y z) ...)         ; match exact list length
  ((list* x y rest) ...)     ; match list with rest
  (nil ...)                  ; match nil
  ((quote foo) ...)          ; match literal symbol
  (123 ...)                  ; match literal number
  ((vector a b) ...)         ; destructure vector
  (_ ...))                   ; wildcard/default
```

Features:
- Destructuring binds (like let but from matched structure)
- Literal matching (numbers, symbols, strings)
- Nested patterns
- Wildcard _ for don't-care positions
- Exhaustiveness warning (optional)
- Guards with `when` clause: `((cons a b) when (> a 0) ...)`

Implementation:
- Macro that expands to nested if/let/typecase
- Could reuse existing destructuring-bind if available
- Compiler can optimize common patterns
