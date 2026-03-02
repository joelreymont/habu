---
title: Canonicalize function and macro lookup
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.116387+02:00"
blocks:
  - habu-canonicalize-loader-specials-4fcbd54f
---

Problem: generic lookup still contains Maxima-specific autoload and dollar-prefix behavior. Acceptance: symbol-function, macro-function, autoload, and special lookup are package-correct and generic. Files: src/interp/repl.zig:1565-1595,4209-4255; ../maxima/src/suprv1.lisp:144-175; ../maxima/src/mlisp.lisp:2037-2117. Verify: lookup regressions and rg for MAXIMA:AUTOLOAD or dollar-prefix special cases. Blockers: habu-canonicalize-loader-specials-4fcbd54f; also depends on habu-fix-macro-table-a8759987.
