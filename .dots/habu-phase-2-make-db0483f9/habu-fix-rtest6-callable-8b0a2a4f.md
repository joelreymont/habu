---
title: Fix rtest6 callable operators
status: open
priority: 1
issue-type: task
created-at: "2026-04-05T11:37:31.469327+02:00"
blocks:
  - habu-fix-rtest6-infix-0e6c5697
---

Problem: PLAN.md 2.1b is still undotted: after dynamic operator/read-state is fixed, canonical rtest6 still needs generic subscripted-callable and operator-expression semantics rather than Maxima-only handling. Acceptance: operator expressions and subscripted callable forms in canonical rtest6 compile and run generically, exposing the next real failure instead of parser/runtime shape mismatches. Files: PLAN.md:597-619, ../maxima/tests/rtest6.mac, src/compiler/compile.zig, src/interp/repl.zig, src/interp/vm.zig. Verify: canonical tools/maxima-rtest.lisp rtest6 advances past the operator-expression cluster.
