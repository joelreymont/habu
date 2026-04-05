---
title: Bring defint residu clean
status: open
priority: 1
issue-type: task
created-at: "2026-04-05T11:37:31.504956+02:00"
blocks:
  - habu-make-maxima-rtest-e12c672e
---

Problem: PLAN.md 2.5 still needs a clean-scope leaf after runner canonicalization so defint.lisp and residu.lisp become real generic correctness gates rather than ad hoc side checks. Acceptance: canonical load/run path reaches defint/residu with no semantic patches; remaining failures are concrete Habu gaps with focused regressions. Files: PLAN.md:770-783, ../maxima/src/defint.lisp, ../maxima/src/residu.lisp, tools/maxima-rtest.lisp, src/compiler/compile.zig, src/interp/vm.zig. Verify: canonical defint/residu smoke plus focused regressions for any exposed runtime/compiler bugs.
