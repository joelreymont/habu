---
title: Finish logical pathname semantics
status: open
priority: 1
issue-type: task
created-at: "2026-04-05T11:37:31.482863+02:00"
blocks:
  - habu-fix-nlx-control-dcb701b2
---

Problem: PLAN.md 2.3 still needs logical-pathname, translate-pathname, translate-logical-pathname, batch stream, and pathname mutation semantics closed generically for Maxima batch/share runtime. Acceptance: canonical batch/test-batch/share paths use truthful logical and physical pathname semantics with no fake success. Files: PLAN.md:694-743, src/runtime/primitives/pathname.zig, src/runtime/primitives/io.zig, src/interp/vm.zig, tools/maxima-rtest.lisp. Verify: focused pathname/stream regressions plus canonical batch/share smoke.
