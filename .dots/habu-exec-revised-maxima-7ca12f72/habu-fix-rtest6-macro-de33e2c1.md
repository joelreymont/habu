---
title: Fix rtest6 macro destructuring crash
status: open
priority: 1
issue-type: task
created-at: "2026-03-07T19:32:55.747036+01:00"
blocks:
  - habu-adopt-canonical-test-a8a0cbe4
---

src/compiler/compile.zig:9488-9665 and macro expansion callers around compile.zig:3892. Root cause: transformDestructuredParams still has a crash/stale/null cons path under real Maxima macros. Fix: identify the actual invalid traversal/rooting failure, add a focused regression, and lift rtest6/rtest6b over the current crash floor. Why: this is the first Stage-3 compiler crash blocker in the revised plan.
