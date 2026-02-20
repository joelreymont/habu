---
title: Maxima load to green
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-17T22:23:04.178094+01:00\\\"\""
closed-at: "2026-02-20T20:50:53.609550+01:00"
close-reason: Fix generational load package root corruption; add regression
blocks:
  - habu-stabilize-eval-vm-d1c1c5cc
---

lib/maxima-loader.lisp and src/tests/integration.zig. Cause: loader reaches end but critical symbols remain unbound. Fix: enforce critical symbol bind checks and fix upstream-triggered CL gaps in Habu.
