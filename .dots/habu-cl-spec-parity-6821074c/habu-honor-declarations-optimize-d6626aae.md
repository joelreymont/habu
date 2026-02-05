---
title: Honor declarations/optimize
status: open
priority: 3
issue-type: task
created-at: "2026-02-05T12:16:55.088869+01:00"
---

docs/cl-symbols.md:1066-1108 mark partial declaration/optimize qualities ignored. Root cause: compiler parses DECLARE/OPTIMIZE but drops effects. Fix: plumb optimize qualities into compiler options + codegen (safety checks, debug info, inlining thresholds), and record declarations in IR for type checker; add tests that (declare (optimize (safety 0))) disables runtime checks.
