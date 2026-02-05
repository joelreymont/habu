---
title: Honor declarations/optimize
status: closed
priority: 3
issue-type: task
created-at: "\"2026-02-05T12:16:55.088869+01:00\""
closed-at: "2026-02-05T22:20:40.362534+01:00"
close-reason: Added declaration spec handling, tests, and synced docs to 978/978 implemented
---

docs/cl-symbols.md:1066-1108 mark partial declaration/optimize qualities ignored. Root cause: compiler parses DECLARE/OPTIMIZE but drops effects. Fix: plumb optimize qualities into compiler options + codegen (safety checks, debug info, inlining thresholds), and record declarations in IR for type checker; add tests that (declare (optimize (safety 0))) disables runtime checks.
