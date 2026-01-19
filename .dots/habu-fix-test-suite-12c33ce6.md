---
title: Fix test suite hang
status: closed
priority: 2
issue-type: task
created-at: "\"2026-01-18T14:36:06.527010+02:00\""
closed-at: "\"2026-01-19T06:45:00+02:00\""
---

zig build test hangs indefinitely. Need to identify which test causes hang, likely infinite loop or deadlock. src/tests/, bisect to find hanging test, fix root cause.

---

Tests no longer hang. 2 failures: flet shadowing, stdlib case macro needs mapcar.
