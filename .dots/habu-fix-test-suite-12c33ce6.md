---
title: Fix test suite hang
status: active
priority: 2
issue-type: task
created-at: "\"2026-01-18T14:36:06.527010+02:00\""
---

zig build test hangs indefinitely. Need to identify which test causes hang, likely infinite loop or deadlock. src/tests/, bisect to find hanging test, fix root cause.
