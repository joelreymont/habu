---
title: Retire native HIR observer
status: active
priority: 1
issue-type: task
created-at: "\"2026-10-04T05:08:20.586680+03:00\""
---

HBR2 and UI-ADMIT were removed because they had no caller. Their target-neutral
post-freeze NBACK observer has no production caller and was removed from
the compiler, image layout and tests. Publication and invalidation callbacks
at $2CF8 and $2D00 remain in use. Lead review and native/generation
qualification are pending before this task closes.
