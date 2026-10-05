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
at $2CF8 and $2D00 remain in use.

The native build retires the former host's $2CF0 CODE declaration through
ADDRESS-CELLS before checking fixed registrations; DATA at that offset and
other unknown fixed rows still refuse. The registered native-build entry
exercises those host rows through the real build path. The registered native
dictionary publication suite retains the fatal post-commit callback case.
Native and generation checks and focused reviewer follow-up remain pending.
