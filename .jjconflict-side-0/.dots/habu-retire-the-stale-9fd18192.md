---
title: Retire the stale outer-context scaffolding
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T18:21:25.680944+02:00"
---

test/compiler/ir-context.f's outer-context scaffolding and its explanatory comment describe the pre-repair leak (cases had to run inside one context because top-level throws leaked) — now belt-and-braces, and the comment is false. Remove the scaffolding where the new top-level cases cover the path, and correct the comment. Found by the CG-08 lane 2026-08-05.
