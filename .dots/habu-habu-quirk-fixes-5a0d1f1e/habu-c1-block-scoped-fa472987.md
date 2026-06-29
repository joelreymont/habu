---
title: "C1: block-scoped locals"
status: closed
priority: 1
issue-type: task
created-at: "2026-06-27T13:15:57.163280+02:00"
closed-at: "2026-06-29T04:41:36.114498+02:00"
close-reason: "Implemented checker and native compiler block-scoped locals: frame-entry local snapshots, branch/loop/case scope restoration, exit/leave teardown, quotation-only raw guard, engine/diagnostic regressions, fixpoint rebuild, full native gate 69842ms <= 90000ms."
---

Bucket C (design). Give locals BLOCK lifetime so {: x :} inside an if/loop branch and after exit both work (today: 'no mid-control locals', 'no bind-after-exit'). Removes the scratch-variable/helper-word workarounds I used everywhere. src/habu locals compiler + checker per-path frame tracking. Biggest single friction source. Preserve fixpoint.
