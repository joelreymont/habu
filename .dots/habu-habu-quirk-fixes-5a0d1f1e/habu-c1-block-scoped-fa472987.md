---
title: "C1: block-scoped locals"
status: open
priority: 1
issue-type: task
created-at: "2026-06-27T13:15:57.163280+02:00"
---

Bucket C (design). Give locals BLOCK lifetime so {: x :} inside an if/loop branch and after exit both work (today: 'no mid-control locals', 'no bind-after-exit'). Removes the scratch-variable/helper-word workarounds I used everywhere. src/habu locals compiler + checker per-path frame tracking. Biggest single friction source. Preserve fixpoint.
