---
title: Skip loop-body locals in the back-edge carrier
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T20:23:22.332612+02:00"
---

Found by the locals-scope landing, deliberately NOT done in-lane (untested optimisation): LS-POP does not skip locals declared inside a loop body, but a loop-body local is re-bound each turn and cannot be live across the back edge - carrying it is a pessimisation only, safe but wasteful. Narrow the back-edge carrier to exclude them, with a differential proving the loop still answers and a pressure fixture showing the saving. Files: src/compiler/native/elaborate.f. Depends: the locals-scope landing (2faa3d7a).
