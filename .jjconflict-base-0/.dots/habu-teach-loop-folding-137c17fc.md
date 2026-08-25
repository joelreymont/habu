---
title: Teach loop folding the per-function base
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T00:11:59.355990+02:00"
---

Found by the succ-ord landing (95001f3a): loop.f declines ANY multi-function module (FOLDS answers 0, REWRITE throws E-NLOOP-PLAN on FUN-COUNT <> 1, source note states it) - so every definition holding a quotation loses loop folding, INCLUDING the ones that landing just unblocked. Fail-closed and pre-existing, cost newly visible. Work: give loop.f the same per-function base the other passes carry (its BLOCK-ORD/SUCC-ORD/NEW-ORD renumbering table is the work) and drop the FUN-COUNT declines, with a differential proving a quotation-holding definition's loop still folds. Files: src/compiler/native/loop.f. Depends: none.
