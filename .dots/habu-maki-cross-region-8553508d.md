---
title: "Maki: cross-region + chained movement source (movement OPTIMIZE wiring)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T21:50:31.323925+02:00"
---

SLICE 4 v1 restricts both the dissolved fold and the copy kernel to movement whose SOURCE is a model INPUT SLOT: maki/move-view.f MVW-SRC-REF throws E-MVW-SRC and maki/lower-move.f LMV-REF-ROWS throws E-LMV-INPUT when a movement operand is an interior/other-region node. So a movement fed BY a compute region (e.g. GELU then CONCAT: concat's A operand is the gelu node) or a CHAINED movement (SLICE then TRANSPOSE) cannot lower yet. This is the same cross-region device-buffer handoff the slice-5 OPTIMIZE wiring removes for E-LLA-INPUT (maki/lower-launch.f). Fix with slice 5: once cross-region buffers exist, MVW-RESOLVE-SRC/LLA-STAGE-IN can point a folded/copy input at a materialized producer's device buffer, and a chained movement resolves through the chain. Repro: MODEL: GC ( x:4x8 b:4x8 -- y ) GELU CONCAT ; FP-BUILD 1 LMV-ANALYZE -> E-LMV-INPUT (maki/lower-mv-test.f).
