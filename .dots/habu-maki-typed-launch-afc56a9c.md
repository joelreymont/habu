---
title: "maki: typed launch-param helpers (LMV/LLA rows-cols uniformity)"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-12T19:35:57.903364+02:00\""
---

Audit review OBSERVATION-3: lower-move.f:134 LMV-DIMS-TRANSPOSE projects rows->LMV-PA / cols->LMV-PB into raw u32 launch slots - same-file codegen boundary (not a violation), but a swap would compile silently; same shape at lower-launch.f:297 LLA-GRID-RED (grid=rows nvar=cols, guarded only by PTX-ROW-LAUNCH-CHECK at runtime). Extend the MV-PACK-SHAPE pattern: typed LMV-SET-RC-PARAMS ( rows cols -- ) and a typed grid-launch wrapper so the role order is checker-enforced at these last two mixed rows/cols raw sites. Small, mechanical; after the region-kind migration lands (same files).
