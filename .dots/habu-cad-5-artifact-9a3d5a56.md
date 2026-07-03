---
title: "CAD 5: artifact store layout + schema"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T01:29:11.719549+02:00"
---

CAD-PLAN section 13 (plan-review round-2 quorum: store was unowned). The single CAD store for kernels (PTX + cubin hash), evidence rows, schedule measurement history, fusion profitability facts, and calibration tables. Content-addressed by (region signature, shape class, dtype, layout, alignment class, target, engine hash, ptxas version). NEW - distinct from the source-digest AOT build cache (tools/hb-build-lib.f, lib/content-key.f). On-disk layout + row schemas + read/write words + eviction-free append v1; habu-kernel-artifact-export externalizes this store. Consumers: cad-5.7 profitability memory, cad-6-tune history, cad-7 regression detection + PROMOTE. Depends: cad-4-schedule (keys).
