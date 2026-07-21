---
title: Harden EMIT-REDUCE SMEM WAR hazard
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T07:41:44.433659+02:00"
---

Loose end from the device-LN landing (stack cb1e4cae): the shared EMIT-REDUCE primitive reuses its SMEM staging across sequential block reductions without a bar.sync between one reduce's broadcast-load and the next's stage-write - a WAR hazard that is empirically safe on current hardware (LN's 4-reduce backward, softmax, rmsnorm, LRED-LN all gradcheck) but is not architecturally guaranteed. Add the fence (or prove the guarantee from the PTX memory model with a citation), re-measure the certified reduction kernels' perf rows, and re-run the device gradchecks. Touches the certified reduction infra - one focused change with the perf-watch rows re-owned.
