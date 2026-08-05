---
title: Pair and alternate the workload timing
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.657118+02:00"
---

CG-29. tools/codegen-workload-time.f runs old always before new (317-327), keeps independent minima per arm (190-196), and derives the noise bar from the maximum of only two or four null rows (482-507); five unchanged-tree runs classified check-batch REAL LOSS three times and NOT MEASURABLE twice, and publication latency is excluded entirely. Fix: alternate execution order, analyze paired deltas with a measured interval/quantile, and measure compilation/publication latency and generated-code runtime as separate axes. Until calibrated, print raw data and remove the REAL verdict labels.
