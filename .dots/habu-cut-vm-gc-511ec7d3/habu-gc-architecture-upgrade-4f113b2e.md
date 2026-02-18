---
title: GC architecture upgrade
status: open
priority: 1
issue-type: task
created-at: "2026-02-18T21:50:22.213398+01:00"
---

src/runtime/heap.zig:223 and src/runtime/gc.zig:1. Cause: semispace full-copy design and collection-path overhead will cap Maxima-scale throughput/memory. Fix: phased generational+LOS architecture with hard perf gates and no workload-specific behavior. Why: generic CL workloads need lower pause/copy cost.
