---
title: "Safepoint: batch polling strategy"
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-20T08:58:56.419512+01:00\\\"\""
closed-at: "2026-02-20T15:39:27.094435+01:00"
close-reason: Batch safepoint polling with bounded op/byte budgets
blocks:
  - habu-barrier-inline-hot-4222c4ad
---

src/interp/vm.zig, src/jit: batch safepoint polling with bounded latency guarantees; remeasure overhead.
