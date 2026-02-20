---
title: "Safepoint: batch polling strategy"
status: open
priority: 1
issue-type: task
created-at: "2026-02-20T08:58:56.419512+01:00"
blocks:
  - habu-barrier-inline-hot-4222c4ad
---

src/interp/vm.zig, src/jit: batch safepoint polling with bounded latency guarantees; remeasure overhead.
