---
title: Add pause-budget telemetry
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-20T08:55:19.442465+01:00\""
closed-at: "2026-02-20T09:33:18.277906+01:00"
close-reason: completed
blocks:
  - habu-define-gc-parity-c2bf61b3
---

File: src/runtime/gc.zig:1, src/runtime/heap.zig:1, bench/gc.zig:1; cause: collector lacks fine-grained budget visibility; fix: expose young/major phase timers and pause histograms; why: tuning without telemetry is blind.
