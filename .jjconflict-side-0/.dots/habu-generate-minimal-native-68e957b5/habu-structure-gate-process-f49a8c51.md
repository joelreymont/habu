---
title: Structure gate process pool
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:31:16.159976+02:00"
---

test/gate-pool.f:22-67 models each process slot with 46 parallel arrays; :232-252 immediately explodes typed outcome into exited/timed-out/code, :435-456 uses raw -1/0/1 lifecycle, and :809-819 copies failure state into thirteen more parallel arrays. Impossible outcome combinations are representable, GT-POOL-KIND-NAME. defaults every residual combination to signal, and a same-cell index/state typo can reuse a live slot, orphan a child, or misreport exit versus timeout. Define STRUCTURE pool-slot and failure-record rows with typed pid/fd/timing/path/count fields in LAYOUT-BUFFER. Keep process outcome as a payload ENUM rather than three cells, and model lifecycle as payload ENUM variants that carry only resources legal for free/active/done states; bind slot ids to a generation if reused. Snapshot a failure by copying one typed record. Every transition preflights and commits atomically; cleanup consumes exactly the active resources. Preserve scheduling, saturation attribution, capture tails/files, timeout/reaper behavior, output, and budgets. Add checker negatives for field/id/state/outcome swaps; exhaustive lifecycle/outcome transitions, stale generation, every spawn/capture/reap/timeout failure, canary/full-capacity, cleanup order, and failure snapshot round trips. Measure source/JIT/DATA/CODELEN, resident bytes, and gate runtime before/after. Files: test/gate-pool.f and focused tests. Verify pool/stats/runner/cold-hot/full gates, typed-local diff, type/package/host/dot lints. Coordinate habu-pkg-native-gate-6ff8a6aa for namespacing; ownership here is representation/lifecycle.
