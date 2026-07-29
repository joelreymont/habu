---
title: Stabilize two pool-flaky gate fixtures
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:07:39.572131+02:00"
---

Full context: measured repeatedly 2026-07-30 by agents storerows, hookpath, and matchdepth under parallel-lane load 18-29. Two gate-stdlib phases flake under pool load and pass alone every time: check-cli-boundary (child fixture exceeds its 10 s budget, throw -2502 E-PROC-TIMEOUT; timed 10.0-11.2 s against the 10 s budget even on an unmodified tree) and compiler-ir-id (three concurrent-allocator/task-reuse timing cases). Same defect class as habu-budget-the-standalone-92d730f2: a wall-clock budget standing in for a logical property, so slow-under-load and broken are indistinguishable. For check-cli-boundary decide what the fixture proves and give it either a load-independent probe or a measured budget with headroom plus a named timeout-vs-dead verdict; for compiler-ir-id make the three timing cases deterministic (synchronize on the actual state transition, not a sleep). A phase that reds only when the host is saturated blocks every merge gate run on a busy orchestrator machine, which is a real cost paid on every integration.
