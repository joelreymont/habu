---
title: "CAD: launch amortization (persistent/graph loop)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T19:28:43.760587+02:00"
---

CAD-PLAN 8.1 lever 5. Jetson-class per-launch overhead is large relative to small-batch region work; after fusion the remaining launch count still costs. Options to evaluate ON MEASUREMENT (PROFILE launch-bound classification first, per section 9 advice table): persistent kernels consuming a region queue, or a graph-style driver loop batching cuLaunch sequences (CUDA Graphs equivalent through our own driver bindings). Fail-closed: only where GOLDEN still proves the batched execution. Depends: PROFILE roofline on-device (classify launch-bound regions first - measure, do not assume). Blocks: end-to-end latency parity.
