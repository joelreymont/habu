---
title: GC regression+perf gates
status: open
priority: 1
issue-type: task
created-at: "2026-02-18T21:50:53.724832+01:00"
blocks:
  - habu-minor-gc-collector-2f89a428
---

bench/check.zig and src/tests/integration.zig. Cause: no enforced performance/correctness gates for generational transition. Fix: add invariants, stress tests, and Maxima throughput thresholds. Why: prevent regressions while iterating.
