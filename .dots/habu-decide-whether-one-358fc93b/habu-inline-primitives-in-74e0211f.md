---
title: Inline primitives in tier 0
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T18:58:19.000537+03:00"
---

Experiment, approved by Joel 2026-10-25 (rulings in habu-decide-whether-one-358fc93b): tier 0 copies a primitive's body from the primitive table instead of calling it, then (separate steps) keeps stack values in registers and ?do counters in registers; measure each step on the kernels, the real workloads and tools/compile-floor.f. If tier 0 runs real workloads within 5% of tier 1 at about tier 0's compile cost, tier 1 can go. Depends: the census and timing children. Claim: jerry.

Step 1 lane running in .jj-ws.noindex/jerry-inline (jerry), brief /Users/joel/.cache/tmp/jerry-lead.K6tX/inline-brief-live.md.
