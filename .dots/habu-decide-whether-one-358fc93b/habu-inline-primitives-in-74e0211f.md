---
title: Inline primitives in tier 0
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T18:58:19.000537+03:00"
---

Experiment, proposed to Joel 2026-10-23 and not yet approved: tier 0 copies a primitive's body from the primitive table instead of calling it, then (separate steps) keeps stack values in registers and ?do counters in registers; measure each step on the kernels, the real workloads and tools/compile-floor.f. If tier 0 reaches tier 1's run time at about tier 0's compile cost, tier 1 can go. Depends: the census and timing children. Claim: unassigned.
