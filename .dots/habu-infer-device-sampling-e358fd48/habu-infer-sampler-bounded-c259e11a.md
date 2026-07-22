---
title: "Infer sampler: bounded top-k kernel"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.698491+02:00"
blocks:
  - habu-infer-sampler-greedy-9fe6096d
---

Why this exists:
top-k needs a bounded device selection algorithm whose ordering and ties match the host reference.

Required result:
implement the declared supported k range and return the filtered distribution or selected identifier without host vocabulary transfer.

Done when:
exact deterministic fixtures match host for k=1 and supported k values; out-of-range k rejects; seeded stochastic histograms meet the same distribution check.

Expected touch points: new lib/ptx/cg-sampling-topk.f, focused device test.
Smallest check: correctness-only GB10 parity.
Prerequisites: greedy and temperature kernel.
Owned result: top-k kernel only.
Claim: unassigned.
