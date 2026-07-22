---
title: "Infer sampler: bounded top-p kernel"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.704174+02:00"
blocks:
  - habu-infer-sampler-bounded-c259e11a
---

Why this exists:
nucleus sampling requires explicitly bounded semantics on device; an approximation cannot be implicit.

Required result:
implement exact host semantics for the supported vocabulary/range or a separately named bounded approximation with a documented cutoff.

Done when:
p=1 and supported p fixtures match host semantics, invalid p rejects before reads, and seeded distribution tests pass.

Expected touch points: new lib/ptx/cg-sampling-topp.f, focused device test.
Smallest check: correctness-only GB10 parity.
Prerequisites: bounded top-k kernel.
Owned result: top-p device kernel only.
Claim: unassigned.
