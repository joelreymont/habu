---
title: "Infer quant dispatch: performance decision"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:43.067954+02:00"
blocks:
  - habu-infer-quant-dispatch-4d8306d2
  - habu-infer-m0-pinned-17b6e648
---

Why this exists:
The release needs one honest go or no-go decision against the best reproducible baseline and the BF16 engine.

Required result:
Measure the quantized engine over the M0 matrix and apply the plan's declared rule: a material performance gain, provisionally 20 percent, or a materially larger safe context or concurrency envelope at comparable latency.

Done when:
All canonical records and raw-log digests validate; the decision cites only measured rows; failures and regressions remain visible; no fourfold-throughput claim is inferred from byte size.

Expected touch points: canonical benchmark records, schedule policy, and release decision note.
Smallest check: benchmark reducer replay and decision-rule fixture.
Prerequisites: end-to-end quantized parity and pinned baseline results.
Owned result: quantized release performance verdict only.
Claim: unassigned.
