---
title: "Infer quant: quality evaluator"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:40.945058+02:00"
blocks:
  - habu-infer-quant-bounded-1f9c9408
  - habu-infer-quant-calibration-bf8a0fa0
  - habu-infer-dense-full-14833530
---

Why this exists:
The project must derive acceptable quantization error from measured model behavior rather than a default tolerance.

Required result:
Evaluate a candidate quantized pack against the pinned BF16 reference over the immutable evaluation corpus, recording logit error, perplexity delta, fixed continuations, and task results under the benchmark schema.

Done when:
Reference fixtures reproduce; a deliberately damaged scale fails; repeated evaluation is identical; every reported threshold is justified by the recorded BF16 and quantized distributions.

Expected touch points: the offline quality evaluator, canonical result records, and focused tests.
Smallest check: the focused good-pack and damaged-pack evaluation.
Prerequisites: bounded NVFP4 transform, calibration corpus, and full BF16 dense-model parity.
Owned result: offline quality measurement and verdict only.
Claim: unassigned.
