---
title: "Infer dense: host reference block"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.446621+02:00"
blocks:
  - habu-infer-dense-tensor-c037a6fd
---

Why this exists:
a modern architecture oracle is needed before device integration.

Required result:
compose one host block with RMSNorm, RoPE, grouped-query attention, SwiGLU, projections, and residuals at the pinned geometry.

Done when:
selected internal checkpoints match the published trusted reference under declared BF16/FP32 tolerance; wrong workspace or tensor binding rejects.

Expected touch points: new maki/infer/dense-block.f, focused test and fixture.
Smallest check: focused block parity test.
Prerequisites: tensor and config binding, landed SwiGLU.
Owned result: one host modern block only.
Claim: unassigned.
