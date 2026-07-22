---
title: "Infer GEMV: real-model integration"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:41.654421+02:00"
blocks:
  - habu-infer-gemv-fused-18482bb4
  - habu-infer-quant-publish-1457f90e
---

Why this exists:
A correct isolated kernel is not proof that every batch-one projection uses the right packed member, scales, epilogue, and output owner.

Required result:
Bind the fused kernel to each supported dense-model projection site and compare one complete quantized decode step with the offline quality reference.

Done when:
Every site selects the expected pack member and kernel key; logits remain within the recorded quality limit; a mismatched layout or scale identity rejects before launch.

Expected touch points: dense-model quantized projection integration and focused device tests.
Smallest check: one complete quantized decode-step parity test.
Prerequisites: fused NVFP4 GEMV kernel and published quantized pack profile.
Owned result: batch-one projection-site integration only.
Claim: unassigned.
