---
title: "Infer GEMM: projection epilogue integration"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:42.373548+02:00"
blocks:
  - habu-infer-gemm-nvfp4-619c572a
  - habu-infer-quant-publish-1457f90e
---

Why this exists:
The raw kernel must apply the correct bias, residual, output conversion, and ownership rules at each dense-model projection site.

Required result:
Integrate the small-batch kernel with the declared epilogue for every supported projection and compare complete site outputs with the offline quantized reference.

Done when:
Each site matches its reference; residual and output owners are consumed exactly once; mismatched epilogue, layout, or scale identity rejects before launch.

Expected touch points: projection epilogue integration and focused device tests.
Smallest check: the focused real-site parity test.
Prerequisites: NVFP4 tensor-core kernel and published quantized pack profile.
Owned result: small-batch projection epilogues only.
Claim: unassigned.
