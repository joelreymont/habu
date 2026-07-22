---
title: "Infer GEMM: NVFP4 tensor-core kernel"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:42.225466+02:00"
blocks:
  - habu-infer-gemm-activation-480f2152
---

Why this exists:
Larger small batches and prefill need a native low-bit matrix-matrix kernel rather than reusing the batch-one path.

Required result:
Emit and launch the supported NVFP4 tensor-core matrix-matrix kernel over packed weights and activation tiles, producing the declared accumulator layout.

Done when:
Every supported geometry matches the independent reference within measured tolerance; unsupported rows reject before launch; exact emitted PTX assembles and performance registration is complete.

Expected touch points: the PTX emitter, assembly fixture, device test, and performance registry.
Smallest check: the correctness-only DGX Spark parity and assembly test.
Prerequisites: activation quantization.
Owned result: raw small-batch NVFP4 GEMM kernel only.
Claim: unassigned.
