---
title: "Infer GEMV: fused NVFP4 kernel"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:41.510113+02:00"
blocks:
  - habu-infer-gemv-nvfp4-852b89a0
---

Why this exists:
Batch-one decode is dominated by weight traffic and needs a kernel that reads packed weights once while applying scales during accumulation.

Required result:
Emit and launch the supported batch-one NVFP4 matrix-vector kernel with scale application fused into the reduction and no intermediate full-precision weight buffer.

Done when:
Every supported projection shape matches the reference oracle within the measured recipe tolerance; unsupported inputs reject before launch; assembly and performance registration are complete.

Expected touch points: the PTX emitter, device test, and performance registry.
Smallest check: the correctness-only DGX Spark parity test.
Prerequisites: NVFP4 reference oracle.
Owned result: batch-one NVFP4 kernel only.
Claim: unassigned.
