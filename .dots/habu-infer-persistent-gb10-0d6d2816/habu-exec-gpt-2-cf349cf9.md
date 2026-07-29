---
title: Execute GPT-2 MLP stage
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:55:50.651340+02:00"
blocks:
  - habu-upload-gpt-2-044b5364
  - habu-own-persistent-inference-ecc98bdf
---

Why: second normalization, MLP projections, GELU, and final residual form one independently testable stage. Interface: package-private GPT2DEV:MLP takes session/weights/layer/activation owners, launches exact second LayerNorm, expansion, GELU, projection, and residual, then returns launch state and owners. Owner: GPT-2 MLP stage only. Production red: no real device MLP consumes uploaded weights. Acceptance: GPT2-REFERENCE first/last-layer norm/GELU/MLP/residual probes, wrong roles/extents, and enqueue failures preserve owners. Forbidden: QKV, attention, full block, allocation, host fallback, or alternate GELU. Smallest owning check: focused GPT-2 MLP-stage test on DGX Spark.
