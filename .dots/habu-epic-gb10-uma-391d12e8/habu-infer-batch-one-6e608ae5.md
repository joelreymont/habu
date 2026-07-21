---
title: "Infer: batch-one quantized GEMV"
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T16:45:21.579511+02:00"
blocks:
  - habu-infer-nvfp4-quantized-ea42f1ae
---

Plan-of-record M8 split (2 of 4): batch-one decode behaves like GEMV, not a well-filled GEMM - a weight-only / native low-bit GEMV path with fused scale/dequant, optimized for memory traffic and launch count. Do NOT assume the large-M tensor-core GEMM wins at M=1; measure. Quality gates from the packer dot's contract; kernel-family + perf-watch registration.
