---
title: "PTX M11: causal flash attention + LLM experiment"
status: open
priority: 3
issue-type: task
created-at: "2026-06-25T13:43:16.950005+02:00"
blocks:
  - habu-make-ptx-device-c0eb12a3
  - habu-ptx-m5-mask-eb0716f1
  - habu-fix-ptx-collective-997cfcce
  - habu-tiled-gemm-codegen-76075375
  - habu-ptx-m10-vectorization-f394cfe1
---

File: PLAN.md:390. Gap: `lib/ptx/cg-attention.f` is fixed-shape,
noncausal, and partly unchecked, so it cannot support the GPT capstone or a
generic flash-attention claim. Fix: implement causal online softmax attention
over the checked kernel construction/GEMM/collective blocks, with no score
matrix HBM materialization, explicit size/resource limits, and named rejection
above supported shapes. Verify: Orin forward golden, future-token sentinels,
malformed launch and size-limit rejects, CUDA-event profile rows, and later LLM
matrix evidence only after the kernel is device-correct.
