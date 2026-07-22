---
title: "Infer: small-batch NVFP4 GEMM"
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T16:45:21.596532+02:00"
blocks:
  - habu-infer-gemm-small-5c717119
---

This is the small-batch quantized matrix-matrix campaign record. Do not dispatch it as implementation work. Its leaves own supported geometry, activation conversion, the NVFP4 tensor-core kernel, projection epilogues, and the measured crossover schedule. The campaign closes when every supported row has correctness and benchmark records.
