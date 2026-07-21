---
title: "Infer: small-batch NVFP4 GEMM"
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T16:45:21.596532+02:00"
blocks:
  - habu-infer-nvfp4-quantized-ea42f1ae
---

Plan-of-record M8 split (3 of 4): the prefill / larger-small-batch path - native NVFP4 (or the measured winner) tensor-core GEMM with activation quantization where required, fused epilogues, shape-keyed dispatch. Builds on the MMA harness discipline (element-tolerance goldens with DERIVED tolerances). NVFP4 was proven working on this chip during the Triton bring-up.
