---
title: Tiled GEMM codegen (compute-bound; shared-mem tiling + accumulators)
status: open
priority: 3
issue-type: task
created-at: "2026-06-27T15:43:15.241989+02:00"
blocks:
  - habu-ptx-ir-opt-b90390f0
---

GAP: ONNX Gemm currently fakes to SAXPY (affine y=a*x+b), not a real matmul; there is no tiled GEMM. Build checked tiled matmul codegen: block the MxNxK iteration into register/shared-mem tiles, stage A/B tiles to .shared with bar.sync, accumulate in registers, write the epilogue. Tile sizes from the M9 autotuner. This is the compute-bound kernel where CODEGEN quality (FMA throughput, tiling, bank-conflict-free staging) wins over DRAM - the other way to beat Triton. Foundation for habu-ptx-m11-attention (flash-attention = tiled matmul + fused softmax). Needs the IR layer + M7 2D tiling. VERIFY: tiled GEMM device-correct vs CPU; GFLOP/s climbs with tile size; certifies as checked Habu.
