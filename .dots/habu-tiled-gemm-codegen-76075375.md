---
title: Tiled GEMM codegen (compute-bound; shared-mem tiling + accumulators)
status: open
priority: 3
issue-type: task
created-at: "2026-06-27T15:43:15.241989+02:00"
---

GAP: ONNX Gemm currently fakes to SAXPY (affine y=a*x+b), not a real matmul; there is no tiled GEMM. Build checked tiled matmul codegen: block the MxNxK iteration into register/shared-mem tiles, stage A/B tiles to .shared with bar.sync, accumulate in registers, write the epilogue. Tile sizes from the M9 autotuner. This is the compute-bound kernel where CODEGEN quality (FMA throughput, tiling, bank-conflict-free staging) wins over DRAM - the other way to beat Triton. Foundation for habu-ptx-m11-attention (flash-attention = tiled matmul + fused softmax). Needs the IR layer + M7 2D tiling. VERIFY: tiled GEMM device-correct vs CPU; GFLOP/s climbs with tile size; certifies as checked Habu.

## Audit refresh (2026-07-06, head 1eb3b5d3)

"There is no tiled GEMM" is stale: lib/ptx/cg-matmul.f is a register-blocked
tiled SGEMM whose MM-CHECKED surface certifies (lib/ptx/gemm-checked-test.f rc 0,
gemm-checked-neg-test.f rejects), device-proven via tools/ptx/matmul-device-test.f
(~283 GFLOP/s recorded). Remaining scope: wire ONNX Gemm to it (maki/onnx.f still
maps Gemm to the SAXPY affine), autotuned tile sizes (M9), and re-expressing the
body in the typed tile DSL is tracked separately in habu-re-express-tiled-9cc4a73a.
