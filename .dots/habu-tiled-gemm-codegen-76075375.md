---
title: Tiled GEMM codegen (compute-bound; shared-mem tiling + accumulators)
status: open
priority: 3
issue-type: task
created-at: "2026-06-27T15:43:15.241989+02:00"
blocks:
  - habu-make-ptx-device-c0eb12a3
  - habu-add-ptx-planner-30b93e8c
  - habu-checker-capability-typed-e0c76a02
  - habu-habu-native-kernel-548b0d4c
  - habu-ptx-ir-opt-b90390f0
---

File: PLAN.md:356. Gap: `lib/ptx/cg-matmul.f` has fixed 64x64x16
assumptions and raw emit islands, while ONNX/Maki cannot route real MatMul/Gemm
through a generic checked planner. Fix: re-express tiled GEMM under checked
loop/shared-memory/accumulator words, support square, tail, tall-skinny, decode
GEMV, QKV/projection/logits/MLP capstone shapes, and emit profile rows from the
generic benchmark API. Verify: Orin device tests cover M/N/K tails and capstone
shapes, GFLOP/s rows climb with selected tile plans, and raw islands
`MM-STAGE`, `MM-KSTEP`, `MM-WRITE`, and `EMIT-MATMUL` are retired or audited at
function level.
