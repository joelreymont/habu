---
title: Tensor-core MMA codegen (the compute-roof beat-Triton lever)
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T17:00:28.959471+02:00"
blocks:
  - habu-tiled-gemm-codegen-76075375
---

ROOFLINE finding (docs/kernel-principles.md): our SGEMM uses fma.rn.f32 -> capped at the FP32-CUDA-core roof (~940 GFLOP/s on Orin NX); we measured 283 = 30% of it (tiling headroom). But Triton's matmul measured 1474 GFLOP/s, ABOVE the 940 FP32 peak - it uses TF32 TENSOR CORES (a different, higher compute roof). So tiling alone tops out near 940; to MATCH/BEAT Triton on compute kernels we must emit tensor-core MMA. FIX: add mma.sync.aligned (TF32 m16n8k8 / FP16 m16n8k16) codegen: load A/B fragments from shared with the prescribed lane->fragment layout (ldmatrix or manual), accumulate FP32, store the C fragment. Constraints (from the course): fixed operand/accumulator format pairs; K aligned to MMA-K; fragment/scale layout is the top cause of 'correct in NumPy, garbage on device' - test fragment layout in isolation first. Reuse the register-blocked tile structure (cg-matmul.f) but swap the 4x4 fma micro-tile for an MMA tile. VERIFY: device-correct vs FP32 ref within TF32 tolerance; GFLOP/s climbs above the 940 CUDA-core roof toward Triton's 1474. Deps: habu-tiled-gemm-codegen (the tiling) + relates to habu-checker-capability-typed (typing MMA fragments).
