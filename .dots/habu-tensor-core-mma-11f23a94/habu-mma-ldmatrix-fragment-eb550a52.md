---
title: MMA ldmatrix fragment loads
status: open
priority: 2
issue-type: task
created-at: "2026-07-05T10:09:39.712003+02:00"
---

cg-mma.f MMM feeds the tensor cores with 4+8 scalar ld.shared.f32 fragment loads/substep (bank conflicts) + 48 cvt.rna.tf32/tile - ptxas shows 38 reg / 0 spill, so it is LOAD/ALU-bound not register-bound (measured 398 GFLOP/s at 2048, under the 442 f32 tile, ~21% of Triton 1890; docs/eval-triton.md step 3). Replace the scalar shared fragment loads with ldmatrix.sync.aligned.m8n8.x4 (packs 4 tf32/.b32, kills the scalar-load bank conflicts and halves the cvt count). Re-run tools/ptx/mma-gemm-check.f (element-exact) + gemm-bench 3-way; expect the biggest single MMA jump.
