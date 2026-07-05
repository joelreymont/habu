---
title: MMA ldmatrix fragment loads
status: closed
priority: 2
issue-type: task
created-at: "2026-07-05T10:09:39.712003+02:00"
closed-at: "2026-07-05T11:10:00+02:00"
close-reason: "measured NEGATIVE - fragment-feed hypothesis falsified: 3-mode ablation (cg-mma.f MMA-LMODE: 0 scalar+cvt / 1 scalar raw no-cvt / 2 ldmatrix.x4 A + raw B no-cvt) all element-exact (mma-probe MP-LDM-ALL 0/128; mma-gemm-check 64^3+128^3 all modes; tf32 golden green); cvt-drop FLAT, ldmatrix ~1.2% SLOWER (370.0/388.9/394.3 vs 376.1/393.5/398.5 GFLOP/s at 512/1024/2048; ptxas 43 vs 38 reg, 0 spill). Rung is issue/dependency-bound not fragment-feed-bound; default stays mode 0 (exact-RNE); ldmatrix mechanism committed+selectable for the 16x64-warp / swizzled-Bs rung. Record: docs/eval-triton.md step 3c"
---

cg-mma.f MMM feeds the tensor cores with 4+8 scalar ld.shared.f32 fragment loads/substep (bank conflicts) + 48 cvt.rna.tf32/tile - ptxas shows 38 reg / 0 spill, so it is LOAD/ALU-bound not register-bound (measured 398 GFLOP/s at 2048, under the 442 f32 tile, ~21% of Triton 1890; docs/eval-triton.md step 3). Replace the scalar shared fragment loads with ldmatrix.sync.aligned.m8n8.x4 (packs 4 tf32/.b32, kills the scalar-load bank conflicts and halves the cvt count). Re-run tools/ptx/mma-gemm-check.f (element-exact) + gemm-bench 3-way; expect the biggest single MMA jump.

OUTCOME 2026-07-05: built + proven + measured; the expected jump did NOT land.
The ablation isolates the variable: mode 1 (drop all 48 cvt) is flat, mode 2
(ONE ldmatrix.x4 replacing the 4 scalar A loads, tf32 = 2 adjacent b16 halves
so the 16x8 A fragment = 4 congruous 8x8 b16 tiles) is ~1.2% slower with MORE
registers. Conclusion: at 16x32 warp tile / 4x A-reuse the MMA pipeline is not
fragment-feed-bound; the limiter is the per-warp mma dependency chain + BK=32
bar.sync cadence. Redirects to habu-mma-16x64-warp (8x A-reuse) and
habu-mma-larger-bk (swizzled Bs -> B-side ldmatrix where the mechanism
amortizes). Full record: docs/eval-triton.md "Step 3c".
