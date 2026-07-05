---
title: MMA larger BK + swizzled bank-free shared
status: open
priority: 3
issue-type: task
created-at: "2026-07-05T10:09:39.727995+02:00"
---

After ldmatrix + bigger warp tile: raise BK past 32 (fewer bar.sync, more compute/sync) and add a swizzled/padded As/Bs shared layout so the fragment loads are bank-conflict-free. Feed the cad-6 shape-keyed autotuner the (BK, warp-tile, stages, swizzle) axes. Keep mma-gemm-check element-exact + tf32 golden green; the target is climbing above the 940 FP32 CUDA-core roof toward Triton.
