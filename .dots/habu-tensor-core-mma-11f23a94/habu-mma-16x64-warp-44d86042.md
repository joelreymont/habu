---
title: MMA 16x64 warp tile (8x A-reuse)
status: open
priority: 2
issue-type: task
created-at: "2026-07-05T10:09:39.720838+02:00"
---

cg-mma.f MMM uses 8 warps at 16x32/warp = 4 MMA n-tiles, A fragment reused only 4x. A 16x64 warp tile (8 n-tiles, A reused 8x) needs a 4-warp / 128-thread cooperative cp.async staging (MM-CP-STAGE is 256-thread; add a 128-thread variant). This is the doc's higher-reuse rung. Keep element-exact (mma-gemm-check) + tf32 LOWER-GOLDEN license; measure via gemm-bench.
