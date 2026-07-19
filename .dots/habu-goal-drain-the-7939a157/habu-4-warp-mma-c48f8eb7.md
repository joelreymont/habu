---
title: 4-warp MMA tile family with deeper pipeline stages
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-19T08:55:48.432992+02:00\""
closed-at: "2026-07-19T10:04:42.102904+02:00"
---

The structural lever the GB10 gap campaign identified (docs/eval-triton.md GB10 section, round-1 record): Habu's tf32 tile is hardwired 8 warps (4x2 warp grid, BN fixed 64, fragment->lane map / accumulator layout / store map all assume it) and maxes at 2 smem pipeline stages; Triton's per-shape winners run 4 warps with 3-5 stages, and under the GB10's 99KB smem-per-block cap that is the only geometry that both deep-pipelines AND keeps multiple blocks per SM. Implement a 4-warp tile variant in lib/ptx/cg-mma.f as a sibling family (2x2 warp grid to start, mirroring Triton's BM128xBN64 winner blocking), which requires reworking the fragment->lane mapping, accumulator layout, and store map for the narrower warp grid - a kernel-engineering change, not a knob. Then raise the stage count on the smaller smem footprint (3+, sweep). Element-exact via mma-gemm-check.f rows FIRST for every variant; then the doc's exact timing protocol; extend the eval-triton GB10 tables + perf-rows.tsv. Goal: >=1.0x vs Triton per shape, campaign header habu-close-the-gb10-26b9f20e.
