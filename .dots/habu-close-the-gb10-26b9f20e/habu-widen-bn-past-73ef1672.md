---
title: "Widen BN past 64: the 4096-class tile"
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T13:57:47.858639+02:00"
---

Round-8 structural lever, from the orchestrator's 13.3-assembler research (2026-07-19): with scheduling equalized, Triton's 4096^3 win (45.3 vs our 27.8 TF) comes from tile GEOMETRY - its winner is BM128xBN256 (st3, 8 warps, 128 HMMAs per K-tile body) while cg-mma.f hardwires BN=64 (BN fixed at MMA-BN; warp_col selects one of two 32-col halves). A BN=128/256 tile family means: B fragments per n-tile row grow 2-4x (B-reuse per load grows - the whole point), accumulator count per lane grows proportionally (watch the 255-reg ceiling; Triton runs ~158 with 0 spills at BN256), the fragment->lane and store maps generalize over BN, and the smem epilogue staging tile grows (BROWS*BN*4 vs the 99KB cap - may need half-tile staging, the doc's recorded next-epilogue idea). Element-exact first for every new BN on both warp grids, byte-identity for BN=64, fail-closed on cap-busting combos, then the doc timing protocol UNDER THE PINNED 13.3 ptxas only (13.0 numbers are no longer decision-grade). Expect the 4096 and 2048 columns to move; 512 stays occupancy-gated.
