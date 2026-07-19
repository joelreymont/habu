---
title: Grouped-raster CTA ordering + tf32 4096 close
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T16:37:49.187793+02:00"
---

The tf32 4096^3 close (campaign header habu-close-the-gb10-26b9f20e; 29.6 TF vs Triton 45.3 = 0.65x post round 8). BN=256 cannot pipeline past stages=2 (BROWS*BN staging + 3-stage BK=32 buffers bust the 99KB cap), so post-reorder residue is expected. MECHANISM TO IMPLEMENT: grouped-raster CTA ordering - Triton GROUP_M-style launch swizzle so concurrently resident blocks share A-row/B-col tiles in L2; we launch naive row-major and have no equivalent. Emit-time knob MMA-GROUP (0 = off, byte-identical; else group height in M-blocks): prologue index arithmetic only remaps (ctaid.x, ctaid.y) -> (tile_m, tile_n) - group = linear / (GROUP*gridN); within group column-major - no smem/register/schedule cost. Element-exact via mma-gemm-check at a NON-SQUARE grid (gridM != gridN, the remap-bug catcher) + byte-identity at MMA-GROUP=0. MEASUREMENT DECIDES THE 4096 WINNER (Round 10, doc protocol, pinned 13.3, solo): swizzle on/off crossed with (a) BN=256 st2 (current winner), (b) BN=128 MFRAGS=2 deeper stages 3-4 (fits smem once reorder makes depth pay), (c) BK=16 staging to shrink per-stage footprint (BK is already a knob - config rows, no new mechanism). Record L2 hit deltas if nsight is available, else wall-clock only. SERIALIZE behind habu-reorder-cp-async-6d9e6538 (same file cg-mma.f); dispatch immediately when that lane merges - Joel: fix this too.
