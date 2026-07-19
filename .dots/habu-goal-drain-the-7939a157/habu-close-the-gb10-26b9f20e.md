---
title: "Close the GB10 GEMM gap: beat Triton on 48 SMs"
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T08:26:43.903714+02:00"
---

North-star successor to the measured head-to-head (docs/eval-triton.md GB10 section): Habu 0.59-0.80x Triton 3.8 TF32 on the GB10; parity is the floor (Joel). The evidence says schedule scaling, not codegen: our wide MMA tile family (built for the 8-SM Orin) under-feeds 48 SMs and plateaus at ~56% of tf32 roof while Triton's SMALL surviving tile reaches ~91%. Levers, in evidence order: (1) mirror Triton's winning config blocking in the Habu schedule space - read its exact autotune winner per shape from the referee script output inlined in the doc (small block, more concurrent blocks); (2) grid-width/persistence: enough blocks to cover 48 SMs at each shape (the 512 measurement, 12.9 vs 21.7, screams occupancy); (3) split-K for the 4096 tail; (4) re-sweep stages/pad on the smaller tiles. Each candidate proves element-exact via mma-gemm-check before timing; measurements extend the doc table and perf-rows.tsv; the goal row is >=1.0x per shape. Separate dot for the missing fp16 m16n8k16 tile family (Triton fp16 runs 73-89 TFLOP/s unopposed).

ROUND 1 LANDED 2026-07-19 (d675b4c1): 1024^3 improved 0.69x -> 0.75x (25.2 TF, MFRAGS=2 128x64 single-buffer B-ldmatrix - the omitted config); 512/2048/4096 unmoved (0.59/0.80/0.62). OCCUPANCY HYPOTHESIS REFUTED by measurement: Triton's 512^3 winner launches the IDENTICAL 32 blocks yet wins 21.7 vs 13.0 - the gap is per-block tensor efficiency. Structural blocker identified and recorded in the doc: cg-mma.f is hardwired 8-warp (4x2 grid, BN fixed 64) with max 2 pipeline stages; Triton's winners are 4-warp with 3-5 stages; under the 99KB smem cap our wide tiles cannot deep-pipeline without 1 block/SM. The two levers are coupled: the 4-warp tile is the prerequisite for deeper stages. Successor: habu-mma-warp-shape dot. This dot stays open as the campaign header until >=1.0x per shape or a final why-we-lose verdict.
