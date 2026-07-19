---
title: "Close the GB10 GEMM gap: beat Triton on 48 SMs"
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T08:26:43.903714+02:00"
---

North-star successor to the measured head-to-head (docs/eval-triton.md GB10 section): Habu 0.59-0.80x Triton 3.8 TF32 on the GB10; parity is the floor (Joel). The evidence says schedule scaling, not codegen: our wide MMA tile family (built for the 8-SM Orin) under-feeds 48 SMs and plateaus at ~56% of tf32 roof while Triton's SMALL surviving tile reaches ~91%. Levers, in evidence order: (1) mirror Triton's winning config blocking in the Habu schedule space - read its exact autotune winner per shape from the referee script output inlined in the doc (small block, more concurrent blocks); (2) grid-width/persistence: enough blocks to cover 48 SMs at each shape (the 512 measurement, 12.9 vs 21.7, screams occupancy); (3) split-K for the 4096 tail; (4) re-sweep stages/pad on the smaller tiles. Each candidate proves element-exact via mma-gemm-check before timing; measurements extend the doc table and perf-rows.tsv; the goal row is >=1.0x per shape. Separate dot for the missing fp16 m16n8k16 tile family (Triton fp16 runs 73-89 TFLOP/s unopposed).
