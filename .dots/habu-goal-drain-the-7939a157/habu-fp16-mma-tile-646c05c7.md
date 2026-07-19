---
title: fp16 MMA tile family (m16n8k16) for the GB10
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T08:26:43.909071+02:00"
---

The GB10 head-to-head exposed that Habu has NO fp16/bf16 tensor-core path - the MMA tile is tf32-only (m16n8k8 f32.tf32.tf32.f32), so Triton's fp16 column (27-89 TFLOP/s) ran unopposed. Implement the m16n8k16 f16.f16.f32 (and bf16 variant) tile family in lib/ptx/cg-mma.f following the tf32 tile's structure: ldmatrix feeds, fragment layout, accumulator f32, element-exact check first (extend tools/ptx/mma-gemm-check.f with fp16 references - mind reference tolerance vs f32 accumulate), then schedule rows + bench column. Also the nanoGPT device leg wants this (training precision policy is an open E2 decision - fp16 tile existence unblocks measuring it).
