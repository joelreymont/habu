---
title: Small model end-to-end on the GPU (fusion -> GEMM -> attention)
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T16:53:45.397131+02:00"
---

GAP #6: we have the kernels (fused elementwise, tiled GEMM, fused attention) and CPU-side maki train/eval, but NOT a single small model lowered through them and run END-TO-END on the device matching CPU - the goal's 'small Maki model train/eval'. FIX: lower a tiny transformer block or MLP (e.g. Linear -> relu -> Linear, or one attention head + MLP) onto the Habu-PTX kernels: GEMM (cg-matmul.f) for the projections, fused elementwise (maki/fusion.f) for bias+activation epilogues, fused attention (cg-attention.f) for the head; run a forward (and ideally one train step) on the Orin and assert it matches the maki CPU reference. This is also the first real workload for AGGRESSIVE fusion (epilogue fusion = the beat-Triton lever) and a true vs-Triton end-to-end comparison. Deps: habu-maki-lower-tensor, habu-automatic-aggressive-fusion, habu-ptx-m11-attention.
