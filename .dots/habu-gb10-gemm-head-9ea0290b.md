---
title: "GB10 GEMM head-to-head: Habu vs Triton 3.8"
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T16:40:11.503110+02:00"
---

North-star milestone (Joel: parity is the floor — Orin already measured 1.60x Triton on the tf32 MMA tile, docs/eval-triton.md via V2 s22.10). After habu-parameterize-ptx-toolchain-0b166646: re-measure the flagship wide MMA tile (MMA-MFRAGS=4 B-ldmatrix, dynamic-smem/wide-grid launch — the codegen-verdict deferral) on sm_121a; enumerate+tune the GB10 schedule rows; then head-to-head vs the source-built Triton 3.8.0 in ~/Work/ml/.venv on identical shapes (512..4096 sq, fp16/bf16/tf32), autotuned both sides, CUDA-event timing, cold+warm. Triton-baseline E2 decision RESOLVED: referee = our source 3.8 build on spark. Context: Triton is documented-hobbled on consumer Blackwell (#8182 smem geometry, inductor is_big_gpu 68-SM gate, fork/cuInit) — record its ceiling honestly (best manual config if autotune refuses). Deliverable: measured table in docs/eval-triton.md GB10 section; the notoriety number.
