---
title: nanoGPT tiny causal GPT end-to-end on Orin
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T16:53:45.397131+02:00"
blocks:
  - habu-make-ptx-device-c0eb12a3
  - habu-add-ptx-planner-30b93e8c
  - habu-ptx-m5-mask-eb0716f1
  - habu-fix-ptx-collective-997cfcce
  - habu-tiled-gemm-codegen-76075375
  - habu-tensor-core-mma-11f23a94
  - habu-ptx-m11-attention-fa7b0598
  - habu-autograd-end-to-ee4d918b
  - habu-maki-lower-tensor-e6bbca3d
  - habu-integrate-adamw-in-3687a100
  - habu-add-maki-orin-7b88fb4b
---

File: PLAN.md:511. Root cause: the project has CPU-side Maki pieces and PTX
kernel seeds, but not the reviewed capstone: a tiny causal GPT assembled from
generic Maki/PTX DSL blocks and run end-to-end on the Orin. Fix: add the thin
`maki/gpt*.f` model DSL for token embedding, position embedding, transformer
block, causal attention, MLP, logits, logits-domain CE, and AdamW update while
keeping optimizer and device-lowering APIs in generic Maki/PTX layers. Verify:
forward CPU/device parity first, then one train step with gradient and AdamW
parity, short deterministic loss decrease, token-id edge tests, and a profile
matrix explaining fusion, tiling/MMA, scalar fallbacks, bytes, FLOPs, and roofs.
