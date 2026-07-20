---
title: Block device lowering parity
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T22:41:31.083051+02:00"
blocks:
  - habu-gb10-batched-attention-3055d565
---

The composition never lowers to device (host-only by construction; affine-LN device forward lowered but execution blocked by E-PTXTC-ARCH per its dot). Own block-level host/device parity: LN/MLP/embedding legs plus the attention leg once habu-gb10-batched-attention-3055d565 delivers it. Parity = element-close forward and VJP against the host path on the GB10.
