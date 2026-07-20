---
title: Compose batched MHA into the block
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T22:41:31.071992+02:00"
---

Replace the single-head attn-eq.f attention inside the block composition with the landed trainable batched mha.f (B,T,C,H,hd; fused QKV) so the trainable-MHA and extent-role-product dependencies are actually consumed by the composition instead of a toy stand-in. Respect the recorded SV-6 boundary (mha.f:32-51): head-major materialization is the honest bridge.
