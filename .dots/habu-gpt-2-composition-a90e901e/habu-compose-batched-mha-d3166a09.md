---
title: Compose batched MHA into the block
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-20T22:41:31.071992+02:00\""
---

Replace the single-head attn-eq.f attention inside the block composition with the landed trainable batched mha.f (B,T,C,H,hd; fused QKV) so the trainable-MHA and extent-role-product dependencies are actually consumed by the composition instead of a toy stand-in. Respect the recorded SV-6 boundary (mha.f:32-51): head-major materialization is the honest bridge.

Claim: agent=bmha workspace=.jj-ws/fable-bmha machine=spark (owns composing batched mha.f into the block: maki/examples/nanogpt/gptblock-attn-test.f attention leg + consumed mha machinery)
