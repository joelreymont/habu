---
title: Compose batched MHA into the block
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-20T22:41:31.071992+02:00\""
---

Replace the single-head attn-eq.f attention inside the block composition with the landed trainable batched mha.f (B,T,C,H,hd; fused QKV) so the trainable-MHA and extent-role-product dependencies are actually consumed by the composition instead of a toy stand-in. Respect the recorded SV-6 boundary (mha.f:32-51): head-major materialization is the honest bridge.

Claim: RELEASED 2026-07-21 (lane stopped with dig-protocol evidence, zero edits - the SV-6-anticipated wall). Stop evidence: the MODEL: parser has exactly two token-resolution paths (registered composable SPEC: equation via EQ-FIND, or fixed core op-kind; anything else E-CAD-OP) and mha.f's batched equations are structurally unregistrable: MHA-S carries 4 free indices vs the EQ-COMPOSABLE? cap of 2 (spec.f:769-773), EQ-REGISTER exits without registering (:805-806), and mha.f:60-64 records host-only as design. B=1 binding does not help (composability gates on declared free-index count, not magnitudes), and mha.f bakes toy extents #MB=2 etc. whose edit would break its locked suite. Splicing host steps would drop attention out of the differentiated graph - a regression. Re-sequenced: now blocked on habu-op-mha-fused-0fa2ef1b (the fused MHA op-kind with an author-facing MODEL: token - the honest enabling capability); alternatively unblocks if SV-6 N-D strided views + stride-aware feeds land first. The single-head attn-eq.f in-graph attention remains the correct landed milestone meanwhile.
