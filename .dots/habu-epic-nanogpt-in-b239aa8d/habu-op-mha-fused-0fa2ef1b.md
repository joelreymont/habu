---
title: "OP-MHA: fused batched attention MODEL: node"
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T06:37:19.914101+02:00"
---

Enabling capability identified by the batched-MHA composition lane's stop (2026-07-21, dig-protocol evidence in habu-compose-batched-mha-d3166a09): the MODEL: parser resolves body tokens ONLY to registered composable SPEC: equations (EQ-FIND) or fixed core op-kinds (OP-KIND, maki/cad.f:743/759/287, else E-CAD-OP cad.f:130), and mha.f's rank-4 batched equations are structurally unregistrable (4 free indices vs EQ-COMPOSABLE? cap of 2, spec.f:769-773; EQ-REGISTER exits unregistered :805-806; host-only boundary recorded mha.f:60-64). Fix: introduce a fused MHA op-kind threading MHA-FWD/MHA-BWD through the op surface as a seg-attn-style fused node WITH an author-facing MODEL: token (unlike OP-SEG-ATTN which has none): op-kind.f enum + OPKIND>N, op-registry.f row + refs, adjoint.f VJP id + wiring, executor.f dispatch, cad.f token map + bind-shape, backward.f synthesized-node emission, move-facts.f. Attention stays in the differentiated graph (per-slot gradcheck for wq/wk/wv/wo preserved); extents come from the bind, not mha.f's baked toy constants - decide the extent-plumbing shape against the extent-role machinery. Device leg stays with habu-gb10-batched-attention-3055d565; host executor first. Acceptance: a MODEL: body writes the MHA token, captures, binds real (B,T,C,H,hd), forward matches MHA-FWD host reference element-close, gradcheck V-PASS on all four projection weights + input, run-twice locked, fail-closed device-lowering reject until the device dot lands.
