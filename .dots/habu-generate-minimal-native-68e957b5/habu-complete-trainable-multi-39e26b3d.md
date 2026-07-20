---
title: Complete trainable multi-head attention
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-19T23:32:50.255160+02:00\""
blocks:
  - habu-extent-role-product-8e364885
  - habu-spec-broadcast-forms-ad851424
  - habu-lowering-hash-unified-586f7881
---

The closed multi-head attention dot overstates a toy forward oracle as a full GPT-2 sublayer. maki/mha.f hardcodes T=4,C=6,H=2,hd=3 with no batch extent, exposes only MHA-FWD/MHA-SUBLAYER-FWD, and has no backward, gradcheck, model-IR node, executor or device lowering. It also omits Q/K/V projection biases entirely; GPT-2 c_attn has one bias for all three projections, while current code adds only output-projection bias. The active GPT-2 block therefore cannot compose or train its declared Nx model. Implement checked B,T,C,H,hd attention through extent-role products and segment-causal semantics, including QKV weights+biases, output weight+bias, residual contract, all parameter/input adjoints, host executor and device lowering. Prove external fixed forward/backward goldens, finite differences for X and every weight/bias, B>1 sequence isolation, causal masking, repeated-batch gradient accumulation, zero/small/max extents, and full GPT-block integration. Correct the closed MHA prose so fixed-shape forward remains an oracle rather than completion proof. Preserve fused-QKV performance ownership separately. Files: Maki MHA/SPEC/model/AD/executor/device tests and historical dots. Depends on habu-extent-role-product-8e364885 and habu-spec-broadcast-forms-ad851424; use unified STRUCTURE shapes after habu-lowering-hash-unified-586f7881.

2026-07-20 dependency status at dispatch: extent-role products LANDED (b192992e), batched free-extent SPEC equations with batch-isolation adjoints LANDED (2b6ad8f8), broadcast/elementwise forms LANDED (82941587), affine LN explicit form LANDED (ef4e8233), segment-causal goldens landed earlier; the unified-STRUCTURE qualifier stays interim (shapes via extents until habu-lowering-hash-unified lands - note it, do not block on it).
Claim: agent=mha workspace=.jj-ws/fable-mha machine=spark (owns maki/mha.f + new batched-attention composition/test files + segment/causal read-consumers; must NOT edit maki/cad.f model-ir.f tensor-value.f gptblock-attn-test.f - the derive45 lane owns them; executor.f/backward.f are landed surfaces - READ-ONLY, stop-and-report if a new op genuinely requires editing them; own-multi-head c863298a and fuse-multi-head 83294c30 SERIALIZE behind this lane)
