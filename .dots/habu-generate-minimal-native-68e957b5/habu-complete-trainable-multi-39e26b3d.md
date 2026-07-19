---
title: Complete trainable multi-head attention
status: open
priority: 1
issue-type: task
blocks:
  - habu-extent-role-product-8e364885
  - habu-spec-broadcast-forms-ad851424
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T23:32:50.255160+02:00"
---

The closed multi-head attention dot overstates a toy forward oracle as a full GPT-2 sublayer. maki/mha.f hardcodes T=4,C=6,H=2,hd=3 with no batch extent, exposes only MHA-FWD/MHA-SUBLAYER-FWD, and has no backward, gradcheck, model-IR node, executor or device lowering. It also omits Q/K/V projection biases entirely; GPT-2 c_attn has one bias for all three projections, while current code adds only output-projection bias. The active GPT-2 block therefore cannot compose or train its declared Nx model. Implement checked B,T,C,H,hd attention through extent-role products and segment-causal semantics, including QKV weights+biases, output weight+bias, residual contract, all parameter/input adjoints, host executor and device lowering. Prove external fixed forward/backward goldens, finite differences for X and every weight/bias, B>1 sequence isolation, causal masking, repeated-batch gradient accumulation, zero/small/max extents, and full GPT-block integration. Correct the closed MHA prose so fixed-shape forward remains an oracle rather than completion proof. Preserve fused-QKV performance ownership separately. Files: Maki MHA/SPEC/model/AD/executor/device tests and historical dots. Depends on habu-extent-role-product-8e364885 and habu-spec-broadcast-forms-ad851424; use unified STRUCTURE shapes after habu-lowering-hash-unified-586f7881.
