---
title: Multi-head self-attention sublayer composition
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-18T15:25:04.133383+02:00\""
closed-at: "2026-07-19T15:21:41.243178+02:00"
close-reason: MHA sublayer merged (835655d0/f732124e) and verified on master.
---

The single-running-value MODEL: DSL CANNOT express an internal Q@K^T: it drains params FIFO and threads ONE running value (cad.f CAP-EMIT-PARAMS, adam-train.f note); ADAM-ATTN sidesteps it by passing Q,Kt,V as SEPARATE inputs, single-head, no mask. attention.f golden is single-head/no-mask/no-batch. Author the FULL sublayer as SPEC: lines: QKV projections from x, head split/merge, scaled scores, causal mask, softmax, A@V, output projection. Deps (noted): causal-attention-mask, batch-sequence-tensor design; hard-blocked on SPEC: word (habu-spec-word-generating).

UNBLOCKED + AUTHORING GUIDANCE 2026-07-19 (orchestrator): the SPEC: word landed, and maki/spec-attention-test.f now proves single-head attention forward authored as SPEC: lines matches the maki/attention.f golden exactly - including the transposed Q.K^T operand, which the shipped grammar already expresses (S[am an] = Q[am ak] K[an ak] * +SUM ak). Composition decision: author the sublayer as plain checked colon words that compose the SPEC:-generated contraction words with named row ops (scale, causal-masked softmax - the segment/causal words that just landed in maki/causal.f and maki/segment.f), exactly as SPAT-FWD does in the test; there is NO multi-statement SPEC block surface, and none should be built unless this dot's authoring hits a concrete wall - if it does, stop and record the wall in this dot rather than growing the grammar ad hoc. Bias adds and residual adds are not SPEC:-expressible yet; that is habu-spec-broadcast-forms-ad851424 (use named ops meanwhile). Batch (B,T,C) stays out of scope until habu-extent-role-product-8e364885 lands.

Destruction review 2026-07-19: the landed file is a fixed T=4,C=6,H=2,B=1 forward oracle. It has no adjoint, model/executor/device integration or Q/K/V projection biases, and therefore is not a trainable GPT-2 MHA sublayer. Corrective owners: habu-complete-trainable-multi-39e26b3d for semantics and habu-own-multi-head-c863298a for package/workspace/API safety. Treat the close reason as the toy-forward milestone only.

Claim: agent=mha-opus workspace=.jj-ws/habu-multi-head-self-a1e0692f
