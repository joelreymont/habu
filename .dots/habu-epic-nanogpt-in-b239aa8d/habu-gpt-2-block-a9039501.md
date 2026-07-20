---
title: GPT-2 block + full model composition (Nx, pre-LN, residuals, LM head)
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-18T15:25:04.149287+02:00\""
blocks:
  - habu-multi-head-self-a1e0692f
---

Compose the GPT-2-small model: token+pos embed -> Nx [pre-LN, MHA, residual, pre-LN, MLP(GELU), residual] -> final LN -> LM head. Residuals ARE expressible today via MODEL: >V NAME + named ref (plan-vocab-test.f shows node.in=x skip); MLP is LINEAR GELU LINEAR (from-scratch-model.f). MISSING: the whole-model composition tying the sublayers together as SPEC:/MODEL:. Deps (noted): affine-layernorm, learned-positional-embedding, cross-entropy-loss, batch-sequence-tensor; hard-blocked on the MHA sublayer dot.

2026-07-19 constraint from posembed lane: the single-running-value MODEL: DSL cannot root two independent table lookups in one body (each rooted op needs the running value; proven E-CAD-PARAM-SHAPE/E-CAD-REF). The block input composition (wte gather + wpe slice + ADD) therefore needs either the SPEC: chain multi-root form or pre-shaped operands; plan the block body accordingly.

2026-07-19 LARGEST HONEST SUB-COMPOSITION LANDED (maki/gptblock-test.f): 11-node MODEL: GPTBLK = token+pos embed -> pre-LN(affine fused op) -> MLP(LINEAR GELU LINEAR) -> residual -> final affine LN (composed LAYERNORM->BCAST-MUL->BIAS) -> LM head, trained end-to-end on TT-XENT int targets: forward golden exact, GC-RUN V-PASS all 13 inputs, 12/13 slots receive gradients (ids correctly not), deterministic 1802->397 mCE. TWO PROVEN WALLS keep this dot open: (1) MHA cannot enter the differentiable graph - maki/mha.f is forward-only, the single-running-value DSL cannot root Q@K^T over the running representation (see habu-differentiable-attention-via-a23b42d4 dot for the equation-op path); (2) cad.f CAP-PEND-CAP=4 named-ref queue forced composing the final LN from primitives (see habu-raise-model-named-e5412b7e dot). Full block = MHA differentiability + (optionally) the cap raise. Claim released (sub-composition lane done).

2026-07-20 SERIALIZED behind the affine lane (spark): OP-LAYERNORM identity/arity migration touches the composed final-LN nodes this block test uses; dispatch after habu-make-affine-layernorm-ddb6d70d merges.

2026-07-20 BOTH WALLS DOWN, dispatching completion: wall (1) fell with the equation-op attention landing (a5da3318 - attention over the running value trains end-to-end, K/V projections folded into the score/context einsums); wall (2) fell with the CAP-PEND ring (1bb03366 - cap now bounds OUTSTANDING refs = 7). Affine LN identity also landed (ef4e8233). File moved to maki/examples/nanogpt/gptblock-test.f in the restructure (cec8db65).
Claim: agent=gptblock workspace=.jj-ws/fable-gptblock machine=spark (owns maki/examples/nanogpt/gptblock-test.f + a possible new composition file + registration; must NOT edit maki/spec.f (rank0reg lane) or maki/maki.f/eval/checkpoint-test/adamw-test/pos-embed-test (decouple lane))
