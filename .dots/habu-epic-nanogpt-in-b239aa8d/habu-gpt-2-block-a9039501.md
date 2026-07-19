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

Claim: agent=gptblock workspace=.jj-ws/gptblock machine=spark
