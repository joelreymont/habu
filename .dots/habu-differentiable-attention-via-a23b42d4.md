---
title: Differentiable attention via equation ops
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-19T23:49:05.200613+02:00\""
---

The load-bearing gap for nanoGPT end-to-end, proven by the GPT-2 block lane 2026-07-19: maki/mha.f is a forward-only buffer golden; the single-running-value MODEL: DSL cannot root Q@K^T over the running representation, so no block can train its attention (adam-attn-grad-test trains only PRE-PROJECTED Q/Kt/V as independent inputs). The machinery for the fix landed TODAY: equation ops (einsum) with adjoints DERIVED at declaration time through the same parser/emitter (habu-derive-adjoint-equations, closed) - S[m n]=Q[m k]KT[k n]*+SUM k is exactly attention scores, and its adjoints gradcheck. Do: (1) express the attention core as a composition of equation ops + softmax + scale: scores equation -> masked/scaled softmax (softmax.f golden; check whether a softmax OP exists in the differentiable graph or needs adding - seg-attn's fused internals are separate per its dot) -> context equation C[m k]=P[m n]V[n k]*+SUM n; (2) prove gradients flow to Q/K/V PROJECTIONS (linear ops feeding the equations) - i.e. the running value CAN root the projections, and the equations consume their outputs via named refs (respect CAP-PEND-CAP=4 or serialize behind habu-raise-model-named-e5412b7e); (3) gradcheck the assembled sublayer vs central FD; (4) train a 1-block model with real attention on integer targets, loss halves deterministically; (5) then the GPT-2 block dot (habu-gpt-2-block-a9039501) can compose the full Nx block. Causal masking: check what seg-attn/softmax goldens support; a causal mask constant is in scope, a new masking op-kind should be justified against existing ops first. Territory: maki (equations/softmax/attention files + tests). SERIALIZE: after habu-raise-model-named-e5412b7e if ref counting forces it.

Claim: agent=attn workspace=.jj-ws/fable-attn machine=spark (owns maki attention/equation/softmax files + cad.f/backward.f/executor.f op wiring this lane)
