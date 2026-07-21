---
title: "Infer: modern dense model family"
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T16:44:54.193349+02:00"
blocks:
  - habu-infer-end-to-20fa7684
  - habu-infer-compiled-model-a204eadc
  - habu-infer-swiglu-op-e778ab7a
---

Plan-of-record M5 - the first PRODUCT gate (GPT-2 is the oracle, not the product): one pinned dense 7-8B decoder-only checkpoint with GQA + RoPE + RMSNorm + SwiGLU, supported tokenizer, conventional MLP, published reference. Additions this demands: normalized HF config intake; the model's tokenizer + special-token handling (chat templates OUTSIDE the core engine); GQA decode (the kernel is GQA-ready by contract); large-vocab LM head; packed layouts via the model-pack dot; correctness fixtures on a public prompt/eval set. Gates: greedy parity vs a trusted reference; logit tolerance declared and measured for nonexact paths; bf16 steady-state decode within the provisional competitive band set after M0; safe startup + controlled rejection at the memory boundary. Also blocked on the model-pack format and SwiGLU dots - frontmatter.
