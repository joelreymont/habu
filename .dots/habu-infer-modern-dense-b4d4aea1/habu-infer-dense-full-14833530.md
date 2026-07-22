---
title: "Infer dense: full BF16 continuation parity"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.463168+02:00"
blocks:
  - habu-infer-dense-tokenizer-a4453246
  - habu-infer-dense-grouped-bc7e6656
  - habu-infer-dense-large-a2437ab1
  - habu-infer-pack-boring-c8e07d29
  - habu-infer-engine-64-02416606
---

Why this exists:
the first product architecture is unproven until the complete BF16 engine produces trusted continuations.

Required result:
compose tokenizer, packed model, prefill, GQA paged decode, SwiGLU blocks, vocabulary head, and greedy selection.

Done when:
fixed public prompts match trusted greedy continuations and declared logit tolerance, run twice is identical, and cancellation releases all resources.

Expected touch points: modern-model end-to-end test and canonical fixtures.
Smallest check: correctness-only GB10 continuation run.
Prerequisites: tokenizer, grouped-query decode, large vocabulary head, model-pack loader, single-sequence engine.
Owned result: full BF16 correctness acceptance only.
Claim: unassigned.
