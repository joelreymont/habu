---
title: "Infer decode: real-step contiguous parity"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.356835+02:00"
blocks:
  - habu-infer-decode-contiguous-e09bacf2
  - habu-infer-gpt2-full-0d5c61d8
---

Why this exists:
synthetic kernel checks do not prove the actual GPT-2 query, key, value, mask, and projection integration.

Required result:
feed committed real-model per-step tensors through the contiguous device kernel and compare its attention output at several decode positions.

Done when:
every selected step matches the GPT-2 host reference within the declared kernel tolerance and produces the same greedy next identifier when integrated at that layer.

Expected touch points: focused real-step fixture and device test.
Smallest check: correctness-only GB10 parity run.
Prerequisites: contiguous device kernel and GPT-2 full host logits.
Owned result: real-model contiguous parity fixture and test only.
Claim: unassigned.
