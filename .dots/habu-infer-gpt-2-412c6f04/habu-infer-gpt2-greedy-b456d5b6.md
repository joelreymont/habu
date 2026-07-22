---
title: "Infer GPT2: greedy oracle loop"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:40:20.859444+02:00"
blocks:
  - habu-infer-gpt2-full-0d5c61d8
  - habu-infer-gpt2-ref-b3c4da77
---

Why this exists:
a single forward does not yet prove autoregressive state, tokenizer integration, or the 64-token oracle required by M4.

Required result:
run the full host forward iteratively with canonical greedy selection and tokenizer inputs, recording host tokens per second only as a correctness baseline.

Done when:
fixed prompts produce 64 or more exact reference token identifiers, run twice is identical, intermediate checkpoints remain inspectable, and timing is clearly labeled host baseline.

Expected touch points: GPT-2 host loop module/test and canonical result record.
Smallest check: 64-token parity test and M0 schema validation.
Prerequisites: full host logits and reference fixture provenance.
Owned result: host greedy loop and baseline record only.
Claim: unassigned.
