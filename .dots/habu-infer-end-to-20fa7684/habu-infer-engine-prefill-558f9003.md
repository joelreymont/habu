---
title: "Infer engine: prefill into paged KV"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.401788+02:00"
blocks:
  - habu-infer-engine-owned-99a98d17
  - habu-infer-gpt2-full-0d5c61d8
  - habu-infer-kv-atomic-cdfb00cb
  - habu-infer-kv-atomic-c402952e
---

Why this exists:
the oracle engine must turn one tokenized prompt into model state and the exact paged KV layout used by decode.

Required result:
compose the existing host prefill path with maximum-context admission and append every prompt position into one sequence.

Done when:
fixed prompts produce the host-reference final logits and exact KV lengths/pages; failed tokenization, model step, or append cancels the sequence and returns all ownership.

Expected touch points: new maki/infer/engine-prefill.f, focused test, FILEMAP.md.
Smallest check: focused prefill and failure-cleanup tests.
Prerequisites: owned engine state, GPT-2 full host logits, atomic KV append and cancellation.
Owned result: single-sequence prefill composition only.
Claim: unassigned.
