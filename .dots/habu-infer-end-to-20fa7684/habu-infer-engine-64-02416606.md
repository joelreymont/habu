---
title: "Run 64 GPT-2 tokens"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.417979+02:00"
blocks:
  - habu-infer-engine-sample-0f2a4ef4
---

Why this exists:
the first product milestone requires a complete real checkpoint prompt-to-output proof through INFER, not isolated kernels or another forward implementation.

Required result:
add one production-path command/test that opens the pinned GPT-2 checkpoint and tokenizer assets, starts INFER with capacity one and the reference KV bound, opens one sequence from the exact committed prompt bytes, output maximum, and seed, prefills its stored tokens, performs 64 greedy batch-of-one NEXT-MANY transactions through the persistent GB10 model and paged cache, captures identifiers and emitted bytes in caller-owned row storage, then closes the sequence and stops the engine. GPT2-REFERENCE is comparison data only and never provides runtime values.

Done when:
all 64 token identifiers equal GPT2-REFERENCE exactly across page boundaries, emitted bytes equal the pinned reference, two complete runs are identical, and normal completion plus injected prefill/step/output cancellation return every cache, batch, sequence, source, and device owner. Device probes and logit parity remain exclusively in the GPT2DEV:LOGITS owning check; this public command adds no observer or raw-logit surface.

Expected touch points: one INFER command/test and existing canonical fixture.
Smallest check: the real GB10 64-token command with ownership counters before and after.
Prerequisites: INFER sampling/detokenization and committed GPT2-REFERENCE data.
Owned result: first end-to-end GPT-2 product acceptance only.
Claim: unassigned.
