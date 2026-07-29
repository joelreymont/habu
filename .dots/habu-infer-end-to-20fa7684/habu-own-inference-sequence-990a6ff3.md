---
title: Own inference sequence rows
status: open
priority: 1
issue-type: task
created-at: "2026-07-29T23:22:19.739387+02:00"
blocks:
  - habu-infer-engine-owned-99a98d17
  - habu-own-sampling-value-6dc1a8cf
---

Why: engine lifetime and request-sequence storage are independent owners and the combined leaf is too broad. Result: extend package INFER engine with one fixed row table and preallocated prompt-token, logit, random, output, and descriptor storage. OPEN-SEQ takes prompt bytes, maximum output tokens, SAMPLE:config, and seed; it validates top-k against the active model valid count, tokenizes once through the active model arm, proves prompt plus output capacity, reserves exact KV capacity, initializes the explicit random state, and mints one opaque copyable seq handle. CLOSE-SEQ is total only at the type-proven no-pending boundary: it validates the handle, delegates whole-sequence KV reclamation, and retires the row without independently synchronizing. Dependencies: the engine lifetime owner and upstream sampling value types. Owner: INFER sequence rows and OPEN-SEQ/CLOSE-SEQ only. Production red: no request can own prompt, sampling state, output, and KV reservation under one handle. Acceptance: exact capacity and one-over, invalid top-k, pending-close rejection, stale, copied, cross-engine, double-close, tokenization refusal, reservation refusal, and row reuse preserve every owner; two engines interleave. Forbidden: sampling algorithm, NEXT-MANY, synchronization, caller token count, second tokenization pass, public row fields, per-request engine, allocation after engine start, callback, version, or compatibility API. Smallest owning check: bin/hb --load maki/infer/engine-sequence-test.f.
