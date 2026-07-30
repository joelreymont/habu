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

Why: engine lifetime and request-sequence storage are independent owners and the combined leaf is too broad. Result: extend package INFER engine with one fixed row table and preallocated prompt-token, logit, random, output, descriptor, and close-list storage. OPEN-SEQ takes prompt bytes, maximum output tokens, SAMPLE:config, and seed; it validates top-k against the active model valid count, tokenizes once through the active model arm, proves prompt plus output capacity, reserves exact KV capacity, initializes the explicit random state, and mints one opaque copyable seq handle. CLOSE-MANY ( INFER:engine ptr INFER:seq CAD-NUM:item-count -- INFER:close-result ) copies one or more distinct handles into engine scratch, validates that every row belongs to the engine and has no pending work, calls KV:CLOSE-MANY once, and only after success clears every row by total stores. It returns closed(engine) or refused(engine,seq,code) without mutation. There is no separate CLOSE-SEQ; batch-of-one callers use the same transition. Dependencies: the engine lifetime owner and upstream sampling value types. Owner: INFER sequence rows, OPEN-SEQ, and CLOSE-MANY only. Production red: no request can own prompt, sampling state, output, and KV reservation under one handle, and no all-row close composes the KV transaction. Acceptance: exact capacity and one-over, invalid top-k, one and many closes, pending-close, duplicate, stale, copied, cross-engine, double-close, tokenization refusal, reservation refusal, injected Nth-row failure, and row reuse preserve every owner; two engines interleave; CLOSE-SEQ is absent. Forbidden: sampling algorithm, NEXT-MANY, synchronization, caller token count, second tokenization pass, public row fields, per-request engine, allocation after engine start, callback, version, compatibility API, or second close policy. Smallest owning check: bin/hb --load maki/infer/engine-sequence-test.f.
