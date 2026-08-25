---
title: "Advance one prompt token"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.401788+02:00"
blocks:
  - habu-infer-batch-decode-a7520e15
---

Why this exists:
the engine must turn the prompt tokens authenticated by OPEN-SEQ into persistent device state through the production device forward without retokenizing.

Required result:
add prefill-result = more(engine,seq) | ready(engine,seq) | rejected(engine,seq,code) and PREFILL ( INFER:engine INFER:seq -- INFER:prefill-result ). It consumes exactly one remaining sequence-owned prompt identifier through package-private RUN-ROWS. A successful nonfinal identifier returns more; the final identifier returns ready with current logits. Failure returns the prior committed prompt position so the same call resumes exactly. This one-token quantum lets the scheduler return to peer sockets and decode rows between prompt steps without another prefill mechanism. No tokenizer call, caller token list, host forward, direct KV operation, optimized prompt kernel, configurable chunk size, second cache, or second prefill path.

Done when:
pinned prompts reach the committed prompt length and next-token identifier after repeated calls; each successful call advances exactly one token; failures at every RUN-ROWS boundary leave a sequence that can resume or close; two sequences and socket polling interleave deterministically. Device logits and paged K/V probes remain exclusively in their GPT2DEV and KV owning checks; PREFILL exposes neither.

Expected touch points: maki/infer/engine.f and focused production test.
Smallest check: real GPT-2 prompt prefill on GB10 plus one-token advancement and injected failure cleanup.
Prerequisites: package-private RUN-ROWS.
Owned result: one-token prefill advancement only.
Claim: unassigned.
