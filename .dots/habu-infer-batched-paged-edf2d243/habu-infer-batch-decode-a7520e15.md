---
title: Run inference rows
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.756554+02:00"
blocks:
  - habu-finalize-provisional-kv-b8b46613
  - habu-prepare-inference-rows-4ff81dea
---

Why: prefill and decode must share one authenticated device transaction; separate single-row and batch commits would duplicate KV and sequence authority.

Result: bare package-private RUN-ROWS ( INFER:engine INFER:plan -- INFER:run-result ) consumes the immutable plan and carries the public unconstructible KV:batch, KV:ready, DEVRT:pending, DEVRT:poison, DEVRT:DONE, and DEVRT:QUIESCED values intact. Only each declaring package constructs its nominal carrier; public UNMAKE exposes only non-authoritative fields, and no foreign package can mint or substitute the carrier. RUN-ROWS obtains matching immutable KV:desc rows through KV:ROW, authenticates them through KV:MATCH-DESC immediately before enqueue, passes only those rows to the active model arm, and invokes that arm once. A refusal before the first enqueue uses CANCEL-BATCH or CANCEL-READY. The first enqueue returns pending. SYNC complete with success DONE calls FINALIZE-DONE and then commits the preflighted sequence history/logit rows with total stores; complete with failed DONE calls FINALIZE-DONE and leaves every sequence at its prior boundary. Poison enters QUIESCE; QUIESCED calls FINALIZE-QUIESCED and returns a terminal engine that cannot launch again, while teardown refusal returns every live owner and error. PREFILL and NEXT-MANY alone build plans. The model arm initially advertises one row; GPT-2 batching raises that cap without changing this transaction.

Add no sampling, detokenization, random/output publication, request cleanup, scheduler state, snapshot, completion mask, caller-supplied page or length, persisted descriptor, raw descriptor pointer, pointer to logits, partial commit, sorting, allocation, foreign carrier construction, or public KV mutation. Package KV performs every cache mutation; INFER alone retains its live cache. SCHED, GPT2DEV, and DEVRT receive no ready carrier or cache mutator. Owner: sole INFER device execution, descriptor authentication, and KV/history/logit atomic commit only. Production red: prefill and decode currently require separate transaction owners. Acceptance: the real package/type gate accepts the complete RUN-ROWS caller chain while rejecting foreign construction of every carrier; public UNMAKE cannot mint or substitute authority; one-row prefill and decode plans serialize deterministically; mutated, stale, cross-cache, or cross-session descriptors reject before enqueue; every pre-enqueue, mid-enqueue, operation, synchronization, poison, quiesce, and finalize failure selects the exact phase and leaves histories, logits, and committed KV unchanged; success retains no plan, batch, ready, pending, poison, DONE, or QUIESCED owner. Multi-row proof belongs to the later GPT-2 arm. Smallest owning check: bin/hb --load maki/infer/engine-run-rows-test.f on DGX Spark. Claim: unassigned.
