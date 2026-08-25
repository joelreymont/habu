---
title: Prove continuous GPT-2 batching
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.773208+02:00"
blocks:
  - habu-infer-scheduler-bounded-53574658
  - habu-infer-batch-decode-fbb73535
---

Why: unit descriptors and kernels do not prove the shared scheduler preserves each real request under interleaving, completion, and cancellation.

Result: a production-path GB10 fixture starts one real GPT-2 model, one INFER engine, and one SCHED scheduler with batch capacity greater than one; submits fixed prompts with different lengths and seeds; drives only SCHED:TICK into caller-owned result rows; and compares every token identifier, byte fragment, finish reason, and count with the same requests run independently through INFER. The trace includes two decode rows in one device step, page crossing, early EOS, waiting cancellation, decoding cancellation between ticks, queue and KV exact fit and rejection, then clean shutdown. Repeat from fresh owners.

Add no prefix sharing, JSON/HTTP, alternate forward execution, benchmark table, latency gate, optimized prefill, second engine or cache, retry, compatibility mode, or synthetic model. Owner: integrated multi-request GPT-2 correctness acceptance only. Production red: no real path executes two interleaved GPT-2 requests through one model and cache. Acceptance: both runs match independent results; at least one NEXT-MANY call has two rows; cancellations affect only their request; STOP finds no live request, sequence, or KV batch. Smallest owning check: this single correctness-only GB10 integration test. Claim: unassigned.
