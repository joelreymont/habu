---
title: "Infer: continuous batching scheduler"
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T15:58:37.109397+02:00"
blocks:
  - habu-infer-batch-decode-7df51c5c
---

Campaign only; do not dispatch. Its leaves add one linear scheduler around the shared INFER engine: nominal request identity, strict immediate KV admission, FIFO order, synchronous cancellation, alternating one-token prefill and decode-batch selection, and one bounded TICK that writes caller-owned result rows. Engine, sequence, cache, tokenizer, sampling, and model ownership remain in INFER; transport state remains outside SCHED.

There is one admission policy and one host thread. Prefill advances one stored token per tick; decode uses real INFER:NEXT-MANY batching. No configurable or optimized prefill, completion mask, priority policy, worker pool, asynchronous device work, snapshot, second cache, event queue, benchmark, metric, version, compatibility path, or new lint belongs here. Close with the same integrated multi-request GPT-2 parity leaf as the batched-device campaign.
