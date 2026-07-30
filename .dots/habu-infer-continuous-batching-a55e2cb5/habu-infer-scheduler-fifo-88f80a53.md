---
title: Admit requests in FIFO order
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.816726+02:00"
blocks:
  - habu-infer-scheduler-req-1ac1dac6
---

Why: admission and waiting order must be one strict policy with exact KV reservation, not a second profile or later best-effort allocation.

Result: SCHED SUBMIT ( SCHED:sched ptr u8 n n SAMPLE:config n -- SCHED:submit-result ) takes prompt bytes, maximum output tokens, sampling configuration, and seed. It validates parameters, selects the oldest free request row, calls the sole OPEN-SEQ so tokenization validates prompt plus output capacity and acquires the exact KV reservation, stores only scheduler counters and state, appends the request to one bounded first-in-first-out order, and returns accepted or rejected. Full rows, full KV reservation, invalid prompt, and invalid sampling reject with no published handle or state. Cancellation removes a row without changing survivor order. Sequence identity, sampling, random state, and reservation remain private to INFER.

Add no deferred admission, optimistic overcommit, second admission profile, retry queue, priority, request pointer, duplicate tokenizer pass, caller-declared prompt token count, global seed, transport/model string, allocation after START, or compatibility path. Owner: SUBMIT and strict FIFO waiting order only. Production red: no caller can atomically turn prompt bytes into one reserved queued request. Acceptance: exact-fit and one-token-over bounds, queue full, KV full, malformed prompt, duplicate/stale cancellation, row reuse, and fixed interleavings match independent FIFO and capacity oracles; every rejection leaves scheduler/engine/cache byte-identical. Smallest owning check: focused real-engine admission, order, and churn test. Claim: unassigned.
