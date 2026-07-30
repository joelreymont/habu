---
title: Select one scheduler tick
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.823651+02:00"
blocks:
  - habu-infer-scheduler-fifo-88f80a53
---

Why: deterministic continuous service needs one small policy that decides between prompt prefill and a decode batch without mixing execution or transport state into selection.

Result: package-private SCHED selection writes one plan into fixed scratch without mutating rows. It alternates service classes when both exist: one oldest waiting or prefilling request for one INFER:PREFILL token quantum, then up to maximum-batch oldest decoding requests for one INFER:NEXT-MANY call. If only one class exists it selects that class. Survivors retain FIFO order and a request appears at most once. The service phase advances only after the engine operation and total result/state copy; rejection leaves the prior phase and rows intact.

Add no configurable or optimized prefill, priority, deadline, second admission profile, completion mask, device descriptor, snapshot, transport state, allocation, or public plan API. Owner: tick selection policy only. Production red: no deterministic policy joins admitted rows to prefill and decode work. Acceptance: exhaustive small matrices and independent traces prove one-token prefill, alternation, FIFO survivor order, maximum batch, no duplicates, and no phase advance on failure. Smallest owning check: focused pure selection and mutation test. Claim: unassigned.
