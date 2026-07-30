---
title: Retire scheduler rows atomically
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T06:46:56.774355+02:00"
blocks:
  - habu-infer-scheduler-fifo-88f80a53
  - habu-publish-owner-product-32b3f03c
---

Why: one NEXT-MANY call can finish several rows, so cleanup must validate and reclaim the entire terminal set before any row disappears. Result: package SCHED declares the public linear terminal owner and MATCH-TERMINAL, then defines package-private RETIRE-MANY over one or more distinct authenticated request rows. RETIRE-MANY stages and publishes no result; it validates every row and terminal reason, calls INFER:CLOSE-MANY exactly once, and only after success performs total queue and row reclamation. Refusal returns cleanup-failed(terminal,req,code) with every request row and committed outcome retained; req identifies the first failing nominal handle only. SCHED:STOP is the only retry consumer and either completes the same aggregate retirement and engine stop or returns terminal intact. Owner: aggregate retirement, terminal carrier, terminal identity match, and terminal STOP arm only. Dependencies: scheduler FIFO rows and owner-only product construction. Production red: sequential close calls can reclaim earlier terminal rows before a later refusal hides a committed batch. Acceptance: one and simultaneous terminal, failed, and cancelled sets reclaim once; duplicate, stale, cross-scheduler, mixed-pending, and injected Nth-row failures mutate no engine, cache, queue, row, result storage, or peer; refusal returns one STOP-only terminal owner; matching identity succeeds, foreign identity and construction reject; STOP retry is all-or-none. Forbidden: public constructor, public batch API, result publication, transport callback, retry loop outside STOP, alternate cancellation policy, compatibility, metric, or lint. Smallest owning check: real SCHED rows through INFER:CLOSE-MANY with every failure position. Claim: unassigned.
