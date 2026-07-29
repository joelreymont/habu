---
title: Retire scheduler requests
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.828511+02:00"
blocks:
  - habu-infer-scheduler-fifo-88f80a53
  - habu-publish-owner-product-32b3f03c
---

Why: a disconnected client must retire its request and exact KV reservation once between ticks without affecting another row; normal and model-failure retirement belongs to TICK.

Result: one private RETIRE transition and public SCHED CANCEL run only between synchronous TICK calls. RETIRE preflights and then closes the exact INFER sequence, releases its reservation/pages/scratch, and reclaims the request for stop, length, model failure, or explicit cancellation. TICK stages a terminal result but publishes no row_count until RETIRE succeeds. Package SCHED declares public linear owner product terminal with no public constructor. An unexpected close refusal returns cleanup-failed(terminal,req,code) with every request owner and the scheduler's exact identity intact. Only SCHED:STOP may retry terminal scheduler cleanup; MATCH-TERMINAL and capability-guarded transport teardown may authenticate and thread terminal unchanged while consuming socket owners. MATCH-TERMINAL ( SCHED:terminal SCHED:id -- SCHED:terminal-match-result ) returns matched(terminal,id) only for the originating scheduler or refused(terminal,id,cross-scheduler) without mutation. CANCEL applies RETIRE to waiting, prefilling, or decoding state without publishing a result. Ordinary repeated, stale, cross-scheduler, terminal-request, or mid-TICK rejection returns the healthy scheduler unchanged; it never constructs terminal. Every device call is synchronized or its lost session is quiesced before RETIRE can run.

Add no public constructor, exposed identity field, cancellation flag, signal, thread, task, detached cleanup, retry state, swallowed error, snapshot, event queue, or transport callback. Owner: the sole request retirement transition, caller-requested cancellation, terminal scheduler carrier, and terminal-identity match; TICK owns result writing. Production red: admitted and terminal rows have no common exact reclamation or scheduler-bound failure capability. Acceptance: foreign terminal construction rejects; matching id succeeds and another live scheduler's id rejects with both values intact; every RETIRE refusal uses only cleanup-failed(terminal,req,code); each live and terminal state releases its sequence, reservation, pages, queue position, and scratch once; cleanup refusal returns terminal and exact live handle without mutation; a peer remains byte-identical; stale and double cancellation reject with the healthy scheduler. Smallest owning check: focused state-by-state retirement and cancellation trace through real INFER/KV. Claim: unassigned.
