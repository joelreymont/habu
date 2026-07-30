---
title: Cancel one scheduler request
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.828511+02:00"
blocks:
  - habu-retire-scheduler-rows-517efa9a
---

Why: a disconnected client must retire its request and exact KV reservation once between ticks without affecting another row; normal and model-failure retirement belongs to TICK through the same aggregate transition.

Result: public SCHED CANCEL runs only between synchronous TICK calls. It validates one waiting, prefilling, or decoding handle, places that row in the fixed retirement list, and invokes RETIRE-MANY once with count one. Success releases the sequence, reservation, pages, scratch, queue position, and request row. Refusal returns cleanup-failed(terminal,req,code) from the shared transition; ordinary repeated, stale, cross-scheduler, terminal-request, or mid-TICK rejection returns the healthy scheduler unchanged and never constructs terminal. Package KV remains the sole cache-state mutation owner; SCHED receives no cache carrier and reaches it only through INFER:CLOSE-MANY. Every device call is synchronized or its lost session is quiesced before CANCEL can run.

Add no second retirement transition, public constructor, exposed identity field, cancellation flag, signal, thread, task, detached cleanup, retry state, swallowed error, snapshot, event queue, or transport callback. Owner: caller-requested single cancellation only; RETIRE-MANY owns reclamation and terminal state, and TICK owns result writing. Production red: no public cancellation composes the shared all-row reclamation path. Acceptance: every live state cancels through RETIRE-MANY count one; refusal returns terminal and the exact nominal handle without mutation; a peer remains byte-identical; stale and double cancellation reject with the healthy scheduler; no CLOSE-SEQ or private single-row RETIRE exists. Smallest owning check: focused state-by-state cancellation through real INFER/KV. Claim: unassigned.
