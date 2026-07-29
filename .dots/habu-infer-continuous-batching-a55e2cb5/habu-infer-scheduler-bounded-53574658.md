---
title: Run one scheduler tick
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.834815+02:00"
blocks:
  - habu-infer-scheduler-token-3490da89
  - habu-infer-scheduler-cancellation-523c6cb8
  - habu-infer-batch-decode-fbb73535
---

Why: selection, engine execution, and caller-result publication need one synchronous transaction that transports can drive repeatedly.

Result: SCHED TICK ( SCHED:sched ptr SCHED:result-row CAD-NUM:item-count ptr u8 CAD-NUM:byte-len -- SCHED:tick-result ) takes a caller-owned fixed result table and byte arena using the schema and bound owned by SCHED. Before INFER, TICK preflights the table for every selected request and the arena for one MAX-TOKEN-BYTES slice per selected decode row. A prefill plan calls INFER:PREFILL once for one token quantum and moves a ready row to decoding. A decode plan calls INFER:NEXT-MANY once for all selected rows. It maps token, token-final, and finished step rows exactly; the transport accumulates token bytes. Before publishing row_count, TICK handles every token-final, finished, or engine failure through one private RETIRE that calls INFER:CLOSE-SEQ and reclaims the request row. If cleanup succeeds, the terminal row becomes visible and the handle is stale. If cleanup refuses, tick-result is cleanup-failed(terminal,req,code) with row_count zero; staged bytes are not published, the terminal request remains owned, no further PREFILL/NEXT-MANY is possible, and only SCHED:STOP or capability-guarded transport teardown may consume the terminal owner. Ordinary result copies and transitions cannot fail after INFER because destinations were preflighted. TICK never returns a reusable scheduler with pending KV work, unsynchronized device access, partial rows, or a terminal live request. The caller can poll sockets between calls.

Add no background worker, thread, task, asynchronous launch, second prefill kernel, retry loop, partial batch commit, transport read or write, event queue, timing, allocation, or per-row device loop. Owner: synchronous scheduler execution, sole result-row writing, and normal/failure retirement only; the caller owns the table and arena bytes. Production red: no production call advances multiple admitted requests through the shared engine and terminal rows leak today. Acceptance: short table or byte arena rejects before execution; token-final and finished reclaim sequence, reservation, pages, scratch, and request before publishing; injected cleanup refusal publishes zero rows and returns only the terminal STOP owner; injected PREFILL, sample, descriptor, kernel, synchronization, and KV finalize failures follow the same named failure/cleanup path; idle and alternating traces are deterministic. Smallest owning check: bin/hb --load maki/infer/scheduler-test.f with the real engine provider on DGX Spark. Claim: unassigned.
