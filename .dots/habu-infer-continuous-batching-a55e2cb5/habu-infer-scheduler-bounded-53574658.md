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

Result: SCHED TICK ( SCHED:sched ptr SCHED:result-row CAD-NUM:item-count ptr u8 CAD-NUM:byte-len -- SCHED:tick-result ) borrows a caller-owned fixed result table and byte arena. SCHED owns the row schema, capacity calculation, and sole writer but stores no result table or byte arena. Before INFER, TICK preflights the table for every selected request and the arena for one MAX-TOKEN-BYTES slice per selected decode row. A prefill plan calls INFER:PREFILL once for one token quantum and moves a ready row to decoding. A decode plan calls INFER:NEXT-MANY once for all selected rows. NEXT-MANY returns every live sequence and committed outcome; it never closes one. TICK copies the complete batch into staged storage, collects every terminal or failed request, then calls RETIRE-MANY once. Success returns ticked(sched,row-count). Cleanup refusal returns cleanup-failed(terminal,row-count,req,code): RETIRE-MANY changed no request, and the complete already-committed batch is published so no token or terminal outcome is lost; the caller must route those rows before SCHED:STOP. No further PREFILL or NEXT-MANY is possible on terminal. Ordinary result copies and transitions cannot fail after INFER because destinations were preflighted. TICK never returns a reusable scheduler with pending KV work, unsynchronized device access, partial rows, or a terminal live request. The caller can poll sockets between calls.

Add no background worker, thread, task, asynchronous launch, second prefill kernel, retry loop, partial result publication, transport read or write, stored result arena, event queue, timing, allocation, or per-row device loop. Owner: synchronous scheduler execution, result schema, sole writer, and complete-batch publication only; RETIRE-MANY owns reclamation and the caller owns storage. Production red: no production call advances multiple admitted requests through the shared engine without losing results when cleanup fails. Acceptance: short table or byte arena rejects before execution; one and several token-final, finished, failed, and mixed token rows reclaim all-or-none; injected Nth-row cleanup refusal returns the untouched terminal owner and the exact complete committed batch, never zero or a prefix; the caller routes it before STOP; injected PREFILL, sample, descriptor, kernel, synchronization, and KV finalize failures follow the exact named result path; idle and alternating traces are deterministic. Smallest owning check: bin/hb --load maki/infer/scheduler-test.f with the real engine provider on DGX Spark. Claim: unassigned.
