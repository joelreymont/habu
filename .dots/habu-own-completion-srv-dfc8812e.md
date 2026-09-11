---
title: Own completion server
status: open
priority: 1
issue-type: task
created-at: "2026-07-29T23:22:19.852365+02:00"
blocks:
  - habu-own-checked-ipv4-44f29beb
  - habu-infer-scheduler-req-1ac1dac6
  - habu-infer-serve-http-4fb09e9a
  - habu-plan-completion-srv-8258eae9
---

Why: listener and fixed server storage need one lifetime separate from polling and shutdown. Result: package SERVE defines one linear server and OPEN. OPEN consumes a healthy SCHED scheduler, immutable INFER:info, one checked SOCK-OS listener, one SERVE:plan, the exact pointer and CAD-NUM:alloc-byte-len minted by MEM:ALLOC-BYTES, and an idle interval, validates and stores the interval in nanoseconds, initializes a checked nonwrapping server-local next-response-id counter, and publishes only after all initialization succeeds. It validates the block against the plan, obtains SCHED:id, carves fixed connection rows with explicit JSON writer states and buffers, poll rows, exactly one SCHED result table and byte arena. Refusal returns scheduler, listener, plan, and the exact untouched memory pair. SCHED owns result schema, capacity, and sole writer; SERVE owns only the table and arena and lends them to TICK. Each later accept alone advances the response counter after SERVE-CONN:OPEN succeeds; exhaustion refuses before accepting another descriptor. FOOTPRINT returns the plan extents without recomputation. Owner: server layout, scheduler binding, response-id source, idle interval, OPEN, result storage, and footprint only. Production red: a listener, scheduler, and storage plan cannot form one product owner, and accepted connections have no response-id or idle-interval source. Acceptance: exact and one-short blocks, wrong plan, alignment, idle conversion overflow, response-id exhaustion, every partial initialization failure, two servers, zero and full connection rows, one result table and arena, and exact footprint preserve or publish every owner once. Forbidden: storage calculation, allocation, listener creation, clock read, deadline calculation, STOP, connection close walk, poll, result writing, thread, event framework, DNS, dynamic row, signal handler, plugin, version, compatibility, metric, or lint. Smallest owning check: the OPEN and footprint slice of maki/serve/server-test.f. Claim: unassigned.
