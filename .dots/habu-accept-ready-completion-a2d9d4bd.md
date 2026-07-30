---
title: Accept ready completion sockets
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:39:44.168176+02:00"
blocks:
  - habu-own-completion-srv-dfc8812e
---

Why: listener readiness and free-row publication are independent from existing connection I/O. Result: package-private SERVE:ACCEPT-READY consumes one healthy server whose listener is readable, captures TIME:MONO-NS once for the pass, preflights the next server-local response id, calls SOCK-OS:ACCEPT until would-block or no free row, and passes each descriptor, exact planned spans, current response id, stored idle interval, and accepted-at time to SERVE-CONN:OPEN. It advances the id exactly once only after a connection publishes. Exhaustion refuses before another ACCEPT. A refused connection open closes that descriptor once, preserves the free row, response counter, and healthy scheduler, and returns refused(server,error); it never fabricates a terminal scheduler. Owner: ready-listener accept loop, server response-id advance, accepted-at capture, and free-row publication only. Production red: a polled listener cannot create a connection row with an owned id and deadline source. Acceptance: zero capacity, one and several accepts, would-block, id exhaustion, clock or deadline refusal, connection-open refusal, descriptor-close failure, full table, and two servers preserve exact counters, rows, and owners. Forbidden: poll planning, poll syscall, existing-row I/O, deadline formula, scheduler tick, retry, allocation, compatibility, metric, or lint. Smallest owning check: real SOCK-OS accept traces through SERVE-CONN:OPEN. Claim: unassigned.
