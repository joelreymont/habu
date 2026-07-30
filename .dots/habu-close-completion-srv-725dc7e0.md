---
title: Close completion server rows
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:31:04.951701+02:00"
blocks:
  - habu-own-completion-srv-dfc8812e
---

Why: server teardown must drain every connection before scheduler and listener shutdown. Result: package SERVE defines CLOSE-ROWS over one stopping server. It stops admission, walks each fixed connection row once, threads healthy SERVE-CONN:CLOSE until the first scheduler terminal, then authenticated CLOSE-AFTER-SCHED-FAIL for every remaining row. Socket close failures are recorded and consumed; no descriptor or writer is retried. Success returns one drained server state with zero connection owners, the original listener, storage, and either healthy scheduler or matching terminal. Any identity mismatch returns the exact terminal server state and untouched remaining rows. Owner: complete connection-row shutdown only. Production red: a cancellation failure can strand later descriptors or writers. Acceptance: zero, one, several, every failure position, cross-server terminal, writer close, socket failure, and two servers prove each attempted owner is consumed once and every unattempted owner remains exact. Forbidden: listener close, scheduler stop, retry, polling, result routing, allocation, compatibility, metric, or lint. Smallest owning check: the STOP row-walk slice of maki/serve/server-test.f. Claim: unassigned.
