---
title: "Hear every client close in ws-test's capacity case"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T18:26:34.088066+02:00"
---

Problem: in the full suite's run on 2026-11-25 (batch 708b7e0e; 315 of 316 passed), src/lib/net/ws-test.f CAPACITY-CASE (:1470-1482) failed at CLOSE-ALL (:1182-1185): after its 257 client connections closed, fewer than CROWD server sockets told their handler closed within WAIT-BOUND-MS (60 s), and the process then hung in futex waits until killed. ws-test passed alone 9 of 9 times, 6 of them under 12 busy loops. AIO keeps its completion queue from overflowing (src/lib/aio.f:8-9), so a lost completion there is ruled out; the cause is not found. The batch changes no WS, HTTP or AIO code beyond prefixed calls. Acceptance: the cause found and fixed; a server socket whose peer closes is always told closed. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-ws-hang.
