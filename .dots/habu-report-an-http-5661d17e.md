---
title: Report an http-test AIO drain past DRAIN-MS as a deadline
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T13:31:23.203122+03:00"
---

Problem: src/lib/net/http-test.f:2168 AIO-STOP retries AIO:STOP until DRAIN-MS (1000 ms, :2159) and then throws E-AIO-BUSY (src/lib/aio.f:1037). Under load the cancel completions arrive later than that, so the pool reads a failure instead of TIMEOUT-UNDER-LOAD, against docs/gate.md:428-441. Found in the review of the in-process wait deadline fix. Acceptance: a drain that outlasts DRAIN-MS ends the suite as a deadline the pool classes TIMEOUT-UNDER-LOAD and names E-AIO-BUSY; DRAIN-MS is not raised (carl's http ruling); a drain that completes behaves as now. Files: src/lib/net/http-test.f. Verify: a copy with DRAIN-MS set to 0 and the row run through the pool reads TIMEOUT-UNDER-LOAD; the http row rc 0. Depends: none. Ownership: src/lib/net/http-test.f. Claim: unassigned.
