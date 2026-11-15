---
title: Waits with no deadline still answer a timeout
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T20:51:23.734400+01:00"
---

Problem: a wait given no deadline keeps a timeout result its callers must match and cannot reach: fs-mutate REFUSED's timeout arm, SIGNAL:WAIT -1 (signal-test WANT-SIGNAL), TCP4:READABLE-WITHIN? -1's idle arm (test/http-accept-starved.f ANSWER$), and libcurl's 300 s connect default answering CURLE 28 into TEST-WAIT:EXPIRED (src/lib/net/http-test.f:751, src/lib/browser/host-test.f:60). Goal: a wait with no deadline has no timeout answer, so no caller writes an unreachable arm, and TEST-WAIT:EXPIRED goes once nothing calls it. Acceptance: a settled design, then its lanes. Depends: the waits campaign commit. Claim: unassigned.
