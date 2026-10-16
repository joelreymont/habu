---
title: Throw a browser host-test curl deadline as one
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T14:42:36.094171+03:00"
---

Problem: src/lib/browser/host-test.f PERFORM (:41-50) answers -2 for every failed transfer and drops the libcurl code, and SERVED (:76) asserts 200 T=, so a transfer that a starved process loses to a deadline fails an assertion instead of reaching the pool as TIMEOUT-UNDER-LOAD (docs/gate.md:428-441). Same defect class as 30f6e66b in src/lib/net/http-test.f TAKE-RESPONSE: libcurl code 28 (CURLE_OPERATION_TIMEDOUT) is the default 300 s connect timeout these handles meet when the process is held, and the server here also bounds the head with IDLE-MS 500 ms (:20). Found by checking every CURL:PERFORM caller after 30f6e66b; not yet seen failing in a gate. Acceptance: follow the shape 30f6e66b lands: every failed transfer names its libcurl code on stderr; a code that is a deadline is thrown as E-PROC-TIMEOUT; every other failure still answers -2 and asserts as now; no deadline is raised. Files: src/lib/browser/host-test.f. Verify: the row (gate-stdlib-cases.f:1661) rc 0; the 30f6e66b lldb hold at the first __connect for 320 s ends rc 67 with hb: uncaught throw code -2502 last. Depends: 30f6e66b. Ownership: src/lib/browser/host-test.f. Claim: unassigned.
