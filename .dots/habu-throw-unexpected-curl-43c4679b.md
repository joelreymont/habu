---
title: Throw unexpected curl-test timeouts as deadlines
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T14:42:36.109030+03:00"
---

Problem: src/lib/net/curl-test.f sets its own transfer limits (REQUEST-MS 10 s, :52, applied with CURL:TIMEOUT! at :934, :1229, :2079 and others) and FETCH (:834-841) records a failed transfer's code; the cases that expect a response then assert LAST-KIND @ KIND-RESPONSE T= (:953 onward), so a limit that expires on a starved process (libcurl code 28) fails an assertion. docs/gate.md:428-441: a test's own deadline is no assertion result, and a test that expects the deadline asserts it, as the code-28 cases at :1132, :1162, :1204, :1435, :1538 and :1723 do. The stream path (NEXT/AWAIT results, :1359) has the same shape. Same class as 30f6e66b; found by checking every CURL:PERFORM caller; not yet seen failing in a gate. Acceptance: where a case expects anything but a timeout and libcurl answers 28, the row names it and throws E-PROC-TIMEOUT; the cases that expect 28 assert it as now; no deadline is raised. Files: src/lib/net/curl-test.f. Verify: the row rc 0; a copy whose expected-response transfer gets a 1 ms limit exits 67 with hb: uncaught throw code -2502 last; a copy whose expected-28 case gets no limit still fails as an assertion. Depends: none. Ownership: src/lib/net/curl-test.f. Claim: unassigned.
