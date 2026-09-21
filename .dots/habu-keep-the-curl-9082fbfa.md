---
title: Keep the curl-http suite green under load
status: open
priority: 3
issue-type: task
created-at: "2026-09-21T13:56:12.416071+03:00"
---

Problem: suite curl-http (lib curl binding, live local HTTP fixture) went red in chain DM (load ~10, gate-DM.log line 264, pool-141661-203) and earlier in chain DJ's period with E-PROC-TIMEOUT, and is green alone and in chain DN (731 ms) every time: a fixed process timeout that a saturated pool exceeds. A gate that reds on load is not measuring the code. Acceptance: the fixture's wait is idle-based (no output for N s) or its budget is derived from the measured run under load, or the suite joins GROUP SEQ native-serial-gates like the PTY fixture (db3908ec) if it drives a child process pool; ten runs at load >= 10 green; assertions unchanged. Files: lib/curl-http-test.f or wherever test/gate-stdlib-cases.f registers curl-http (rg curl-http test/gate-stdlib-cases.f), test/gate-stdlib-cases.f only if regrouped. Verify: the suite ten times under a bounded load driver. Depends: none. Ownership: lib/tests (alder). Claim: unassigned.
