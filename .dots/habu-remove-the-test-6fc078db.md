---
title: Remove the test timers at their root causes
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T23:42:12.860649+02:00"
---

Problem: tests and the gate end work on timers instead of on the events they wait for, and under load a timer expires on work that was not hung; no test timer has ever caught a real hang. Joel 2026-10-29: 'none of the tests should timeout or even be timed'; 'always address the cause and not a symptom! why are we launching a child? why would it be hanging at all? how can we make sure it doesn't hang under any circumstances?'; 'why do we need a test where we have to artificially restrict it in case it hangs?'. He refused a hang-policy framing and asked for root causes. Joel's rulings: drop `ms` from every capture word (design C1); no per-row bound of any kind (the WAIT line with the CPU trend plus SIGINT names a hang); "no build timeouts"; one staged PROCESS word sets the LSP's check child's bound and nothing else. The design's lanes: 1 capture-exit, 2 pool-exit, 3 capture-no-deadline, 4 waits-on-events, 5 fork-held-refusal, 6 product-bounds. Lanes 2 and 6 landed (603615e29b "End pool rows on exit, drop pool clocks", 0b00fee1b0 "Delete PTX-BARRIER!"); PROC-WAIT-BOUNDED and HB_BUILD_TIMEOUT_MS are gone. What is left of this dot is the end proof once the four open lanes land.
Acceptance: the four lane dots are closed; a full gate at eight slots under a second load ends no row on a timer, and no timer, budget or deadline is left in src/lib/test, test/gate-*.f, tools/*test-lib.f or the builds except the staged LSP bound.
Files: none beyond the lanes'.
Verify: rg for the lanes' timer names prints only the staged LSP bound; a full gate at eight slots under a second load shows no row ended by a timer.
Depends: habu-end-a-capture-ce0c5a71 (lane 1), habu-drop-the-ms-09b50524 (lane 3), habu-end-test-waits-12de56df (lane 4), habu-refuse-a-facility-7a3fc2ad (lane 5).
Ownership: unassigned. Claim: unassigned.
