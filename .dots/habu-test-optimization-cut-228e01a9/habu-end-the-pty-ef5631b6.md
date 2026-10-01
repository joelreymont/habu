---
title: "End the pty tty smoke test's deadlines as timeouts"
status: closed
priority: 3
issue-type: task
created-at: "\"2026-10-01T17:17:49.794496+02:00\""
closed-at: "2026-10-01T18:39:56.150881+02:00"
close-reason: Fixed by kzsypkvv 05583da9 (review 217; lead doc, comment and message fixes it spelled out)
---

Problem: test/process-pty-tty-smoke.f:63 EXPECT-AFTER and :128 `EXIT-MS PROCESS-PTY:AWAIT TTRUE` read an expired deadline as a failed assertion, so a loaded host reports a defect instead of TIMEOUT-UNDER-LOAD (review 167). Acceptance: catch at the case boundary, PROCESS-PTY:TEARDOWN, AIO:STOP, print WAIT-FAILED's diagnostic to stdout, rethrow E-PROC-TIMEOUT so 'hb: uncaught throw code -2502' stays the last stderr line; a forced short deadline shows the pool label before failing as an assertion; a real assertion failure still fails. Files: test/process-pty-tty-smoke.f. Verify: the row alone rc 0; the forced-deadline probe rc 67 with the label.
