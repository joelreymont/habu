---
title: "PROCESS:WAIT on a pid at or below 0 reaps any child"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T23:42:26.276433+02:00"
---

Problem: PROCESS:WAIT (src/lib/process.f WAIT-RAW, near :218 after the WAIT feature) passes a pid <= 0 straight to wait4, which waits for any child or a process group; NO-PID is -1, so a caller that casts an unset pid cell waits for and reaps some other child. WAIT-BOUNDED refuses such a pid (:399); process-fork.f (CHECKED) and pty-harness.f WAIT-EXIT guard it themselves. Found by the WAIT feature review. Reproduced: `caffeinate -i bin/hb --load src/lib/process-test.f` hangs in TEST-WAIT-BAD at 0.29 s CPU, because `caffeinate -i cmd` runs cmd in its own pid with caffeinate as cmd's child (checked with ps), so wait4(-1) blocks on caffeinate instead of failing with ECHILD; run without caffeinate the file passes. Maki's NO-CHILD would hang the same way in any process with a live child (inferred, not run). Callers that rely on wait(-1) today: Maki test/kernel.f NO-CHILD and kernel-pipe-test.f:63 assert E-PROC-WAIT from -1 >PID to prove no child is left; Habu process-test.f TEST-WAIT-BAD. Acceptance: a wait names one child or refuses: WAIT on pid <= 0 throws E-PROC-WAIT, and a caller that needs 'no child is left' has a word that says so, so Maki's NO-CHILD keeps its power. Files: src/lib/process.f and those callers. Verify: a probe with a second child alive while WAIT gets -1 (today it reaps the second child). Depends: habu-give-a-wait-81260550 landed. Ownership: carl. Claim: unassigned. Consumers: none meets it today; waits behind the consumer-met work (AGENTS.md).
