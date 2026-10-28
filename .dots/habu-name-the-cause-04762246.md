---
title: Name the cause of a failed spawn
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T12:53:47.739841+02:00"
---

Problem: a failed spawn throws a bare E-PROC-SPAWN and discards what the primitive knew. spawn-io answers the negated errno (src/lib/process-test.f:146-150 asserts -ENOENT on macOS), but PROC-SPAWN-IO (src/lib/process.f:283-286) and PROC-CAPTURE-ADOPT-SPAWN (src/lib/process.f:1064-1065) test only for a negative pid and throw, so neither the errno nor the program path survives; the comment at src/lib/process.f:55-58 states this. Observed in Maki 2026-10-28: test/drawing-entry.f:58 runs a missing native fixture through PROC-CMD:RUN-RC (src/lib/process-command.f:582), and the run died with an uncaught E-PROC-SPAWN that read as a resource failure; only rebuilding the fixture showed it was ENOENT. FS already solves the same problem with a task-local cause cell read right after the failure (FS:ERRNO, src/lib/fs.f:107-113, band in src/habu/layout.f:1993). Acceptance: after E-PROC-SPAWN, checked code can read the spawn's errno the way FS:ERRNO works (task-local, cleared on a semantic refusal), and an uncaught E-PROC-SPAWN from PROC-CMD's run words reports the program path and the errno name; regressions: a missing program answers ENOENT, a non-executable file EACCES, both through PROC-SPAWN-IO and the capture path. Files: src/lib/process.f, src/lib/process-command.f, src/habu/layout.f (band), src/lib/process-test.f, src/lib/process-cwd-test.f. Verify: src/lib/process-test.f; src/lib/process-cwd-test.f; test/run.f. Depends: none (process-command.f overlaps phil's cmdcore vmopvnro; sequence after it). Ownership: process library. Claim: unassigned.
