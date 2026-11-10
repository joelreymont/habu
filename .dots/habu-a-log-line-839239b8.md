---
title: A log line to a broken pipe kills the process
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T10:14:07.763429+02:00"
blocks:
  - habu-make-incremental-builds-41fd3214
---

Problem: src/lib/log.f:8-9 says a log call never throws for LOG's own failure and a line fd 2 refuses is lost. When fd 2 is a pipe whose reader has gone, LOG's write (log.f:148 ROW-OUT, log.f:156-160 OUT) raises SIGPIPE and the process dies with status 141 before the catch around the log call returns: case e4.f, rerun by the lead. FD:WRITE-FULL leaves signals to its caller (src/lib/fd.f:195-199), LOG takes no decision, and no layer of the runtime ignores SIGPIPE. A daemon whose stderr reader exits, or `hb … 2>&1 | head`, dies while reporting a failure. Source: the code review of package LOG Joel asked for on 2026-11-10 ("check that LOG:CODE+ is implemented correctly, and the whole LOG package"), report ~/Work/reviews/habu-10x-2026-11-10/log-review.md, cases ~/Work/reviews/habu-10x-2026-11-10/log-review/, run as `cd <installed Habu tree> && bin/hb --load <case>`. Lines are as of 92c8b01b. Acceptance: a line logged to a broken pipe on fd 2 is lost and the program goes on, as the header says; a test logs to a reader-less pipe on fd 2 and then runs its next statement. One layer owns SIGPIPE for that write. Which layer is the design: a process-wide disposition changes every Habu program, so it goes to Joel before a worker starts. Files: src/lib/log.f, src/lib/log-test.f, and the owning layer the design names. Verify: src/lib/log-test.f with the new case. Depends: habu-make-incremental-builds-41fd3214. Ownership: src/lib/log.f. Claim: unassigned.
