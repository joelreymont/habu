---
title: GE-RUN words take a timeout every caller passes as forever
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T20:51:23.673071+01:00"
---

Problem: test/gate-common-lib.f:114-148 GE-RUN-* take a timeout ms, and after the waits campaign every caller passes GE-TIMEOUT-MS, which no longer bounds anything (test/debugger-resume.f:39, test/engine-stack-machine.f:44, :91, :98, test/gate-debug-lib.f:103 and others). Acceptance: GE-RUN-* take no timeout and wait with no deadline; GE-TIMEOUT-MS is deleted; the suites loading gate-common-lib pass. Depends: the waits campaign commit. Claim: unassigned.
