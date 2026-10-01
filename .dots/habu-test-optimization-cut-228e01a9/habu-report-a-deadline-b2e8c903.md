---
title: Report a deadline as a timeout in PCMDT-RUN-YES-TRUNCATED
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T12:03:31.909562+02:00"
---

Problem: lib/process-command-test.f:126 PCMDT-RUN-YES-TRUNCATED, checked at :393 by TTHROWSQ E-PROC-TRUNCATED, runs under a 1000 ms deadline. Under load the deadline can fire before truncation; since r4-wblabel f2d76c1c RUN-RC then throws E-PROC-TIMEOUT, which TTHROWSQ reports as a wrong throw code (a plain failure) instead of letting the pool label it TIMEOUT-UNDER-LOAD. Found by the r4-wblabel c3 worker. Acceptance: with the deadline forced to 1 ms the row ends as an uncaught E-PROC-TIMEOUT (pool kind TIMEOUT-UNDER-LOAD), seen as kind=exit first; a real truncation still passes; a wrong non-timeout code still fails. Files: lib/process-command-test.f (and the shared throw-check word only if the rule belongs there for every deadline case). Verify: the row alone through test/gate-pool.f.
