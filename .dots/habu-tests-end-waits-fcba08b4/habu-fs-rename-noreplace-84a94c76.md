---
title: fs-rename-noreplace-test ends its race wait by a clock
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T20:51:23.683918+01:00"
closed-at: "2026-11-13T21:29:14.025281+01:00"
---

Problem: src/lib/fs-rename-noreplace-test.f RACE-WAIT (:81-88) throws E-PROC-TIMEOUT after RACE-WAIT-NS (:23, 5 s) while the two racers meet. Acceptance: the racers meet with no deadline (test/AGENTS.md: a hang is named by its WAIT line); RACE-WAIT-NS is deleted; the suite holding the file passes. Depends: none. Claim: unassigned.
