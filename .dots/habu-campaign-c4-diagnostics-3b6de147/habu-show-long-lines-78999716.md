---
title: Show long lines in a record as a window
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T11:44:17.983557+01:00"
closed-at: "2026-11-16T04:07:34.040802+01:00"
---

Problem: a record prints its source line, caret line and notes in full: test/signature-nesting.f's 9000-pointee case prints a 36KB line three times (108KB), more than an agent can read, and the suite's stderr buffer was raised to hold it. Acceptance: a source or evaluated line longer than 140 bytes shows a window of 140 around the token with ... at each cut end and the carets aligned; a note quoting program text shows at most one line of 137 bytes and ...; signature-nesting passes with its stderr buffer back at CAP. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
