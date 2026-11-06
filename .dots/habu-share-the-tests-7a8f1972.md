---
title: "Share the tests' stderr capture readers"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T15:06:36.239543+02:00"
---

Problem: three copies of the same capture-file readers (LOG-PATH, LOG-INIT, LOG$, LINES): src/lib/net/model-test.f:875-900, mcp-test.f:1031-1057, test-issuer-test.f:440-466. Acceptance: one copy in src/lib/test/stderr.f (package STDERR, beside CAPTURE and ;CAPTURE) that all three call; a new public STDERR word follows card section 1 and goes to Joel only if a user program would call it. Files: src/lib/test/stderr.f, the three tests. Verify: the three tests and src/lib/log-test.f. Depends: netlog and outage lanes landed (they edit two of the copies). Ownership: carl, worker lane. Claim: unassigned.
