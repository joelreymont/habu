---
title: "Share the tests' stderr capture helpers"
status: open
priority: 3
issue-type: task
created-at: "2026-11-06T13:06:47.398867+02:00"
---

Problem: LOG-PATH, LOG-INIT, LOG$, LINES and CAPTURED, which run code under STDERR:CAPTURE and read the captured lines, are written four times: src/lib/net/model-test.f:874-901 and :2064, mcp-test.f:1031-1058, test-issuer-test.f:440-469, and a variant in log-test.f:66-85 (review of kpmxnwny); test-issuer-test's LINE$ lacks model-test's guard for an empty capture. Acceptance: one helper in src/lib/test/stderr.f (package STDERR, which owns fd-2 capture, errors.f:1213-1214) that the four tests call; names chosen by the lead (words only Habu's tests call); the copies go. Files: src/lib/test/stderr.f, the four tests. Verify: the four tests rc 0. Depends: habu-log-mcp-s-61343cb4. Ownership: carl. Claim: unassigned.


Merged from habu-share-the-tests-7a8f1972 ("Share the tests' stderr capture readers"), 2026-11-10:

Problem: three copies of the same capture-file readers (LOG-PATH, LOG-INIT, LOG$, LINES): src/lib/net/model-test.f:875-900, mcp-test.f:1031-1057, test-issuer-test.f:440-466. Acceptance: one copy in src/lib/test/stderr.f (package STDERR, beside CAPTURE and ;CAPTURE) that all three call; a new public STDERR word follows card section 1 and goes to Joel only if a user program would call it. Files: src/lib/test/stderr.f, the three tests. Verify: the three tests and src/lib/log-test.f. Depends: netlog and outage lanes landed (they edit two of the copies). Ownership: carl, worker lane. Claim: unassigned.
