---
title: Report a storage name too long for the checker
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T17:59:42.819956+02:00"
---

Problem (inferred from reading by the A2b fold-in re-review, not run): src/habu/verify-source.f SCAN-STORAGE-NAME passes a NEXT-RAW token, which has no length limit, to src/core/checker.f CHECKER-LBUF-NAME-OK?; a name longer than CK-NAME-MAX (7999 bytes) fails its CK-NAME-SPAN? test, which answers false with no error record, and the declaration is dropped silently. Acceptance: first confirm with an 8000-byte TYPED-VARIABLE name through tools/check.f --json-errors; if it holds, the scan reports the name with a named error (an existing code if one fits) and a test case shows it. Files: src/habu/verify-source.f, src/core/checker.f, a rejected-program test (test/gate-diagnostics-lib.f went with the test prune). Verify: that case. Depends: none. Ownership: those files. Claim: unassigned.
The load path too (inferred by the next re-review, not run): src/core/layout-buffer.f STORAGE-LINE-TOKEN takes the name with parse-name, which can return a token longer than CK-NAME-MAX, so CHECKER-LBUF-NAME-OK? answers false with no report there as well, the definer defines nothing, and LBUF-NAME-OK?'s comment ("the checker has reported") is false for that input.
