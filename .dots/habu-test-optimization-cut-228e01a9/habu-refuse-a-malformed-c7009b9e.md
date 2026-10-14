---
title: Refuse a malformed qualified name in CHECKER-DEFER
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-02T13:57:08.473840+02:00\""
closed-at: "2026-10-14T19:46:59.197628+03:00"
close-reason: c75ff6d0 adds CHECKER-RECORD-NAME-AT before symbol writers; current CHECKER-DEFER calls it and name-length-test.f tests malformed names with no row publication.
---

Problem (lane 307 r4-namelen): a name with two inner colons passed to CHECKER-DEFER appends a defer row for symbol 0 (measured DFER-END 1168 -> 1184 on the 4f2a202a engine): CHECKER-RECORD-SYM (src/core/checker.f ~:10686) answers 0 for it and DFER-ADD-SYM (~:11656) appends anyway. Acceptance: every checker entry that records a symbol refuses a name CHECKER-RECORD-SYM cannot record (answer 0) with its existing refusal and appends no row; a case for CHECKER-DEFER and each other caller of CHECKER-RECORD-SYM that appends, seen failing first. Files: src/core/checker.f, test/name-length-test.f.
