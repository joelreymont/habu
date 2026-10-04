---
title: Refuse a malformed qualified name in CHECKER-DEFER
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T13:57:08.473840+02:00"
---

Problem (lane 307 r4-namelen): a name with two inner colons passed to CHECKER-DEFER appends a defer row for symbol 0 (measured DFER-END 1168 -> 1184 on the 4f2a202a engine): CHECKER-RECORD-SYM (src/core/checker.f ~:10686) answers 0 for it and DFER-ADD-SYM (~:11656) appends anyway. Acceptance: every checker entry that records a symbol refuses a name CHECKER-RECORD-SYM cannot record (answer 0) with its existing refusal and appends no row; a case for CHECKER-DEFER and each other caller of CHECKER-RECORD-SYM that appends, seen failing first. Files: src/core/checker.f, test/name-length-test.f.
