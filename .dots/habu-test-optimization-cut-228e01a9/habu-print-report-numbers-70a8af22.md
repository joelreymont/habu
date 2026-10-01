---
title: Print report numbers inline everywhere
status: open
priority: 4
issue-type: task
created-at: "2026-10-01T17:44:47.838341+02:00"
---

Problem: `.` ends each number with a newline in this engine, so report lines that print a number mid-line with `.` split: tools/check-repair-hints-test.f:200-204 (expected exit, code, stdout/stderr bytes), tools/launch-context.f:65 (child rc), test/gate-common-lib.f:94 (spawn raw code), test/gate-pool.f:1384 (pool outcome code), tools/hb-build-test.f:133 and tools/hb-build-test-lib.f:431 (`rcn . cr`; review 201). The stats lane (dot 9e543840, b28e2cf5) fixed GE-PRINT-OUTCOME and GE-PRINT-CAPTURE-STATS with FMT:.INT (signed, never throws, unlike GT-U-TYPE which throws E-TBL-FIELD on a negative) and found these. Acceptance: every report line in tools/ and test/ that prints a number followed by more text on the same line uses FMT:.INT (census by search, listed in the commit); a forced-failure probe for each changed report shows one line before/after; the touched rows pass. Base: after 9e543840. Files: those three and any the census finds.
