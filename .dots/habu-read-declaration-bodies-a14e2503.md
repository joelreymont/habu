---
title: "Read declaration bodies' strings as the engine does"
status: open
priority: 2
issue-type: task
created-at: "2026-11-02T16:17:28.147718+02:00"
---

Problem (P2 lane review, 2026-11-02): src/habu/verify-source.f's shared reader NEXT skips a string literal wherever it runs, including the declaration-body loops that use NEXT-SCAN (RECORD-SUMTYPE-BODY, RECORD-PRODUCT, RECORD-VALUE-RECORD, RECORD-STRUCTURE), while the engine reads those bodies raw. So `package CKT-SU SUMTYPE optq VARIANT absent ;VARIANT VARIANT present s\" x\" n ;VARIANT ;SUMTYPE ;package` passes `tools/check.f --verify-only` (rc 0) while the load refuses it (rc 70, unknown payload type); probe /tmp/carl-pkg2-rv/tmp/su2.f on master's engine. PRODUCT and VALUE-RECORD share the reader, unprobed. Acceptance: --verify-only refuses every declaration body the load refuses for a token the scan skipped, with the load's error code where one maps (E-BAD-DECLARATION for su2.f); a check-verify test row per declaration kind that fails before. No public word added or changed. Files: src/habu/verify-source.f, tools/check-verify-test-lib.f. Verify: hb --load tools/check-verify-test.f rc 0. Depends: none. Ownership: carl. Claim: unassigned.
