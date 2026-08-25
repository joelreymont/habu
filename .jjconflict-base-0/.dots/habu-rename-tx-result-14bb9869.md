---
title: Rename tx-result family to drop owner prefix
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T14:43:29.610446+02:00"
close-reason: "Landed as 534b02c5e794."
---

R3-class rename surfaced by the C12 migration lane: family tail tx-result inside package TX violates the redundant-owner-prefix rule (E-REDUNDANT-PACKAGE-PREFIX, pre-existing - the lint fires only on added name lines, and the migration rewrite is what puts the declaration in front of it). Rename the family tail to result: generated constructor package becomes TX-RESULT (13 bytes, clear of the cliff), (package, tail) identity disambiguates from the global result family per R7. Cascade, all in one commit: 23 occurrences across maki/db/transaction.f (15), transaction-test.f (4), commit-store.f (1), keywire-xproc-env-child.f (3) - every TX:tx-result effect spelling and every TX-TX--RESULT: constructor call site; the cross-process child must be re-verified standalone AND through its parent suite keywire-xproc-env-test.f. Spelling migration proven by calibrated verdict tables: old spellings unresolvable after, new spellings accepted with the identical effect table. Acceptance: package-diff-lint accepts the declaration line (the exact probe that failed); all four suites plus the child standalone green; maki/test.f green. Owner: package TX.
