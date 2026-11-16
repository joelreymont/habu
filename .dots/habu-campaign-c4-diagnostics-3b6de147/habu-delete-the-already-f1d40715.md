---
title: Delete the already-reported machinery
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T16:00:42.384409+01:00"
---

Problem: REFUSAL-ABI:CODE-CELL (src/habu/layout.f, src/habu/data-claims.f, src/habu/process-fork.f, its compare in src/habu/habu2.f), REPORTED-ERROR?, CAST-ERROR:SKIP's list, PINNED?, JSON-REPORTED and the THREW? list tests exist only to guess whether a code was printed. The guess misses a rejection that renders its record and then throws its own code: tools/check.f prints a second, generic E-STATEMENT-THROW record after `: P:Q:R ( -- ) ;` (src/core/checker.f CHECKER-RECORD-NAME-AT, E-BAD-QUALIFIED-RECORD) and after an FFI record thrown through src/lib/ffi-abi.f HELP-THROW (tools/check-all-errors-core.f CHECK-ALL-ERRORS:THREW?); test/diag-position-test.f REJECTED-MALFORMED pins the second record. Acceptance: all are deleted; check.f prints one record for each of those rejections; every throw left in src/core, src/habu and src/compiler is a rejection, a 76 die or a control code with a named catcher in its package. Depends: none. Claim: unassigned.
