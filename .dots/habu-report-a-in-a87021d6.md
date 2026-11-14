---
title: Test check.f's report of a comment in a declaration body
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T00:30:22.317627+02:00"
---

Problem: tools/check.f reports a `(` or `\` comment in an ENUM or STRUCTURE body as one E-BAD-DECLARATION packet, rc 70 (on the installed hb: 'ENUM kind VARIANT first ;VARIANT ( c ) ;ENUM', 'STRUCTURE pt ( c ) FIELD x n ;STRUCTURE', and a `\` after ;VARIANT), but no test pins these forms: test/diag-position-test.f's E-BAD-DECLARATION cases (:771-808) cover names and missing ends only. Acceptance: a refused-program case per form in test/diag-position-test.f (REFUSED-MODES :680); moot if habu-accept-comments-inside-145f82eb makes bodies accept comments. Files: test/diag-position-test.f. Verify: that test. Depends: incremental builds (Joel 2026-11-04: the check tool's work waits) and habu-accept-comments-inside-145f82eb. Ownership: the check tool's declaration replay. Claim: unassigned.
