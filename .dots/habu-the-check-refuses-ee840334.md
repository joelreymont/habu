---
title: The check refuses a STRUCTURE declared in a sealed package, which the load admits
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:33:18.399060+02:00"
---

Problem: a STRUCTURE (by reading also a PRODUCT) declared inside a sealed package loads with rc 0, its MAKE and UNMAKE going into a new package of their own, while tools/check.f refuses it with E-SEALED-PACKAGE at the family name (src/habu/verify-source.f:2618 RECORD-STRUCTURE-DECL, :2643 RECORD-PRODUCT). Present before the seal lane. Found by its review (/home/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/seal-review.md, finding 5; p/in-struct.f). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the check admits what the load admits, and a test pins a STRUCTURE and a PRODUCT in a sealed package in both. Files: src/habu/verify-source.f, test/diag-position-test.f. Verify: in-struct.f under --load and check.f. Depends: none. Ownership: the source pre-pass. Claim: unassigned.

2026-11-10 (bob): this defect is the quick check's alone (tools/check.f --verify-only, src/habu/verify-source.f), the scanner habu-check-src-through-1198f4ed replaces. It is fixed by that dot, not in the scanner, and is worked last in its section.
