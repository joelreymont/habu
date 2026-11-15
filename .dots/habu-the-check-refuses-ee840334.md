---
title: The check refuses a STRUCTURE declared in a sealed package, which the load admits
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:33:18.399060+02:00"
---

Problem: a STRUCTURE declared inside a sealed package loads with rc 0, its MAKE and UNMAKE going into a new package of their own, while tools/check.f refuses it with E-SEALED-PACKAGE at the family name (src/habu/verify-source.f:2461 RECORD-STRUCTURE-DECL). Present before the seal lane. On the installed hb `package SS seal-package STRUCTURE pt FIELD x n ;STRUCTURE ;package` is refused by the load too (E-SEALED-PACKAGE "'PT-MAKE' is sealed", rc 84) and by tools/check.f at 'pt' (3:11, rc 70). Found by its review (finding 5). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the check admits what the load admits, and a test pins a STRUCTURE in a sealed package in both. Files: src/habu/verify-source.f, test/diag-position-test.f. Verify: such a file under --load and check.f. Depends: none. Ownership: the source pre-pass. Claim: unassigned.

2026-11-10 (bob): this defect is the quick check's alone (tools/check.f --verify-only, src/habu/verify-source.f), the scanner habu-check-src-through-1198f4ed replaces. It is fixed by that dot, not in the scanner, and is worked last in its section.
