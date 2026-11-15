---
title: Place a nameless STRUCTURE or ENUM at its keyword
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T23:18:47.666431+01:00"
---

Problem: a file holding only 'STRUCTURE' or 'ENUM' is rejected by the load at 2:1, past the text (src/core/structure-decl.f DRIVE, src/core/enum-decl.f DRIVE), while tools/check.f gives E-MISSING-NAME at the keyword. Acceptance: the load and check.f print one record at the keyword with a help naming it, as NEWTYPE, SUMTYPE and VARIANT do. Depends: none. Claim: unassigned.
