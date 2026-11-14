---
title: "Keep a declaration's name across stdin lines"
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T19:58:43.281045+01:00"
---

Problem: TDECL-SUMTYPE and TDECL-PRODUCT (src/core/sumtype.f, lane carl-decl :2462, :2498) keep the family name as a pointer into the input buffer (TDN-A :110) while SUMTYPE-COLLECT / PRODUCT-COLLECT read later lines; from stdin, where the buffer refills line by line, a declaration spanning lines then reads a stale name (seen in a rejection record by the carl-decl worker; whether registration also takes the stale bytes is unverified). Acceptance: a SUMTYPE and a PRODUCT whose body spans lines, read from stdin, register and report under their own name, as a test. Depends: none. Claim: unassigned.
