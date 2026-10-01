---
title: Let tools/check.f read a constant buffer count
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T23:47:27.361550+03:00"
---

Problem: the source preverifier reads a `TYPED-BUFFER` count from the text with `CHECKER-LBUF-COUNT?` (src/core/checker.f), decimal digits only, so `ARG-CAP TYPED-BUFFER ARGV …` (src/habu/kernel-hir-x64.f) and `VOCAB TYPED-BUFFER …` (src/compiler/native/x64ir.f) refuse as E-CHECKER-LAYOUT-BUFFER (7121), and `bin/hb --load tools/check.f -- src/habu/link-x64.f` exits 67 through them. src/habu/verify-source.f (the BOTH TABLES note) works around it with create/allot; a decimal literal would state each capacity twice.
Acceptance: a `TYPED-BUFFER` count named by a constant the candidate scope defines certifies with that constant's value (still positive and within `LBUF-COUNT-MAX`); an unknown or non-constant token still refuses 7121. tools/check.f accepts kernel-hir-x64.f, x64ir.f and link-x64.f. Tests: accepted and refused counts through tools/check.f.
Files: src/core/checker.f, src/habu/verify-source.f (count reader and its note), a check test.
Verify: the check test; tools/check.f on the three files; gate.
Depends: none.
Ownership: krait.
Claim: unassigned.
