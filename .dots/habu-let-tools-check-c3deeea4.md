---
title: Let tools/check.f read a constant buffer count
status: closed
priority: 2
issue-type: task
created-at: "2026-10-01T23:47:27.361550+03:00"
closed-at: "2026-10-02T10:44:24.747486+03:00"
close-reason: "delivered: the checker certifies a named TYPED-BUFFER or LAYOUT-BUFFER count by its effect ( -- n ) (CHECKER-LBUF:CERTIFY) and the load bounds the value. With the rebuilt engine (sha256 21eba36d), tools/check.f exits 0 on link-x64.f, elf.f, kernel-hir-x64.f and x64ir.f (67 on the tip engine); tools/check-test.f case check/layout-buffer-count passes (its 15 asserts on the accepted and zero-count sources fail on the tip engine); verify-source.f holds DEFINER-SYM as a TYPED-BUFFER and verify-source-self certifies it."
---

Problem: the source preverifier reads a `TYPED-BUFFER` count from the text with `CHECKER-LBUF-COUNT?` (src/core/checker.f), decimal digits only, so `ARG-CAP TYPED-BUFFER ARGV …` (src/habu/kernel-hir-x64.f) and `VOCAB TYPED-BUFFER …` (src/compiler/native/x64ir.f) refuse as E-CHECKER-LAYOUT-BUFFER (7121), and `bin/hb --load tools/check.f -- src/habu/link-x64.f` exits 67 through them. src/habu/verify-source.f (the BOTH TABLES note) works around it with create/allot; a decimal literal would state each capacity twice.
Acceptance: a `TYPED-BUFFER` count named by a constant the candidate scope defines certifies with that constant's value (still positive and within `LBUF-COUNT-MAX`); an unknown or non-constant token still refuses 7121. tools/check.f accepts kernel-hir-x64.f, x64ir.f and link-x64.f. Tests: accepted and refused counts through tools/check.f. Correction (lead, 2026-10-02): a count named by a word of effect ( -- n ) certifies by its effect, without its value; the definer bounds the value at load (E-LAYOUT-BUFFER).
Files: src/core/checker.f, src/habu/verify-source.f (count reader and its note), a check test.
Verify: the check test; tools/check.f on the three files; gate.
Depends: none.
Ownership: krait.
Claim: unassigned.
