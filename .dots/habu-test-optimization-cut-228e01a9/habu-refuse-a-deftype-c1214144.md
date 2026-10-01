---
title: Refuse a DEFTYPE name the signature grammar owns
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T07:36:04.579916+02:00\""
closed-at: "2026-10-01T17:39:58.700700+02:00"
close-reason: Fixed by szrolmvv 22adadb6 (review 131 REVISE, doc fixed by the lead)
---

Problem: `package P DEFTYPE PTR ;package` passes the declaration (TDECL-RESERVED? in src/core/sumtype.f does not list ptr; the TFAM-DECL dup check is same-package only), then the loader dies at MINT's CAST: step: `>PTR:  n -- ptr : checker: bad stored signature`, rc 76 (src/core/checker.f:7730 USIG-ADD-BAD). tools/check.f --json-errors dies the same way with no JSON record (before r4-tic6x it gave E-BAD-NOMINAL-TYPE rc 70). Top-level `DEFTYPE PTR` is refused properly. Measured by review 63 on zqykzzvl 989d40c0; among probed tails only ptr shows it (n, bool, cell, i64, u8, idx, len, fd, rc, a, field, str, pid refused with a packet; xt, res, atom, opt, vec, list, slice, arr, map, fam, pair, box, ref, handle, option, result, chan accepted by both). Acceptance: the shared DEFTYPE name rule (TDECL-RESERVED? / CHECKER-DEFFAMILY, which check.f already calls) refuses every tail the signature grammar reads as something other than the family, so loader and check.f refuse it at the declaration as E-BAD-NOMINAL-TYPE (prose and JSON), never at CAST:; derive the set from the signature parser, not a probe list; a legal in-package shadow (DEFTYPE SIDE) stays admitted. Cases first through tools/check-test-lib.f and the deftype suite, seen to fail. Files: src/core/sumtype.f (baked: rebuild and converge), tools/check-test-lib.f, docs/effects.md if it states the rule. Verify: the cases, tools/check-test.f, deftype-suite, type-family-suite (whitebox), g1/g2 cmp, two-generation build. Ownership: DEFTYPE name rule.
