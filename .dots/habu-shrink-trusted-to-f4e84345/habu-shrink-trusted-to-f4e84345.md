---
title: "Shrink TRUSTED: to real boundaries"
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T09:27:22.486239+02:00"
---

Joel 2026-10-31: shrink TRUSTED: after the T0 release, not before. Today: 102 TRUSTED: definitions in src (27 files; src/lib/xml/c2.f 31, checker.f 8, verify-source.f 7, include.f 7), 16 in tools, 69 in test. docs/forth.md says never add TRUSTED: to bypass a missing checker model and keeps it for bodies the checker cannot model (evaluate, metaprogramming) behind a named, tested boundary; many uses are plain variable reads (checker.f FIELD-PROJ-NAME$) or xt fetches. Census every use; give each the checked form or the checker the model it lacks at its owner; keep only the uncheckable ones. t0.f TYPE-ENTRY/XT-STORE-ENTRY/DOES-PATCH-ENTRY duplicate elaborate.f's: one shared copy or none.

Batch 1 landed (habu-shrink-trusted-to-f4e84345, eight commits ending "Row the checker words the owner calls"): 75 TRUSTED: definitions became checked : or CAST: definitions; TRUSTED: definitions in src 102 -> 55, tools 16 -> 5, test 69 -> 52. The tools sites were the 11 REC-* writers in tools/native-unit-object.f.

The xt feature landed (habu-let-signatures-name-5931014b, habu-check-the-primitive-4b61908e): a signature names a ticked primitive's type as xt, so the tick-only entry words became checked. TRUSTED: definitions in src 55 -> 47 (elaborate.f 4 -> 0, t0.f 3 -> 0, select-x64.f 1 -> 0, trap.f 1 -> 0, dict.f 0 -> 1 for the one shared DOES-PATCH-ENTRY); tools 5, test 52 unchanged.

The checker verdict words landed (habu-publish-json-diags-116b6142): REPORT-UNCHECKABLE, MULTI-ERR-BEGIN and MULTI-ERR-END moved into checker.f: the TRUSTED: CHECK-BODY copy is deleted and MULTI-ERR-BEGIN is checked. TRUSTED: definitions in src 47 -> 45 (verify-source.f 7 -> 6, cell-effects.f 4 -> 3).

The C2 disposer landed (habu-check-the-c2-99fd69f8, on habu-pass-a-fresh-bb992e31): ALLOC-DISPOSE is checked through a C2-MEM callback pack. TRUSTED: definitions in src 45 -> 44 (c2-owner.f 1 -> 0); tools 5, test 52 unchanged.
