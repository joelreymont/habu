---
title: "Shrink TRUSTED: to real boundaries"
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T09:27:22.486239+02:00"
---

Joel 2026-10-31: shrink TRUSTED: after the T0 release, not before. Today: 102 TRUSTED: definitions in src (27 files; src/lib/xml/c2.f 31, checker.f 8, verify-source.f 7, include.f 7), 16 in tools, 69 in test. docs/forth.md says never add TRUSTED: to bypass a missing checker model and keeps it for bodies the checker cannot model (evaluate, metaprogramming) behind a named, tested boundary; many uses are plain variable reads (checker.f FIELD-PROJ-NAME$) or xt fetches. Census every use; give each the checked form or the checker the model it lacks at its owner; keep only the uncheckable ones. t0.f TYPE-ENTRY/XT-STORE-ENTRY/DOES-PATCH-ENTRY duplicate elaborate.f's: one shared copy or none.

Batch 1 landed (habu-shrink-trusted-to-f4e84345, eight commits ending "Row the checker words the owner calls"): 75 TRUSTED: definitions became checked : or CAST: definitions; TRUSTED: definitions in src 102 -> 55, tools 16 -> 5, test 69 -> 52. The tools sites were the 11 REC-* writers in tools/native-unit-object.f.
