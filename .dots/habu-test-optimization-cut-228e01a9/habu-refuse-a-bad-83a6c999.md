---
title: Refuse a bad definer row after undefine
status: open
priority: 3
issue-type: task
created-at: "2026-10-04T02:34:47.716120+03:00"
---

Problem: under tools/check.f --all-errors, a non-checked definer with a bad signature that reuses a name freed by undefine is swallowed: `: X ( -- n ) 1 ;` / `undefine X` / `defer X ( -- nosuch )` / `: Y ( -- n ) 1 2 ;` exits 70 with only Y's E-MISMATCH record, while the same defer under a fresh name gives E-BAD-STORED-SIGNATURE first. USIG-ADD-BAD's guard (src/core/checker.f ~:9052 in batch 4d) compares the row against NMA/NMU, which NAME-TOK sets at the : definition and nothing clears on undefine. Found by review 560 (fixtures $HOME/.cache/tmp/kestrel-r4-rev560/fx/undef-defer.f, defer-fresh.f, undef-defer-noy.f). Fix: replace the name comparison with a structural mark of the publish tail's re-add (for example clear NMA/NMU once the re-add is done, or flag the re-add itself), so only the definition's own re-add is skipped. Acceptance: undef-defer.f gives E-BAD-STORED-SIGNATURE then E-MISMATCH, rc 70; type-decl-suite's TDSME1/TDSME2 still count 2; baked: rebuild, g1 == g2, two-generation build. After: batch 4d on master.
