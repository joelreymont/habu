---
title: Re-port TFAM 13 variant/tag onto master TERM-FAM diagnostic
status: open
priority: 2
issue-type: task
created-at: "2026-07-10T17:42:16.280228+02:00"
---

master's c41afe48 'Attribute diagnostics to failed types' reworked the ADT family diagnostic onto a TERM-FAM/U-FAIL mechanism and carries only the 'family' field (docs/repair-diagnostics.md), NOT variant/tag. My parallel TFAM 13 impl (abandoned commit be6b5433: DVAR/CVLIVE variant capture at CHECKER-STEP checker.f:1416 beside DEXP/DACT, SV-DVAR paired in TRIAL-SAVE/REST, CONSTRUCT-TOK latch/clear around CONSTRUCT-STEP-XT, render DIAG-VARIANT emitting variant+tag, red-first fixture GDX-ADT-VARIANT proving tag tracks the actual arm) was built on the now-replaced DIAG-FAMILY/ROW-FAM path, so it can't rebase. Re-implement the variant/tag capture on master's U-FAIL/TERM-FAM diagnostic path: capture the failed sum-variant SUMV id + tag at the U-FAIL point, emit variant/tag in master's render diag, add the GDX-ADT-VARIANT red-first fixture + repair-diagnostics.md rows. Reference impl in abandoned be6b5433 (recoverable via jj op log if needed). Chain continues: payload-pos -> arity per habu-tfam-13-repair-2d8488c5. tfam lane, src/core.
