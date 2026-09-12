---
title: Close the wide-effect launder through an untyped store and interpret execute
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T13:16:25.178649+03:00"
---

Problem: with TRUSTED: GE-WMK ( -- gewide<n,n> ), a colon body's ['] GE-WMK stored in a variable and executed at interpret level lands a two-cell bundle on the untyped interpret stack (measured 2026-09-12: depth 2, values 9 7, rc 0), which LWIDE exists to prevent; the DNAME-INT half was closed in the compiled tick (habu2.f C-BTICK) but the DNAME-WIDE half cannot be, because : F ( -- gewide<n,n> ) ['] GE-WMK execute ; is legitimate checked code. The leak is the untyped store of a typed quotation plus interpret-mode execute (checked code already refuses E-EXEC-OPAQUE-XT). Acceptance: one of: interpret-level execute gated on the target record's wide flag, or the untyped store of a typed quotation refused; stated in docs/effects.md; a regression that fails on the old engine. Files: src/habu/habu2.f or habu1.f (execute), src/core/checker.f if the store side, test/internal-word-gate.f. Verify: the suite and test/run.f on a rebuilt engine. Depends: none. Ownership: hazel. Claim: unassigned.
