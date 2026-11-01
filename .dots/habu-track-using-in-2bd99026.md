---
title: "Track using in check.f's package scan"
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T05:27:43.964403+02:00"
---

Problem: tools/check-core.f's package scan (CHK-PKG-STEP) runs no package word and does not track `using`, so CHECKER-RESOLVE:PART-GUARD, called with RECORD true from CHK-PKG-OPEN, cannot see a part opened under a using its package opened, nor the buffer; the guard reads only the checker's record (section, inside a part). A part opened under its package's own using (`package X public : XW ( -- n ) 1 ; ;package package A using X package B deftype N deftype N ;package ;using ;package`, its lines joined by spaces) gets 'check.f: bad nominal type' from check.f instead of the E-PKG-CONTEXT refusal the Habu loop gives. Acceptance: the scan keeps the using depth and floor the guard reads, and PART-GUARD and PART-PATH lose their RECORD flag if the scan then keeps every condition; check.f refuses that program with E-PKG-CONTEXT in both its modes (fails before). Files: tools/check-core.f (CHK-PKG-STEP, CHK-PKG-OPEN), src/core/checker.f (CHECKER-RESOLVE). Verify: rows check-verify, checker-verify-pkg-scope, using-import. Depends: habu-open-a-pkg-6e585334 landed. Claim: unassigned.
