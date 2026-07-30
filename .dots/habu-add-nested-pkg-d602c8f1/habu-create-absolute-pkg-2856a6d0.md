---
title: Create absolute package prefix nodes
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T21:36:54.696414+02:00"
---

Current package A:B exits 75; E1 must make absolute create/reopen real without deep lookup. package A:B:C creates or reuses package records for A, A:B, and A:B:C. DREC stays 48 bytes: the full case-preserving absolute path is identity under folded comparison; -1 remains the namespace marker; the existing min-input byte is kind (package=0, future type=1); package rows own nonzero public/private WIDs. Parent is derived by chopping the path; no stored parent, side table, new counter or log, format or version change, or compatibility state. Rollback remains CP/NDICT and WIDN stays monotonic. Restore raises WIDN above namespace roles; owner validators accept well-formed colon paths and reject leading, trailing, and double separators. Compact AOT EXT support is excluded because its proven capture range has no packages. Owner and write set: src/habu/layout.f, src/habu/habu2.f, src/habu/aot-capture.f, test/gate-dictionary-lib.f, and only necessary current trust-ledger rows. Exclude habu1.f, checker, bootstrap, declarers, nested blocks, deep or ancestor lookup, using, generic AOT work, and new lint. Acceptance at M17: a private word defined in A:B:C is called bare by a case-varied reopen; reopening prefixes allocates nothing; malformed paths publish nothing; a caught failure restores CP/NDICT; snapshot owner identity and namespace WIDs survive; restored WIDs cannot collide. Pre-M17 checkpoint: a failing real probe, the stdin AOT capture census, and representative typed, package, and trust diff gates.
