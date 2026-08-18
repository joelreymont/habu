---
title: "Destruction-review minors: five small hardenings"
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T19:43:25.683644+02:00"
---

From the 2026-08-18 review of record, one lane: (1) REG-EXT-BND-SAVE/RESTORE-XT default to silent NOOP - a MARK before INSTALL-BOUND records a boundary missing every type-registry counter and CURSORS cannot see it (=20 either way); the BND pair has NO legitimate pre-install caller (unlike the scope pair) so its default becomes die-by-name (checker.f:12487). (2) CHECK-RETRY is not throw-safe: a throw inside CHECK leaves ARMED/RESCAN stale - in a surviving REPL, a muted tape observer and a suppressed diagnostic (checker.f:12936; the candidate path got the snapshot, the top-level path did not - give it the same). (3) PFX-MARK re-enumerates the seven TFAM counters TF-SAVE also enumerates - the drift class one level down; derive one from the other or assert equality (type-family.f:2274 vs :2189). (4) BF-BUILD-STDIN is dead code - no caller tree-wide (build-fixpoint.f:1595); delete. (5) The 'before a byte is emitted' comment overclaims on cached verbs - BF-STAMP-KEY! emits sources before BF-PREFLIGHT there; fix the sentence to what the probe actually guards (the stage compile) (build-fixpoint.f:412). Each with its mutation or proof; one gated commit each or grouped sensibly.
