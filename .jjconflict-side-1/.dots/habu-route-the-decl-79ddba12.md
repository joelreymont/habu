---
title: "Route the decl suites' rollback through TFAM-REWIND"
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T15:25:15.015911+02:00"
---

test/enum-decl-suite.f:91 and test/structure-decl-suite.f:74 each carry a TRUSTED REG-RESTORE that writes TFAM-N, TF-STR-U, TF-PK-N, SUMV-N and LAY-N directly - the same shape src/core/sumtype.f's TDECL-RESTORE carried before habu-retire-tfx-and-a2d767da replaced it with the registry-owned TFAM-REWIND, which retires the tail index (TFX) and the constructor-symbol index (SVX) before the counters move. Both suites pass today because nothing in them stamps an index watermark down afterwards, so TFX-ENSURE still rebuilds; the anti-pattern is latent, not live, and the new TFX-RETIRE guard does not fire on either (checked). Route both through TFAM-REWIND so there is one rewind in the tree. It was left out of the fix commit because the package lint requires a changed module word to be package-owned, and REG-MARK/REG-RESTORE plus their nine RB-* cells are legacy globals with about six call sites each per file: the move is a small package extraction (package REG-SNAP, public MARK/RESTORE, qualified call sites), unrelated to the registry defect, and better reviewed on its own.
