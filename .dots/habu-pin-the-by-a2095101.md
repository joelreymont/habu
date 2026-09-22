---
title: Pin the by-name refusal of a stripped ptr-cell-mark call
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T06:52:10.475088+03:00"
---

Problem: RELOC-DIRECT (src/habu/aot-lib.f) drops a BL only when its target is the (MARK) record and hands every other unmapped target to MAP-TARGET!, which refuses by name ('aot: PC-relative target removed or outside closure site=...'); no fixture pins that arm, and e0b71e05 changed it. ptr-cell-mark (BPTRCELLMARK, src/habu/habu2.f) branches to LPTRMARK, an entry inside the (MARK) body but not its code entry, so FINDADDR-PTR resolves nothing and a stripped image that stores a pointer into a persisted pointer cell at run time should be refused site=ptr-cell-mark - unmeasured, the life-hook worker built no such program. Acceptance: an HBT-STRIPPED-* fixture in tools/hb-build-test.f whose program stores a pointer into a persisted pointer cell at run time and pins the measured refusal text; if checked code cannot reach that store, the measurement that shows it and a comment beside RELOC-DIRECT stating so. Files: tools/hb-build-test.f, src/habu/aot-lib.f. Verify: tools/hb-build-test.f. Depends: none. Ownership: hazel. Claim: unassigned.
