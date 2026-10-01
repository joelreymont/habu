---
title: Close definitions with semicolon in Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.939780+03:00"
---

Problem: `;` is the assembly close sequence (`habu2.f:7563-7573`).
Acceptance: `;` -> `NCOMP:COMPILE ( ptr u8 n )`, `TIER-PROV:CLOSE`, the trusted-state clear and the `PEND` clear as `habu2.f:7563-7573` does; the `compiler-native-*` and `checker-*` suites green at `1 set-tier` under the feature cell.
Files: `src/habu/definers.f`, `src/habu/outer.f` (compile-mode dispatch), cases beside `test/outer-interpret.f`.
Verify: spark: the `compiler-native-*` and `checker-*` suites at `1 set-tier` under the feature cell; gate.
Depends: habu-capture-does-in-46068833 (I5d).
Route: Alder (shared: src/habu/definers.f, src/habu/outer.f and the test).
Ownership: krait (Intel lane).
Claim: unassigned.
- From I4 design: this leaf also owns interpret-mode `immediate` (`habu2.f:8354`), which marks the definition it closes.

K-lane correction (design 2026-09-30): Acceptance add provenance open/close rows in `prims.f` with bodies on both targets. Every `TIER-PROV:OPEN,`/`CLOSE,` call site is the assembly interpreter (`habu2.f:2783,7704,9259-9381`); a Habu loop needs them as primitives. K9b provides the x86 bodies. Files add `src/habu/prims.f`, `src/habu/habu1.f`.

Lead note (2026-09-30, from the I8/I5a design): gains `cast:` (from I5a). `def-open` (I4c) already stores OPEN-CELL, so I5e's provenance row is only the close, and that row also clears what `def-open` set (DEF-TIER, TSIG, TCSIG, DOESB, TRUSTED, PEND). Files: `definers.f`.
