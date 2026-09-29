---
title: Close definitions with semicolon in Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.939780+03:00"
blocks:
  - habu-capture-does-in-46068833
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
