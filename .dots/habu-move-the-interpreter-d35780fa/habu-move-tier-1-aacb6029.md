---
title: Parse the colon head in Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.655350+03:00"
---

Problem: colon definitions start in the assembly interpreter (`habu2.f:7613-7660`, `EM-INTERPRET-COLON`). First of the five tier-1 compile-mode leaves I5a-e.
Acceptance: `:`/`:trusted`/`TRUSTED:`/`CAST:` name parse, the qualified-name rules (`C-QUALIFY-DEF`), the pending record, signature capture (`C-COLON-MAYBE-SIG`), the per-definition state reset and the tier dispatch cell, as `habu2.f:7613-7660` does, in `src/habu/definers.f`.
Files: `src/habu/definers.f`, `src/habu/outer.f` (compile-mode dispatch), cases beside `test/outer-interpret.f`.
Verify: spark: colon-head cases through the Habu loop under the feature cell; gate.
Depends: habu-scan-and-interpret-eea996a2 (I4).
Route: Alder (shared: src/habu/definers.f, src/habu/outer.f and the test).
Ownership: krait (Intel lane).
Claim: unassigned.
