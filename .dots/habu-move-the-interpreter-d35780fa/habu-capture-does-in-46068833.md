---
title: Capture does> in Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.930638+03:00"
blocks:
  - habu-compile-immediates-from-cc47ecf4
---

Problem: `does>` capture is the assembly `CAPTURE-DOES` (`habu2.f:7495-7507`).
Acceptance: `CAPTURE-DOES` semantics: `DOESB-CELL`, `C-PARSE-CREATED-SIG`, the second-`does>` refusal.
Files: `src/habu/definers.f`, `src/habu/outer.f` (compile-mode dispatch), cases beside `test/outer-interpret.f`.
Verify: spark: `does>` cases through the Habu loop under the feature cell, the second-`does>` refusal included; gate.
Depends: habu-compile-immediates-from-cc47ecf4 (I5c).
Route: Alder (shared: src/habu/definers.f, src/habu/outer.f and the test).
Ownership: krait (Intel lane).
Claim: unassigned.
