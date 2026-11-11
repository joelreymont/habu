---
title: "Derive NDICT's lookup chain from OUTER"
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T16:08:44.075068+03:00"
blocks:
  - habu-move-evaluate-and-9119f746
---

Problem: after I9a two chains apply the same lookup order: `INTERPRETER:FIND` (I2, the interpreter's) and NDICT's private chain (`src/compiler/native/dict.f:29-110`, the compiler's). They differ in one contract: NDICT applies `VISIBLE-RECORD?` (`dict.f:47-53`) at every step and continues past an internal or START-0 row, where `INTERPRETER:FIND` stops at the first match and reports the flag.
Acceptance: `OUTER` exposes a per-wordlist visibility predicate or a filtered probe, and NDICT's chain becomes `OUTER`'s order over it, so the order rules exist once; `test/outer-find.f` and the compiler suites that read NDICT pass unchanged.
Files: `src/compiler/native/dict.f`, `src/habu/outer.f`.
Verify: rebuild; chain; gate.
Depends: I9a (`habu-move-evaluate-and-9119f746`), which puts `outer.f` in the product closure (`compiler.f:31` requires `dict.f`).
Route: Alder (shared: `dict.f`).
Ownership: krait (Intel lane).
Claim: unassigned.

Census 2026-11-04: a disagreement between two implementations of one thing; it is closed by its parent removing the second implementation, not by patching that copy.
