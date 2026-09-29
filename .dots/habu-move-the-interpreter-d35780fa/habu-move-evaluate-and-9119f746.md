---
title: Re-enter evaluate from Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.689073+03:00"
blocks:
  - habu-close-definitions-with-8ace78d8
  - habu-move-pkgs-using-22f18b81
---

Problem: `evaluate` is assembly. First of I9a-c.
Acceptance: a re-entrant `evaluate` in `src/habu/outer.f`: saved input state, the `EVALD` depth, nested exits (`LEX0`/`EM-EVAL-CLEAN-EXIT` semantics).
Files: `src/habu/outer.f`, cases beside `test/outer-interpret.f`.
Verify: spark: nested `evaluate` cases through the Habu loop under the feature cell; gate.
Depends: habu-close-definitions-with-8ace78d8 (I5e), habu-move-pkgs-using-22f18b81 (I8).
Route: Alder (shared: src/habu/outer.f and the test).
Ownership: krait (Intel lane).
Claim: unassigned.
