---
title: A locals group takes -- and | as locals
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T10:40:12.990770+02:00"
---

Problem: LOC-TOK (src/core/checker.f:20785-20788) answers `--` inside `{: … :}` by setting UNCK and nothing else, so `: T ( n -- n ) {: a b | c -- d :} a ;` is refused as E-UNCHECKABLE with the bare line `habu: in t: at '--'` (case /Users/joel/Work/reviews/habu-10x-2026-11-10/refusals/p14.f). `|` is not caught at all: `: T ( n n -- n ) {: a | :} a ;` certifies with `|` bound as a local that takes a stack cell. Source: the root-cause hunt of habu-checker-refusals-that-3152182a, 2026-11-10; Joel, 2026-11-10: "you must dot and launch a fix automatically for any issues you identify". Acceptance: `--` and `|` in a locals group are refused at the token as E-BAD-LOCAL-SHAPE, each with a reason after the token: for `--`, that a locals group names only locals and the signature declares the outputs; for `|`, that Habu has no uninitialised locals. test/diag-position-test.f:855-856 (a mismatch at c@ wins over the `--` local) still holds. docs/forth-card.md's refusal table gains the row. Files: src/core/checker.f LOC-TOK, src/core/render.f LOCALBAD-PROSE and its JSON twin, the checker's tests, docs/forth-card.md. Verify: p14.f and the `|` case print the reason; a private engine and the full suite, since the checker is shared. Depends: none. Ownership: the checker's locals group. Claim: unassigned.
