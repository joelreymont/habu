---
title: An unmodeled immediate in a does> clause is refused as undefined at does>
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T15:25:57.186812+02:00"
---

Problem: a does> clause that uses an immediate word with no model is refused by the load and by tools/check.f --json-errors with E-UNDEFINED at 'does>' where a body's use is refused with E-UNMODELED-IMMEDIATE at the word (case does.f in /home/joel/Work/reviews/habu-10x-2026-11-10/cases8/imm; --verify-only accepts it, which is habu-the-pre-pass-3587fdd1). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Cause (hunt, briefs8/reports/immdoes-hunt.md, cases cases8/immdoes): the engine's immediate preflight hands the checker the whole text captured so far, head, `does>`, clause signature and clause, with no clause offset; CHECKER-PREFLIGHT:CHECK-TARGET! (src/core/checker.f :26011) checks it as one body, and DOES-SPLIT (:11055) is set only by the native compiler at `;`, so `does>` is walked as an ordinary word, refused first, and pinned.

Settled fix (brief /home/joel/Work/reviews/habu-10x-2026-11-10/briefs8/Finish-does-clause-preflight.md): no reader of the engine's DOESB-CELL or TCSIG cells and no second scanner. In the preflight the walk ends the head where it meets a does> token, CHECK names the closer after the walk, and RUN checks the clause apart under <name>;does; the offset is private to CHECKER-PREFLIGHT, reached through two new public words of that package, DOES-TOK? ( n -- bool ) and CLAUSE? ( -- bool ). A head that ends at a clause and that the check neither certifies nor refuses is refused by its own pinned fault and the clause is not checked (v-head-undef stays E-UNDEFINED, mk, NOSUCH in both modes). The clause's place comes from the engine's rows, not from moving the diagnostic origin. Unlanded and unfinished: unqylnuy 5168035f "WIP: Check a does> clause apart in the immediate preflight" (HEAD-REFUSE built); continue from it.

Acceptance: the packet names mk;does, the immediate word and E-UNMODELED-IMMEDIATE at its true place (line 2, column 39, bytes 74-81), for does.f under --load and --json-errors, and a test in test/diag-position-test.f pins it. Depends: habu-an-evaluate-inside-d3da5e06 (the does> clause's places); habu-the-pre-pass-3587fdd1 changes the same words. Claim: unassigned.
