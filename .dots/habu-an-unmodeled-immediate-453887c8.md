---
title: An unmodeled immediate in a does> clause is refused as undefined at does>
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:25:57.186812+02:00"
---

Problem: a does> clause that uses an immediate word with no model is refused by the load and by tools/check.f --json-errors with E-UNDEFINED at 'does>' where a body's use is refused with E-UNMODELED-IMMEDIATE at the word (case does.f in /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/imm; --verify-only accepts it, which is habu-the-pre-pass-3587fdd1). Found by that dot's hunt (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/imm-hunt.md, table row 'does'); cause not found. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt names as path:line why the clause's refusal names does> and E-UNDEFINED; then the packet names the immediate word and its code, and a test pins it. Files: src/core/checker.f, test/diag-position-test.f. Verify: does.f under --load and --json-errors. Depends: none; habu-the-pre-pass-3587fdd1 changes the same words. Ownership: the checker's diagnostics. Claim: unassigned.

Hunt (report briefs8/reports/immdoes-hunt.md, cases cases8/immdoes, table.txt): the engine's immediate preflight (habu2.f CAPTURE-IMMEDIATE to C-PREFLIGHT) hands the checker the whole text captured so far, head, `does>`, clause signature and clause, with no clause offset. CHECKER-PREFLIGHT:CHECK-TARGET! (src/core/checker.f) checks it as one body: DOES-SPLIT is set only by the native compiler at `;`, so `does>` is walked as an ordinary word, refused first, and pinned. Fix: CHECK-TARGET! splits at the clause (the engine's DOESB-CELL and TCSIG-A/U through a data-base mirror), checks the head, then the clause as CHECKER-SOURCE-DOES! does, so the packet names mk;does, the immediate and E-UNMODELED-IMMEDIATE. An undefined word or stray `then` in a clause is already right.
