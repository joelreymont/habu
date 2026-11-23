---
title: Print a stale row tail on both sides of a mismatch
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T14:22:43.406079+01:00"
---

Problem: in `( R n [ R n -- R n ] -- R n n ) catch` the text reads expected `n n` against found `stale<A> stale<n> n`: RBUF-ROW (src/core/render.f:291) drops a row-variable tail but prints a stale tail as a value, RROW-LETTER (:330) letters it in render order, not by the declared R, and END-LABEL (:1938) calls a stack holding only a stale row empty (hunt: /tmp/carl-mmtxt-report.md). Acceptance: both sides print their tails by declared letter when one is stale, and the label names the stale row; a test asserts T5's and T4B's texts. Depends: none. Open (review of xmwpltro): its O12 assertion (test/catch-stale-suite.f:619-620) expects END-LABEL's text, but syvpyrwo makes O12 E-STALE-READ, which repeats O1; END-LABEL's stale-only arm is still reached by CS-K132's program `( R [ R n -- R n ] -- R ) [: 5 swap [: [: CS-EXEC ;] catch drop drop ;] execute ;] execute`, which should replace O12. PAIR-STALE? (src/core/render.f:583) names tails outside expected/found (`: M1 ( R [ R -- R ] -- R ) dup >r catch drop r> execute ;` prints inferred `R [ R -- R ] -- R R`): it must answer false while RNAMES is set. Quotations inside a stale pair still print cells only (`( R [ R -- R ] -- R [ R -- R ] ) catch drop` shows expected `R [ -- ]`). Comments the stack adds in src/core/render.f and checker.f break card rule 9 (replacements: /tmp/carl-ctext-review-l2.md). Claim: unassigned (its commit is on the parked stack lmrmpkys..xvroprym above tqsnwlwz, workspace .jj-ws.noindex/carl-infname).
