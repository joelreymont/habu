---
title: "A catch calls cells intact that a row-polymorphic callee's quotation overwrote"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T12:47:11.826826+02:00"
---

Problem (review of the call-edge mask fix, dot 00c7ef76, 2026-11-11; programs ~/.cache/tmp/carl-edge/rv/p4.f, p5.f, logs p5-new.log, both the installed engine and the candidate): a callee's reach is taken to end at its declared fixed inputs (src/core/checker.f CALL-EDGE-MASK, ROW-COMMON bounds the popped region; the catch window is counted from Q>DIN ROW-TERMS in QUOT-WINDOW about 5911), but a callee that executes a quotation typed over its row variable hands the cells under its inputs to code that pops and pushes them. RPX ( R n [ R n -- R n ] -- R n [ R n -- R n ] ) {: q :} q execute q; ZAP ( option<n> n -- option<n> n ) does drop drop 1000 2000 7 throw on one path; 9 OPTION:SOME 5 ['] ZAP ['] RPX catch drop drop {: o:option<n> x:n :} is accepted, x reads 7 where 5 is meant and reading the option ends 'hb: bad option tag' rc 85. Acceptance: the three programs Y1, Y2, Y3 of p4.f are refused with E-STALE-READ; a callee whose quotation keeps the row stays accepted; cases in test/catch-stale-suite.f. A fix treats the cells such a callee's row variable binds inside the window as popped, in CALL-EDGE-MASK and in the catch window. Files: src/core/checker.f, test/catch-stale-suite.f. Verify: catch-stale-suite rc 0; p4.f refused. Depends: 00c7ef76 (lands first). Ownership: unassigned. Claim: unassigned.
