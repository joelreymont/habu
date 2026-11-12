---
title: "A catch calls cells intact that a row-polymorphic callee's quotation overwrote"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T12:47:11.826826+02:00"
---

Problem: a callee's reach is taken to end at its declared fixed inputs (src/core/checker.f CALL-EDGE-MASK :6037, ROW-COMMON :5748 bounds the popped region; the catch window is counted from Q>DIN ROW-TERMS in QUOT-WINDOW :6107), but a callee that executes a quotation typed over its row variable hands the cells under its inputs to code that pops and pushes them. RPX ( R n [ R n -- R n ] -- R n [ R n -- R n ] ) {: q :} q execute q; ZAP ( option<n> n -- option<n> n ) does drop drop 1000 2000 7 throw on one path; 9 OPTION:SOME 5 ['] ZAP ['] RPX catch drop drop {: o:option<n> x:n :} is accepted, x reads 7 where 5 is meant and reading the option ends 'hb: bad option tag' rc 85. Programs ~/.cache/tmp/carl-edge/rv/p4.f, p5.f. Acceptance: the three programs Y1, Y2, Y3 of p4.f are refused with E-STALE-READ; a callee whose quotation keeps the row stays accepted; cases in test/catch-stale-suite.f. A fix treats the cells such a callee's row variable binds inside the window as popped, in CALL-EDGE-MASK and in the catch window. Depends: none. Claim: unassigned.
