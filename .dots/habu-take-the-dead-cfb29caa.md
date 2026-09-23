---
title: Take the dead flag through a ticked callee
status: open
priority: 3
issue-type: task
created-at: "2026-09-23T19:01:45.620198+03:00"
---

Problem: the tick route (BTICK-EDGE, landed with dot 00fe3689) takes a ticked callee's throw edge and intact masks but not its CTL-DEAD flag, measured: with the flag, compiler-native-finally-aot reds 'ncomp: cannot compile THROWING at [']' (E-NELAB-QUOT -8651, exit 67) and ( ptr u8 -- n ) ['] WBOOM catch {: v code:n :} code certifies but dies under test/compiler/aot-mode.f with 'ncomp: cannot compile CB at [']'. Cause: a dead body publishes CWIN-OUT = CELLS-NONE and src/compiler/native/elaborate.f turns that into the body's calling convention (DO-FINALLY: body in out 0 max QFILL; DO-CATCH: k win win QFILL); for a literal that is the function it is about to emit, for a tick the row already carries an existing routine's declared ABI and QFILL refuses the disagreement at the [']. So ['] WDEAD catch is today the fit-check against WDEAD's declared output row (test/catch-stale-suite.f T11 pins that it is not the stale rule). Acceptance: the elaborator lets a dead body's site result row leave a named routine's convention alone (DO-CATCH/DO-FINALLY), BTICK-EDGE then takes CTL-DEAD, T11 flips to the stale-rule refusal, the AOT suites (test/compiler/aot-mode.f with native-finally.f and native-catch.f, compiler-native-finally-aot) stay green, docs/forth.md's catch-stale entry drops the dead-callee exception. Files: src/compiler/native/elaborate.f, src/core/checker.f BTICK-EDGE, test/catch-stale-suite.f, docs/forth.md. Verify: the two suites, the AOT rows, test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.
