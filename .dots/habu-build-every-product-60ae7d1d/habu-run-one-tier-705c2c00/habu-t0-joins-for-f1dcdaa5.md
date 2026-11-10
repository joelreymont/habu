---
title: Keep T0 values in registers across case and MATCH joins
status: open
priority: 1
issue-type: task
created-at: "2026-10-29T05:11:01.114852+02:00"
---

Source, Joel: "T0 takes the JIT's techniques."

Problem: T0, the tier 0 compiler, keeps the top of the data stack in registers (its virtual stack) where paths meet at `else`/`then`, a `begin` loop and a counted loop: the opener records a layout (SNAPSHOT) and each joining path reconciles to it (RECONCILE). Counted loops landed in a215caf3c3 and 4a80ee47f3. But "every `case` and MATCH join spills the whole virtual stack, so those paths meet with it empty" (src/habu/t0.f:173-174): `of`/`endof`/`endcase` (t0.f DO-ENDOF :1046, DO-ENDCASE :1054) and a MATCH's arms and its closing `;MATCH` (t0.f :1356-1369) store every value to memory and reload it after the join.

Acceptance: `case` and MATCH record a layout at the opener and each arm's end reconciles to it, as `if`/`else`/`then` do; values live before a `case` or MATCH stay in registers across it; the comment at t0.f:168-174 says so.

Files: src/habu/t0.f, src/habu/t0-emit.f, src/habu/t0-arm64.f as needed, test/t0-install.f, test/t0-arm64-rows.f.

Verify: rows in test/t0-install.f that run words with values live across a `case` and across a MATCH and check their results, as a215caf3c3's counted-loop rows do, and a test/t0-arm64-rows.f row that checks the compiled words store no live value to the stack at the join, as 4a80ee47f3's IDX-CASE checks its words; then bin/hb --load each of the two files passes.

Depends: none.

Ownership: unassigned. Claim: unassigned.
