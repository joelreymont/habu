---
title: Bind a multi-cell value to an untyped local whole
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T00:26:14.863182+02:00"
---

Problem: docs/forth.md:2638 says an untyped local can transport a multi-cell value. At run time binding an option<n> to an untyped local leaves one cell on the stack while the checker counts none: /tmp/stuart-habu/depth.f (stuart, Tender, 2026-11-07; installed hb at master 2dbfb33041) prints cumulative depths plain 0, optn 1, optn2 2, direct 2, direct2 3, then E-EVAL-RESIDUE. OPTN binds '{: a b c code:n :}' after a catch of KEEPO ( n n option<n> -- n n option<n> ); OPTN2 the same with a callee of empty body; DIRECT2 binds '1 2 3 option:SOME {: a b c :}' with no catch; DIRECT ('1 2 3 option:SOME KEEPO {: a b c :}') leaves none. The checker and the compiled code disagree on the stack's depth, so a checked program misbehaves. Cause not yet found. Acceptance: the root cause is fixed in the layer that has it; every word in depth.f leaves depth 0; a test runs a program binding a multi-cell value (option, and a value after catch) to an untyped local and checks the depth. Files: to be named by the root-cause report. Verify: bin/hb --load /tmp/stuart-habu/depth.f prints depth 0 on every line. Depends: none. Ownership: carl (Habu integrator); Tender lane x2b waits on it. Claim: unassigned.
