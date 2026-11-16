---
title: "Take a body's parse-imm immediate as no step of the walk"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T16:11:17.060612+01:00"
---

Problem: a body call of a parse-imm immediate passes the immediate's call effects to the definition (src/core/check-walk.f CALL-STEP applies DO-TOK, whose INHSET line takes the callee's flags), though the immediate runs while the definition compiles: with `: HIT ( -- ) parse-name 2drop ;  : PI ( -- ) HIT ; immediate  s" PI" 1 parse-imm  : USE ( -- n ) PI zz 73 ;  USE . cr`, `hb --load tools/check.f -- --verify-only` warns W-CHECK-DEFERRED at the top-level USE ('no parses: row says what it reads') and leaves the rest of the file to the run, while the load runs USE as a word that reads nothing. Acceptance: the walk takes a body's parse-imm immediate as no step, so the definition takes none of its flags or edges (parses, renders, throw); --verify-only certifies that program with no deferral; a test pins it. Depends: none. Claim: unassigned.
