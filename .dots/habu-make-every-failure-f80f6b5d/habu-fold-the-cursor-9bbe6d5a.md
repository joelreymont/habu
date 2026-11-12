---
title: Fold the cursor-line copies into one word
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T20:12:46.129794+02:00"
---

Problem: 'the line the cursor is on, counted from the open file's first owned byte' is written out in src/habu/interpreter.f LINE (:327-328), src/core/checker.f :29514-29515 and :29524-29527; dot habu-say-a-wasm-19848f34 adds SRCLOC:LINE for it in checker.f's SRCLOC. src/compiler/native/definition.f:459-461's copy goes with tier 1. Acceptance: the interpreter.f and checker.f copies call SRCLOC:LINE and no other copy remains outside tier 1; the chain build reaches its fixpoint and the suite passes. Depends: habu-say-a-wasm-19848f34. Claim: unassigned.
