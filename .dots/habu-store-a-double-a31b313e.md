---
title: Store a double into a memory cell
status: open
priority: 2
issue-type: task
created-at: "2026-08-02T23:03:47.334635+02:00"
---

The scalar float leaf places a double in a straight line only. src/compiler/native/elaborate.f COERCE1 crosses a cell into a double wherever a float operand wants one, and refuses the other direction with E-NELAB-TYPE - so a body that stores a double with ! (the T-SET shape maki/array.f uses) is refused rather than compiled. What is missing is the crossing at hir.store's value operand, and the rule that says which cell positions a double may cross into: the honest one is an operation that WRITES memory, read off the schema's effect, not a list of opcode names. Reachable today: : W ( r ptr a -- ) {: v:r b:ptr :} v 1.0 f+ b ! ; throws -8580. Test: test/compiler/native-migrate.f FLOAT-REFUSAL-CASES pins the refusal, and flips to a compiled row when this lands.
