---
title: Compile a row with an open logical term beside other values
status: open
priority: 3
issue-type: task
created-at: "2026-10-04T03:33:41.515040+03:00"
---

Problem: the native compiler cannot place an open logical term (a sum or family application whose width reads an open type argument, which PUSH-LOGICAL keeps as one logical term) when it shares its declared row with other values: src/compiler/native/dict.f ROW-GLUE (~:219-237 on d37500ab). `: KEEP2 ( n option<a> -- n option<a> ) ;` certifies at tier 0 and fails at tier 1 with `ncomp: cannot compile KEEP2`, throw -8519, on the d37500ab engine; lane 562's `OR-ELSE ( a option<a> -- a )` fails the same way after its checker change. Fix: the compiler learns each logical term's cell width from the owner (a type variable is one cell, ruling 562-restart1) and places the row. Acceptance: KEEP2 and OR-ELSE compile at tier 1 and run equal to tier 0 through test/compiler/native-generic-calls.f and its tier-1 row compiler-native-generic-calls-aot, seen failing first; baked: rebuild, g1 == g2 with .names, two-generation build. After: 7ce778f8.
