---
title: "Print every interpreter refusal in the checker's shape, with a class"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T08:03:07.752747+02:00"
---

Problem: the engine's interpreter (src/habu/interpreter.f) refuses in two shapes of its own, neither the checker's: five refusals print 'E-CLASS: text at path:line' (UNDEFINED-NAME :342-343, E-SEALED-PACKAGE :364, E-UNDERFLOW :406 and :2036, E-MISSING-SIGNATURE :1591; also src/compiler/native/definition.f:476), and about 40 print 'hb: text' with no class at all (:349-2162, e.g. 'hb: dictionary full at:', 'hb: defer: missing name after'). The checker prints 'E-CLASS habu: path:line: prose' (src/core/render.f DSITE :909-916, E-UNDEFINED :1782-1784). So one undefined word reads 'E-UNDEFINED: X at f:1' from the interpreter and "E-UNDEFINED habu: f:1: undefined word 'X'" from the checker, and an agent reading a refusal must know which layer spoke; a classless one names no reason it can look up. Acceptance: every refusal interpreter.f and definition.f print starts with an E- class and has the checker's shape, path:line first where a file is open; the tests that match the old text (test/dictionary-zero.f, repl-address-cell-rollback.f, cast-negative-suite.f, program-diagnostics-test.f, repl-open-definition.f, gate-dictionary-lib.f, load-reject-diag-test.f, shadowed-arity-test.f, src/lib/task-test.f:620) match the new. Depends: none. Claim: unassigned.
