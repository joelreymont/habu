---
title: Name the outcome-assert completion variants
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T03:56:10.961794+02:00"
---

Static invariant: lib/test/outcome.f collapses three different facts into one anonymous line. T-OUTCOME-EXITED= reports a child that hit its capture deadline, a child killed by a signal, and a child that exited with the wrong status all as 'expected 0 got 1', so a test cannot tell a hung child from a broken one without reading the fixture source.

Full context: found 2026-07-30 while stabilizing test/compiler/ir-id.f under dot habu-stabilize-two-pool-763a7ec9. That fixture now carries its own CHILD-EXITED= with named CHILD-HUNG and CHILD-SIGNALED verdicts, because the shared library cannot be edited: lib/test/outcome.f defines its three assertions at global scope with no package, so tools/package-diff-lint.f rejects any change to them (measured: a one-character edit to the timeout arm reports E-PACKAGE-OWNERSHIP lib/test/outcome.f:9:3). Every other caller of T-OUTCOME-EXITED= still gets the anonymous line.

Required result: package lib/test/outcome.f (a short package such as TOUT with a public T-OUTCOME-* surface, or fold it into an existing test package), then give each MATCH arm a named diagnostic: deadline reached, died on signal N, exited with the wrong status. Keep exactly one assert per call so existing assert numbering does not shift. Then delete the local copies in test/compiler/ir-id.f and any other fixture that grew its own.

Acceptance: package-diff-lint exit 0 on the diff; every existing caller of T-OUTCOME-EXITED=, T-OUTCOME-SIGNALED= and T-OUTCOME-TIMEOUT still compiles and passes; a forced timeout and a forced signal each print a line naming which one happened; test/compiler/ir-id.f loses its private verdict words.
