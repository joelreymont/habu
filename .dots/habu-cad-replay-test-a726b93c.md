---
title: cad-replay test flakes under gate load
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T04:03:53.837694+02:00"
---

maki/cad-replay-test.f went red once inside a full gate (child asserts 108/109) and green standalone and in two later gate runs (2026-08-10, trap lane). It clones the engine and runs the whole cad suite in a child with a 120s fixed timeout and an inherited HB_TMP; maki does not reference the compiler chain, so codegen lanes cannot reach it. Diagnose before shrinking: is the 120s wall fixed where every other suite wall scales with load (the schedlint precedent, STDLIB-GATE:SUITE-TIMEOUT-MS), or is the child racing something in the shared HB_TMP? Files: maki/cad-replay-test.f. Depends: none.
