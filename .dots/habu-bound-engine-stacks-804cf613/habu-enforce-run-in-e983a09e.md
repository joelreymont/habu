---
title: Enforce run-in-stack target capacity
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:20:20.420015+02:00"
blocks:
  - habu-infer-checked-stack-c2c1a721
  - habu-typed-top-xt-096a8f1b
---

Static invariant: run-in-stack executes only an xt carrying a typed effect and finite peak certificate, and the supplied target capacity is at least that certificate for every stack it relocates. Problem: run-in-stack currently ignores its size argument and trusts a caller guarantee, so a target whose final rows fit can still overflow the temporary stack during execution. Fix: give run-in-stack a typed target effect/peak input, validate exact target capacities before switching stacks, propagate target failures distinctly, and remove the unused-size boundary. Acceptance: exact capacity and nested finite use pass; one-cell-short data/return/loop capacity, uncertified xt, positive-growth target, mismatched certificate, and overflow fail before execution; caller state restores on every target throw. Files: src/habu/habu1.f, src/core/checker.f, bootstrap/cg/forth.fs, new test/run-in-stack-capacity.f, FILEMAP.md. Verify: focused checker/runtime fixtures, typed-top xt tests, catch/engine suite, bootstrap parity, typed-local diff, fixpoint, host/filemap/dot lints, full native gate. Depends on typed xt effects and checked stack-peak certificates; dispatch only after both and overlapping owners land.
