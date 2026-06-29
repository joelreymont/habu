---
title: Refactor native test suite setup groups
status: open
priority: 2
issue-type: task
created-at: "2026-06-30T00:21:17.417802+02:00"
---

Problem: test/run.f and test/gate-stdlib-inline-lib.f still blur suite setup, test groups, and tests; tool semantic tests reload shared tool setup in separate process groups and failures can lose suite/test context. Fix: add a checked test-suite framework with suite setup/teardown, named groups, test timing/pass/fail reporting, and group-level shared setup loaded once; tests must include only test-specific files. Files: test/gate-stdlib-inline-lib.f, test/gate-runner-entry.f, test/run.f, docs/gate.md. Verify: direct tool-semantics timing, full native test suite with --timings.
