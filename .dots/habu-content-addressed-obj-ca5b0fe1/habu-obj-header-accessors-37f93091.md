---
title: Object header accessors
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T22:07:36.763640+02:00\\\"\""
closed-at: "2026-07-01T22:09:47.050631+02:00"
close-reason: "completed: OBJ now exposes SOURCE$/TARGET$/CHECKER$/COMPILER$ over validated canonical rows, with build/load regression coverage. Proof: object-test, typed-local-diff-lint, stdlib-manifest-test, filemap-lint, host-lint, trust-lint, stale-status-lint, lint-artifacts-fast, full native suite 18039ms <= 40000ms."
---

Problem: OBJ exposes body rows but not canonical source/target/checker/compiler header fields, so a higher-level source+ABI object cache cannot verify that a loaded object matches the requested build identity after following an index entry. Fix: add checked OBJ:SOURCE$, OBJ:TARGET$, OBJ:CHECKER$, and OBJ:COMPILER$ accessors over validated canonical rows, refactor row accessors through private helpers, and cover loaded/build records. Files: lib/object.f, lib/object-test.f, docs/stdlib.md, lib/std.manifest. Acceptance: accessors return the validated header fields after BUILD and after OBJ:LOAD; row accessors keep behavior; missing/invalid headers still fail closed. Verify object-test, typed-local-diff-lint, manifest/lints, lint-artifacts-fast, full native suite before master.
