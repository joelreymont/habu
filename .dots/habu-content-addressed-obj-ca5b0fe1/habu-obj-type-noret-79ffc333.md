---
title: Object type noret tables
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T21:58:35.451466+02:00\\\"\""
closed-at: "2026-07-01T22:01:53.608276+02:00"
close-reason: "completed: OBJLINK now preserves type and no-return metadata with counts/accessors and overflow failures. Proof: object-link-test, stdlib-manifest-test, typed-local-diff-lint, filemap-lint, host-lint, trust-lint, stale-status-lint, lint-artifacts-fast, full native suite 18653ms <= 40000ms."
---

Problem: OBJ records carry type and noret rows but OBJLINK discards them, so checked type/no-return metadata would be lost before the object linker/build cache can use it. Fix: extend lib/object-link.f with bounded type and noret tables, copy type name+kind and no-return symbol rows during ADD, expose counts/accessors, and cover overflow. Files: lib/object-link.f, lib/object-link-test.f, docs/stdlib.md, lib/std.manifest. Acceptance: type/noret rows survive later OBJ:LOAD calls; accessors bounds-check; overflow throws E-OBJ-CAPACITY; existing link checks still pass. Verify object-link test, typed-local-diff-lint, manifest/lints, full native suite before master.
