---
title: Object link symbol validation
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T21:08:21.063672+02:00\\\"\""
closed-at: "2026-07-01T21:14:36.117585+02:00"
close-reason: "completed: added OBJLINK checked symbol validation over object rows, tests for resolved imports/duplicate exports/unresolved imports/table overflow, docs, manifest, filemap, suite wiring, and manifest-test buffer growth needed by the expanded docs. Proof: object/object-cache/object-link tests; typed-local-diff-lint; stdlib-manifest-test; filemap/host/trust/stale lints; lint-artifacts-fast; full native suite 17799ms <=40000ms."
---

Problem: object records can be serialized, stored, and inspected, but a future linker still lacks a checked in-memory symbol pass to reject duplicate exports and unresolved imports before byte merge. Fix: add lib/object-link.f as OBJLINK package over OBJ row accessors; it copies export/import names out of the current OBJ record into bounded tables, rejects duplicate exports, exposes counts/accessors, and validates every import against the accumulated exports. Files: lib/object-link.f, lib/object-link-test.f, docs/stdlib.md, lib/std.manifest, FILEMAP.md, test suite cases. Acceptance: multiple loaded OBJ records can be added; duplicate export throws E-OBJ-SCHEMA; unresolved import throws E-OBJ-SCHEMA; symbol storage/table overflow throws E-OBJ-CAPACITY; no build/link integration yet. Verify object-link test, typed-local-diff-lint, manifest/filemap/host/trust/stale lints, lint-artifacts-fast, full suite before master.
