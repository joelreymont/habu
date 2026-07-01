---
title: Object address definitions
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T21:21:43.262370+02:00\\\"\""
closed-at: "2026-07-01T21:27:07.962768+02:00"
close-reason: "completed: added OBJ:DEF+ def rows, OBJLINK definition table, duplicate/out-of-range validation, merged text-address lookup, and preserved export-only import resolution. Proof: object-test, object-link-test, stdlib-manifest-test, typed-local-diff-lint, filemap-lint, host-lint, trust-lint, stale-status-lint, lint-artifacts-fast, full native suite 18995ms <= 40000ms."
---

Problem: export rows carry symbol/effect metadata but no text offset, so the object/linker layer still cannot resolve relocations to merged code addresses. Fix: add OBJ:DEF+ / def rows (symbol, text offset, effect), validate them in OBJ:LOAD, and extend OBJLINK with a definition table that copies names, rejects duplicate definitions, validates def offsets against current text size, stores merged text addresses, and exposes DEF count/name/address lookup. Existing export/import validation remains. Files: lib/object.f, lib/object-test.f, lib/object-link.f, lib/object-link-test.f, docs/stdlib.md, lib/std.manifest. Acceptance: def serializes/loads; bad def offset rejects; OBJLINK records def global addresses using per-object text base; duplicate defs reject; out-of-text def offsets reject; no build integration yet. Verify object/object-link tests, typed-local-diff-lint, manifest/lints, full suite before master.
