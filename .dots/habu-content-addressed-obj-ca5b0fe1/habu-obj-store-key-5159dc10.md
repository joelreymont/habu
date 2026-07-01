---
title: Object store key validation
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T22:03:45.032236+02:00\\\"\""
closed-at: "2026-07-01T22:05:57.199094+02:00"
close-reason: "completed: OBJSTORE:LOAD recomputes OBJ:KEY-HEX and rejects schema-valid wrong-key cache files. Proof: object-cache-test, typed-local-diff-lint, stdlib-manifest-test, filemap-lint, host-lint, trust-lint, stale-status-lint, lint-artifacts-fast, full native suite 17861ms <= 40000ms."
---

Problem: OBJSTORE:LOAD accepts any schema-valid object at a requested key path without recomputing OBJ:KEY-HEX, so a corrupted or stale cache file can masquerade as a content-addressed hit. Fix: after OBJ:LOAD in lib/object-cache.f, recompute the current object key and compare it to the requested key, throwing E-OBJ-SCHEMA on mismatch. Files: lib/object-cache.f, lib/object-cache-test.f, docs/stdlib.md if wording needs clarification. Acceptance: correct STORE/LOAD still passes; schema-invalid cache file still throws; schema-valid wrong-key cache file throws; no object index/link behavior regresses. Verify object-cache-test, typed-local-diff-lint, manifest/lints, lint-artifacts-fast, full native suite before master.
