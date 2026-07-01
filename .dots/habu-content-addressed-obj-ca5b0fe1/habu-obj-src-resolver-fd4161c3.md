---
title: Object source resolver
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T22:11:01.388044+02:00\\\"\""
closed-at: "2026-07-01T22:16:04.735727+02:00"
close-reason: "completed: added OBJRES source+ABI resolver over OBJIDX and OBJSTORE, with missing-index false and stale/wrong object fail-closed coverage. Proof: object-resolve-test, typed-local-diff-lint, stdlib-manifest-test, filemap-lint, host-lint, trust-lint, stale-status-lint, lint-artifacts-fast, full native suite 17881ms <= 40000ms."
---

Problem: OBJIDX and OBJSTORE are separate pieces; build integration needs a single checked source+ABI resolver that turns source digest + target/checker/compiler into a verified loaded OBJ, without trusting an index entry blindly. Fix: add lib/object-resolve.f package OBJRES with ROOT!, STORE, and LOAD. STORE stores the current OBJ and records its own SOURCE$/TARGET$/CHECKER$/COMPILER$ index. LOAD computes the source key, returns false on missing index, uses OBJSTORE:LOAD for content-key validation, then checks loaded headers against the requested source+ABI and throws E-OBJ-SCHEMA for stale/wrong entries. Files: lib/object-resolve.f, lib/object-resolve-test.f, docs/stdlib.md, lib/std.manifest, FILEMAP.md, test/gate-stdlib-cases.f. Acceptance: store+load roundtrip works; miss returns false; wrong-index object throws; corrupt index/object paths fail closed; object/link/index tests still pass. Verify object-resolve-test, typed-local-diff-lint, manifest/filemap/lints, lint-artifacts-fast, full native suite before master.
