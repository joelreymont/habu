---
title: Object cache file store
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T21:01:06.289189+02:00\\\"\""
closed-at: "2026-07-01T21:07:25.107406+02:00"
close-reason: "completed: added OBJSTORE checked object cache file store, object max capacity export, tests, docs, manifest, filemap, and stdlib suite wiring. Proof: object/object-cache tests; typed-local-diff-lint; stdlib-manifest-test; filemap/host/trust/stale lints; lint-artifacts-fast; full native suite 17829ms <=40000ms."
---

Problem: content-addressed object/linker parent has canonical object records and row accessors, but no checked file-store API to persist/load objects by key without ad hoc path assembly. Fix: add lib/object-cache.f as a separate OBJSTORE package over lib/object.f + lib/fs-mutate.f. Provide root/path/key/path builders, atomic store of OBJ:BYTES$, existence check, and load-by-key that validates via OBJ:LOAD. Files: lib/object-cache.f, lib/object-cache-test.f, docs/stdlib.md, lib/std.manifest, FILEMAP.md, test suite manifests. Acceptance: cache path is root/<64-hex>.hbo, root dirs are created, store is atomic, load rejects missing/malformed files through existing FS/OBJ errors, and no build/link integration yet. Verify object-cache test, typed-local-diff-lint, manifest/filemap/host/trust/stale lints, lint-artifacts-fast, full suite before master.
