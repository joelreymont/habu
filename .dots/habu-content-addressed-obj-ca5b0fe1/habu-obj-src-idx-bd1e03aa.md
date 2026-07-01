---
title: Object source index
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T21:48:47.976954+02:00\\\"\""
closed-at: "2026-07-01T21:53:10.187302+02:00"
close-reason: "completed: added checked OBJIDX source+ABI index, deterministic source key hashing, source-key to object-key store/load, miss and malformed record coverage, docs, manifest, filemap, and suite wiring. Proof: object-index-test, stdlib-manifest-test, typed-local-diff-lint, filemap-lint, host-lint, trust-lint, stale-status-lint, lint-artifacts-fast, full native suite 18509ms <= 40000ms."
---

Problem: content-addressed object files are keyed by full object bytes, so a build cannot know whether a source+ABI object exists without recompiling it. Fix: add a checked OBJIDX source-index store that hashes source digest + target/checker/compiler ABI into a source key and maps that key to a stored OBJ key. This is the lookup layer needed before compiler integration. Files: new lib/object-index.f, lib/object-index-test.f, docs/stdlib.md, lib/std.manifest, FILEMAP.md, test/gate-stdlib-cases.f. Acceptance: source key is deterministic and changes when any ABI/source field changes; STORE writes a validated object key under a source key; LOAD returns key+true for hits and false for misses; malformed keys/records fail closed; suite/lints/full gate pass before master.
