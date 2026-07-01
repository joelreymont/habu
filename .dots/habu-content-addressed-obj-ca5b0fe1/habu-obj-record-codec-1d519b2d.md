---
title: Object record codec
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T20:36:59.835127+02:00\\\"\""
closed-at: "2026-07-01T20:46:32.734705+02:00"
close-reason: "completed: added checked OBJ package object-record codec, deterministic serialization/load/keying, focused lib/object-test.f coverage, stdlib docs/manifest/filemap/suite wiring. Proof: lib/object-test.f ok; typed-local-diff-lint ok; filemap-lint ok; stdlib-manifest-test ok; lint-artifacts-fast ok; host-lint/trust-lint/stale-status-lint ok; full native suite PASS 20608ms <= 40000ms persistent budget."
---

Problem: habu-content-addressed-obj-ca5b0fe1 is too broad to land safely as one change. Current caches in tools/hb-build-lib.f/test/run-lib.f key final executables and maker artifacts, not reusable pre-link source objects. First root-correct slice: add a checked Habu object-record library with typed metadata for source hash, target/checker/compiler ABI text, exported/imported symbols, relocations, package/require metadata, effect records, and no-return/type records. Files: new lib/object.f, lib/object-test.f, FILEMAP.md, lib/std.manifest, docs/stdlib.md. Acceptance: object records serialize deterministically, reject capacity/field overflows, round-trip through tests, content keys include ABI/source/target fields, and no build path uses the format until the linker/compiler-emission slice exists. Verify: bin/hb --load lib/object-test.f; typed-local-diff-lint; host-lint; filemap-lint; full native suite before master.
