---
title: Object row accessors
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T20:52:05.779491+02:00\\\"\""
closed-at: "2026-07-01T20:57:20.602803+02:00"
close-reason: "completed: added OBJ row-count/row/tag/field accessors with field bounds checks, docs, manifest rows, and object tests. Proof: bin/hb --load lib/object-test.f; typed-local-diff-lint; stdlib-manifest-test; filemap/host/trust/stale lints; lint-artifacts-fast; full native suite 17718ms <=40000ms."
---

Problem: lib/object.f can serialize/load object records but a future linker cannot inspect validated records without reparsing ad hoc. Fix: add checked row accessors over the current canonical object: row count excluding magic, row slice, tag slice, field count, and field slice. Files: lib/object.f, lib/object-test.f, docs/stdlib.md, lib/std.manifest. Acceptance: accessors throw E-OBJ-SCHEMA/E-OBJ-FIELD for bad indexes, expose tag/fields for source/text/data/reloc rows, preserve existing serialization/key behavior, and do not integrate build/link paths yet. Verify: bin/hb --load lib/object-test.f; typed-local-diff-lint; stdlib-manifest-test; filemap/host/trust/stale lints; full native suite before master.
