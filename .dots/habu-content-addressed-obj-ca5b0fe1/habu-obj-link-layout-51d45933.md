---
title: Object link layout table
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T21:18:40.162142+02:00\\\"\""
closed-at: "2026-07-01T21:21:02.690579+02:00"
close-reason: "completed: OBJLINK records bounded per-object text/data base+size layout rows with checked accessors and object overflow coverage. Proof: object-link test; typed-local-diff-lint; stdlib-manifest-test; filemap/host/trust/stale lints; lint-artifacts-fast; full native suite 17761ms <=40000ms."
---

Problem: OBJLINK computes total section sizes but does not retain per-object base/size rows, so the future linker still cannot relocate object-local offsets into merged text/data offsets without rescanning add order. Fix: extend OBJLINK with bounded per-object layout tables recording text/data base and size at each ADD, plus checked accessors. Files: lib/object-link.f, lib/object-link-test.f, docs/stdlib.md, lib/std.manifest. Acceptance: ADD records object index, text base, data base, text size, data size; accessors bounds-check indexes; object overflow throws E-OBJ-CAPACITY; existing symbol/reloc validation still passes. Verify object-link test, typed-local-diff-lint, manifest/lints, full suite before master.
