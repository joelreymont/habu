---
title: Object text/data sections
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T20:47:33.972112+02:00\\\"\""
closed-at: "2026-07-01T20:51:12.201080+02:00"
close-reason: "completed: added OBJ:TEXT+ and OBJ:DATA+ hex section records, load-time section hex validation, deterministic expected bytes, key coverage, docs and manifest rows. Proof: lib/object-test.f ok; typed-local-diff-lint ok; stdlib-manifest-test ok; filemap-lint ok; host-lint/trust-lint/stale-status-lint ok; lint-artifacts-fast ok; full native suite PASS 17853ms <= 40000ms persistent budget."
---

Problem: lib/object.f now records object metadata but cannot carry actual code/data bytes, so it is not yet a useful pre-link object contract. Fix: add checked section appenders for binary text/data payloads, encoded deterministically as hex records in the object stream, with parser validation on OBJ:LOAD. Files: lib/object.f, lib/object-test.f, lib/std.manifest, docs/stdlib.md. Acceptance: OBJ:TEXT+ and OBJ:DATA+ encode arbitrary bytes, reject malformed loaded section hex, preserve load/serialize round-trip, and affect OBJ:KEY-HEX. Verify: bin/hb --load lib/object-test.f; typed-local-diff-lint; stdlib manifest; filemap/host/trust/stale lints; full native suite before master.
