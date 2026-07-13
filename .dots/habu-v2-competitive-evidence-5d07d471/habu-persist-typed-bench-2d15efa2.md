---
title: Persist typed BENCH comparison rows
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:58:06.385858+02:00"
blocks:
  - habu-define-checked-bench-5341ffc8
  - habu-v2-typestate-promotion-2266b236
  - habu-v2-typestate-store-57afdc0a
---

Problem: competitive evidence is currently persisted through raw strings and untyped numeric fields, so a writer can bypass the BENCH comparison schema or replay a row under the wrong policy, kind, or key. Fix: add a checked canonical codec and store records for BENCH comparison values after the R7 store and promotion contracts are sealed; the only public writer accepts the typed comparison value and derives its exact key, schema version, digest, and promotion evidence. Acceptance: cross-policy, cross-kind, wrong-schema, duplicate-field, noncanonical, digest-mismatch, stale-promotion, and direct raw-writer fixtures reject with named E-BENCH codes; a valid row round-trips and replays byte-for-byte; no public raw persistence path remains. Files: new maki/competitive-store.f, maki/competitive-store-test.f, maki/test.f, FILEMAP.md. Verify: exact test, maki/test.f, typed-local diff, host/filemap/dot lints.
