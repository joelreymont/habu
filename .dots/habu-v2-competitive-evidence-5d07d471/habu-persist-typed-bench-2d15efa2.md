---
title: Persist typed BENCH comparison rows
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-13T16:58:06.385858+02:00\\\"\""
closed-at: "2026-07-15T02:18:18.825112+02:00"
close-reason: "Merged a3f32118 on master: maki/competitive-store.f - the only public writers are typed BENCH-PUT-GBPS/-GFLOPS; the raw appender is package-BENCH-private (bypass regression: qualified BENCH:BENCH-ROW-APPEND verdict 1 unresolvable, read + typed-write + encode controls -1). Row = canonical render (the exact key - every field participates) + schema=bench/v1 + independent policy/unit re-encodings + FNV-1a-64 digest + promo=COMPARABLE? verdict. Eleven named reject classes -5310..-5320 all proven firing (fields/label/token/dup/schema/kind/policy/digest/canon/promo), each resolving against the clean golden; byte-for-byte round-trip goldens for gbps/gflops/absent-baseline rows. Zero TRUSTED surface. Documented deviation: cross-artifact linkage to a specific ART:promoted grant is the promotion lane's scope - the promo field records/re-checks the in-scope comparability evidence. maki suite + all lints + error-code green on the exact merged tree. Unblocks migrate-saxpy 2c02dd5a (the final competitive-chain leaf)."
---

Problem: competitive evidence is currently persisted through raw strings and untyped numeric fields, so a writer can bypass the BENCH comparison schema or replay a row under the wrong policy, kind, or key. Fix: add a checked canonical codec and store records for BENCH comparison values after the R7 store and promotion contracts are sealed; the only public writer accepts the typed comparison value and derives its exact key, schema version, digest, and promotion evidence. Acceptance: cross-policy, cross-kind, wrong-schema, duplicate-field, noncanonical, digest-mismatch, stale-promotion, and direct raw-writer fixtures reject with named E-BENCH codes; a valid row round-trips and replays byte-for-byte; no public raw persistence path remains. Files: new maki/competitive-store.f, maki/competitive-store-test.f, maki/test.f, FILEMAP.md. Verify: exact test, maki/test.f, typed-local diff, host/filemap/dot lints.

Claim: agent=benchstore workspace=.jj-ws/fable-benchschema

NOTE 2026-07-18 (run-identity landing 785c4021): this dot also owns the
metric UNITS vocabulary (the run-metric package models population/
direction/aggregation; units deferred pending an owner - decide the
closed vocabulary here where the evidence matrix consumes it).
