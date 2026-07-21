---
title: Migrate competitive schemas
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:38:30.918440+02:00"
---

Invariant: payload cases use named-field ENUM, fixed records use STRUCTURE, and payloadless identity vocabularies remain compact ENUM. The competitive reporting and evidence modules landed after the original unified-type migration census, so their payload SUMTYPE readings and PRODUCT comparison and evidence rows have no exact migration owner. PRODUCT and payload ENUM are semantically different: one is a fixed set of named fields present together; the other selects one named variant whose payload fields depend on the case.

After the unified declaration surface is certified and the hard cutover is ready, migrate throughput readings to payload-bearing ENUM with named units and values, migrate comparison and evidence records and handles to STRUCTURE, and retain compact ENUM only for closed payloadless identities. Preserve constructor package spelling, tag order, field order and widths, wire bytes, pool and handle identity, MATCH behavior, public effects, persistence, and replay compatibility. Delete every legacy declaration token and compatibility alias in this subsystem.

Prove compile-negative semantic-field swaps, wrong payload roles, exhaustive MATCH, construct and destruct round trips, byte-identical benchmark and competitive-evidence version-one encodings, old persisted-row replay, malformed-row rejection, snapshots, ahead-of-time compilation, fixpoint identity, competitive suites, Maki, and full native gates. Measure definition count, JIT, DATA, CODELEN, serialization time, and record storage before and after. Depends on the exact green hard-cutover proof in habu-type-dsl-prove-93da83c4; this dot owns only competitive schema migration.
