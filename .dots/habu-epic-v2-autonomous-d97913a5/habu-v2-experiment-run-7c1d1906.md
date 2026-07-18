---
title: V2 experiment run identity
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-11T12:25:27.690944+02:00\\\"\""
closed-at: "2026-07-18T05:22:59.092221+02:00"
close-reason: "Run identity landed (785c4021): 13-field interned key, static train/held-out separation, typed license/authority rejects, deterministic batch ids, lineage resume proven. Units vocabulary folded into competitive-evidence."
blocks:
  - habu-v2-canonical-artifact-ee5121b4
---

Implement MODEL-CAD-V2-PLAN.md:1672-1689 immutable dataset/split/preprocess/seed/model/optimizer/numeric/target/compiler/environment run keys and typed metric populations. Acceptance: every semantic mutation changes the run id, equal keys resume the same lineage, held-out test metrics cannot be consumed as training objectives, missing license/authority rejects, and deterministic next-batch identity is pinned. Files: maki/experiment/run.f plus focused tests.

Claim: agent=exprun workspace=.jj-ws/fable-exprun (owns new maki/experiment/run.f + tests)

RESOLVED 2026-07-18 (exprun lane, commit 785c4021): ACCEPTANCE MET.
Package RUN owns CAD-KIND:run-id (sanctioned kind + audited pair +
seed, per precedent): 13 digest-covered fields over landed owners
(conservative readings documented: compiler/environment as config-id
pending a toolchain owner; license/authority as content keys - the
plan under-specifies the license model; dataset/model as artifact-id);
interned identity (equal builds -> one id -> LIN-RESUME lineage across
rebuild, journal-composed). Population separation: report-metric vs
objective-metric as never-unifying flat families - held-out-as-
objective is STATICALLY untypeable + the sole PROMOTE-OBJECTIVE bridge
dynamically rejects non-train populations. All five acceptance legs
test-proven incl. the 13-field flip matrix and deterministic
BATCH-ID(run-key||k) both orders. Deferred, documented in-source:
metric UNITS (needs an owner vocabulary) - folded into
habu-v2-competitive-evidence-5d07d471 (the metrics-matrix consumer).

NOTE 2026-07-18 (compev lane): the deferred metric UNITS vocabulary now
EXISTS as a sealed closed `unit` enum {ns, ms, gflops, gbps, bytes,
count, watts} in maki/competitive-evidence.f (package CEVID), the
competitive-evidence matrix consumer. It is owned by the matrix schema
that consumes it (not wired into run-metric.f MEASURE, which would change
that word's signature and every caller); run-metric.f's population/
direction/aggregation axes are unchanged. joules is deliberately excluded
(no measured energy in the corpus). Status stays closed.
