---
title: "Infer alloc: backing microbenchmark"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.675252+02:00"
blocks:
  - habu-infer-alloc-class-b2201920
  - habu-infer-m0-schema-27e8ca5d
---

Why this exists:
candidate backing choices need one reproducible benchmark that applies each class's actual processor and graphics-processor access pattern.

Required result:
measure prefaulted file mapping, registered/advised system memory, and CUDA allocation where valid, recording cold faults, warm bandwidth/latency, peak memory, and cleanup.

Done when:
identical byte ranges and workload are used per candidate; correctness is checked before timing; unavailable candidates remain explicit.

Expected touch points: new tools/infer-bench/allocation-class.f, focused test, FILEMAP.md.
Smallest check: fixture benchmark test and presence-gated GB10 smoke.
Prerequisites: class contract table and M0 schema.
Owned result: allocation backing harness only.
Claim: unassigned.
