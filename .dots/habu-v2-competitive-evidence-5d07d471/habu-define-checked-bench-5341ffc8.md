---
title: Define checked BENCH comparison schema
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-13T16:58:06.379273+02:00\\\"\""
closed-at: "2026-07-15T01:40:48.429046+02:00"
close-reason: "Merged 9ee1f678 on master: maki/competitive-report.f package BENCH - nominal workload/shape/protocol/baseline id families (slot swaps reject statically), closed cache-state/absence sums, UNIT-TYPED readings (gbps/gflops distinct sum families carrying value-or-named-absence, so a GFLOP/s value cannot fill a GB/s slot), comparison-gbps/-gflops products whose subject AND baseline carry NPOL:dom witnesses with COMPARABLE? requiring witness equality - the historical Habu-FP32-vs-Triton-TF32 pair rejects (0) with resolving positives (-1). Canonical bench/v1 renders byte-golden for real device rows (SAXPY 64.209 GB/s vs Triton 63.0; MMM TF32 884.889 vs 1890.5 GFLOP/s); 11 per-field key-alteration cases; absences render na:<reason>; E-BENCH-CAP/-ROW -5257/-5258 (the dot-suggested block was owned by maki/target - error-code-lint caught it). Zero TRUSTED surface: the parametric comparison<a> spelling was audited and deliberately replaced by concrete per-unit families (cell-tier-only product params would need trusted unit witnesses AND weaken value typing - documented deviation + LESSONS). 106-suite maki + all lints + lint-manifest green on the exact merged tree. Unblocks persist-typed-bench 2d15efa2."
---

Problem: competitive rows are opaque strings/raw n, and historical Habu FP32 versus Triton TF32 data can masquerade as one comparable result. Fix: add package BENCH in maki/competitive-report.f with nominal workload/shape/protocol/baseline ids, closed cache/absence/metric unit sums, and comparison<a> whose subject and baseline carry the same concrete numeric-policy witness; add canonical versioned RENDER only, no store or docs migration. Acceptance: FP32/TF32, identity-slot, raw-n, cache-state, and throughput-unit confusions reject with verdict 0 plus resolving positives; SAXPY FP32 and Habu-MMM/Triton TF32 rows render byte-stably; cold/warm and every exact-key field alter the key; unavailable metrics name their reason; capacity throws named E-BENCH codes. Files: maki/competitive-report.f, maki/competitive-report-test.f, maki/test.f, FILEMAP.md. Verify: exact test, maki/test.f, typed-local diff, host/filemap/dot lints.

Claim: agent=benchschema workspace=.jj-ws/fable-benchschema
