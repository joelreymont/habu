---
title: Define checked BENCH comparison schema
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:58:06.379273+02:00"
blocks:
  - habu-v2-numeric-policy-6f639843
---

Problem: competitive rows are opaque strings/raw n, and historical Habu FP32 versus Triton TF32 data can masquerade as one comparable result. Fix: add package BENCH in maki/competitive-report.f with nominal workload/shape/protocol/baseline ids, closed cache/absence/metric unit sums, and comparison<a> whose subject and baseline carry the same concrete numeric-policy witness; add canonical versioned RENDER only, no store or docs migration. Acceptance: FP32/TF32, identity-slot, raw-n, cache-state, and throughput-unit confusions reject with verdict 0 plus resolving positives; SAXPY FP32 and Habu-MMM/Triton TF32 rows render byte-stably; cold/warm and every exact-key field alter the key; unavailable metrics name their reason; capacity throws named E-BENCH codes. Files: maki/competitive-report.f, maki/competitive-report-test.f, maki/test.f, FILEMAP.md. Verify: exact test, maki/test.f, typed-local diff, host/filemap/dot lints.
