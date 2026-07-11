---
title: V2 competitive evidence matrix
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:14:24.933792+02:00"
---

Problem: MODEL-CAD-V2-PLAN.md:1376-1395 and 1530-1545 require non-cherry-picked exact-key comparisons, but current docs/eval-triton.md rows are manually assembled and do not share one versioned evidence schema. Fix: add the first checked report schema for workload/revision/shape/numeric-policy/target/compiler/cache/protocol plus baseline identity and latency/throughput/bytes/launches/memory/energy fields; migrate one SAXPY and one GEMM row. Acceptance: mismatched numeric domains cannot share a comparison row; cold/warm states are explicit; replay renders byte-stable output. Files: maki/report.f, maki/store.f, docs/eval-triton.md, MODEL-CAD-V2-PLAN.md:1376-1395. Verify: focused report/store tests, maki/test.f, host-lint, filemap-lint.
