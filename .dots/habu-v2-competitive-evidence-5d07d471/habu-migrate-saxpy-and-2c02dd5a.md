---
title: Migrate SAXPY and GEMM evidence rows
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:58:06.392422+02:00"
blocks:
  - habu-persist-typed-bench-2d15efa2
  - habu-v2-typestate-report-df8e34fa
---

Problem: the shipped SAXPY and GEMM competitive rows and their reports still use the legacy loose representation, including a historical Habu FP32 versus Triton TF32 GEMM comparison that is not semantically comparable. Fix: migrate SAXPY FP32 and Habu-MMM/Triton TF32 evidence through the typed BENCH store and typestate report path, with cold and warm cache states explicit; keep the historical invalid pair as separately labelled source evidence that the checked importer rejects, never as a competitive result. Acceptance: migrated rows load, report, and replay byte-stably; every report exposes workload, shape, protocol, baseline, cache state, metric units, numeric policy, artifact identities, and absence reasons; changing any exact-key field invalidates lookup; the historical mismatched-policy pair has a checked negative regression. Files: tools/eval-triton.f, its focused tests, competitive report fixtures, MODEL-CAD-V2-PLAN.md, docs/performance.md, FILEMAP.md. Verify: focused evaluator/report tests, maki/test.f, typed-local diff, host/filemap/dot lints.
