---
title: Migrate SAXPY and GEMM evidence rows
status: closed
priority: 1
issue-type: task
created-at: "2026-07-13T16:58:06.392422+02:00"
closed-at: "2026-07-15T02:47:09.861156+02:00"
close-reason: "Merged 1f0d547f on master: tools/eval-triton.f(+test) migrates the shipped competitive claims through the typed BENCH surface as an EXTERNAL consumer (public API only - proves the sealed store suffices, zero TRUSTED): SAXPY-FP32 (64.209 vs 63.0 GB/s exact/exact) and HABU-MMM-TF32 (884.889 vs 1890.5 GFLOP/s rel/rel) import, persist, and replay byte-for-byte to the committed canonical rows/digests; the HISTORICAL Habu-FP32-vs-Triton-TF32 pair is separately-labelled source evidence the checked importer REFUSES (new E-BENCH-INCOMPARABLE -5321, executed negative + resolving positive) - it renders but can never load as a competitive result; lookup-invalidation pinned at this layer (cache-field change -> store miss). docs/eval-triton.md (the real shipped-claims doc; the dot's docs/performance.md never existed) reframes the 4.6-5x 'gap' as incomparable-by-policy source data with TF32-vs-TF32 as the like-for-like result. maki suite + lint-tools (maki-dep) + all lints green on the exact merged tree. COMPLETES the competitive-evidence chain (schema -> persistence -> migration)."
---

Problem: the shipped SAXPY and GEMM competitive rows and their reports still use the legacy loose representation, including a historical Habu FP32 versus Triton TF32 GEMM comparison that is not semantically comparable. Fix: migrate SAXPY FP32 and Habu-MMM/Triton TF32 evidence through the typed BENCH store and typestate report path, with cold and warm cache states explicit; keep the historical invalid pair as separately labelled source evidence that the checked importer rejects, never as a competitive result. Acceptance: migrated rows load, report, and replay byte-stably; every report exposes workload, shape, protocol, baseline, cache state, metric units, numeric policy, artifact identities, and absence reasons; changing any exact-key field invalidates lookup; the historical mismatched-policy pair has a checked negative regression. Files: tools/eval-triton.f, its focused tests, competitive report fixtures, MODEL-CAD-V2-PLAN.md, docs/performance.md, FILEMAP.md. Verify: focused evaluator/report tests, maki/test.f, typed-local diff, host/filemap/dot lints.

Claim: agent=benchmigrate workspace=.jj-ws/fable-benchschema
