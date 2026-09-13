---
title: "Produce and measure an optimized native compiler"
status: open
priority: 1
issue-type: task
created-at: "\"2026-09-11T16:38:06.278925+03:00\""
blocks:
  - habu-wire-the-checker-eec26aea
  - habu-select-optimizing-compilation-cf2b21d4
  - habu-check-arena-append-c7b1e040
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own selfbuild integration and seed/bootstrap documentation; earlier leaves own source interfaces. Use identified current native optimizer and corrected source-owner checker/compiler pair; pending tier91fff115 is unfinished. Verify all compiler/helpers compiled through NCOMP using provenance and representative disassembly, JIT entry denied before dependencies, then product-hosted rebuild. No ancient stdin refresh/fake setter/interpreter/JIT rescue prerequisite. Record first uncached all-AOT Tender3079/3079 baseline, full executable wall time, floor and perdef/scaling rows. Historical131.6s/3668us were JIT-built compiler timings. This baseline does not automatically satisfy1.7s.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.
