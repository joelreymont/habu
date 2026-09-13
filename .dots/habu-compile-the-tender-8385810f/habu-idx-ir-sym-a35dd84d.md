---
title: "Index symbols with owned collision-safe lookup"
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T18:11:06.102488+03:00"
blocks:
  - habu-honour-the-committed-615f47a9
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own ir/symbol.f private index/NEW/NEW-FROM/INTERN, build.f ownership plumbing and lifetime tests. Keep insertion-ordered rows/bytes authoritative. Context-owned mutable hash buckets yield candidate ordinals, then full length/byte comparison. Validate arena generation/state before index dereference; invalidate before teardown. Clone without mutable alias; reserve index/row/byte growth before publication; duplicate hits allocate nothing. Preserve owner/stale/frozen/capacity and failed-insertion semantics. Verify collisions, probe scaling, clone memory/setup, slot reuse/teardown. No arbitrary write API in append-only IR; all-AOT floor measured at campaign acceptance.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.
