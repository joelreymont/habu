---
title: "Index address cells at the owning registrar"
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T11:09:21.785913+03:00"
blocks:
  - habu-size-the-snapshot-1ca5db10
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own habu2.f EMIT-MARK lookup, layout.f derived index and native-build.f ADDR-ROWS!/KEEP-ROWS-BELOW invalidation only. Index exact DATA byte offsets into ordered rows. Same-kind duplicate no-op; conflict/capacity/range refuse before store; unaligned cells legal. Rebuild at restore/reset/compaction; persist no scratch pointer/stale ordinal. Test collisions, both kinds, unaligned/boundary, compaction/restore,40000 cells and byte identity. Count probes across sizes and all-AOT full-build pair. Quadratic scan proven; wall share unmeasured. No second relocation representation.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.
