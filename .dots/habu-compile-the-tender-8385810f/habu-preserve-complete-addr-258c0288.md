---
title: "Preserve complete address rows through artifact IO and merge"
status: open
priority: 1
issue-type: task
created-at: "2026-09-13T14:51:13.059918+03:00"
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own aot-file.f row lengths/bases/counts/merge/version, aot-decl.f row contract and exact-row/MERGE fixtures. XTOFF-ROW=8 while IO uses4; cleared-buffer reproduction loses second row yet says roundtrip=ok. Use shared width; preserve fixed/window location and CODE/DATA target tags plus nullable offset+1. Shift window locations and nonnull targets by proper merged bases, checking range before publication. Reject truncated rows/incompatible old version. Verify fresh-buffer exact rows, both kinds/null/fixed cells, real merge and restored execution.25770093 owns other negative reader cases.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.
