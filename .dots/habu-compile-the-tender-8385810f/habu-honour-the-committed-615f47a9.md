---
title: Honor committed ceilings on symbol prototype clones
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T18:11:06.107844+03:00"
blocks:
  - habu-check-arena-append-c7b1e040
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned (stale claim cleared 2026-09-16).

Own symbol.f NEW-FROM ceiling parameters, build.f SYM-NEW and ir-symbol/ir-build tests. Pass P-SYMS/P-SBYTES for clones; refuse smaller-than-prototype occupancy before mutation/allocation. Verify zero/small/exact fit, future insertion ceiling, duplicate at capacity and independent clone. This establishes constructor contract before index ownershipa35dd84d; no implicit CAP-MAX/BYTE-MAX override.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Integration: c2d230b8. Cedar independently reviewed NEW-FROM preallocation bounds and every caller, then reran ir-symbol and ir-build on the freshly rebuilt private candidate05fd7864: both pass. Combined compiler/runtime gate remains pending; no speed result claimed.
