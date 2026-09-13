---
title: "Size the address table for the complete Tender closure"
status: open
priority: 1
issue-type: task
created-at: "2026-09-12T10:41:22.834193+03:00"
blocks:
  - habu-build-engine-layout-abdd0188
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own layout.f XTCELL/AOT capacity, habu2.f capacity refusal and fixture only. Rebase reviewed pending0d78b97f; it is not integrated. Measure runner-inclusive closure/headroom before65536 bound; old19088 comment is stale. Verify40000 declarations exceed old32768, exact capacity refusal count/cap/newline, first-generation actual versus advertised limit and complete Tender build. Corrected native layout is prerequisite; ancient seed refresh is not. Registrar index follows this layout contract.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.
