---
title: Size the address table for the complete Tender closure
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-12T10:41:22.834193+03:00\""
blocks:
  - habu-build-engine-layout-abdd0188
---

Plan: [PLAN.md](../../PLAN.md). Claim: Cedar.

Own layout.f XTCELL/AOT capacity, habu2.f capacity refusal and fixture only. Rebase reviewed pending0d78b97f; it is not integrated. Measure runner-inclusive closure/headroom before65536 bound; old19088 comment is stale. Verify40000 declarations exceed old32768, exact capacity refusal count/cap/newline, first-generation actual versus advertised limit and complete Tender build. Corrected native layout is prerequisite; ancient seed refresh is not. Registrar index follows this layout contract.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

2026-09-13: current tracked B2 starts with 30885 rows (30887 after APP-IMAGE
support loads); real SAVE exits96 at32768. Ported the cap/encoding fix without
the obsolete second-generation workaround:65536 rows, full-width cap loads,
one contiguous diagnostic including the cap and newline, and section reservation
increased by32768*8 bytes. Relocation proof/source and assembler suites pass.
New real-load tests require40000 total rows and exact advertised capacity,
including duplicate registration at the bound. First rebuilt product, complete
capture/Tender closure measurements and full gate are still pending.
