---
title: Load checker extension in recovery prefix
status: active
priority: 2
issue-type: task
created-at: "2026-10-15T00:39:57.055657+03:00"
---

The new-core bootstrap-engine-stack existing E2E exits 70 before BES-OK: CHECKER-OWNER-EXT:UNIT-EXPORT-REFS-OFF is undefined. test/nf.fs loads bootstrap/cg/forth.fs; its PFX-FILES emits checker-fetch-abi then checker-owner-abi, omitting checker-owner-extension. Native source, bootstrap launcher and build-fixpoint already require extension before ABI. Fix the recovery prefix source-label and shared PFX-COMMON row at the responsible boundary; preserve one table for load/path/provide projections. Verify existing bootstrap-engine-stack and the affected recovery consumer, then independent Astra review. No fake ABI constant or native cache workaround.
