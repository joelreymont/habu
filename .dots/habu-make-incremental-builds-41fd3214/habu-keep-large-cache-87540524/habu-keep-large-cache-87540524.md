---
title: Keep large cache diagnostics usable
status: open
priority: 2
issue-type: task
created-at: "2026-10-19T13:02:18.010733+03:00"
---

A real native self-build with HABU_NATIVE_CACHE_DIAG=1 reuses support but exits E-PROC-TRUNCATED before completion because per-definition stderr diagnostics overflow the outer builder capture. Without diagnostics the same command can proceed. Bound and repair diagnostic delivery at the responsible process/output boundary; do not silently drop lines or raise arbitrary fixed limits. Existing real engine replay is the acceptance path.

Review lane A finding 3 (2026-10-22) is this defect: HABU_NATIVE_CACHE_DIAG=1 kills a stripped/AOT build with uncaught -2504 E-PROC-TRUNCATED, rc 67; tools/hb-build-lib.f:55 caps maker stderr at a fixed 65536 bytes. The gate row of habu-gate-the-recompile-2ec826c7 runs with the variable set, so it needs this fixed.
