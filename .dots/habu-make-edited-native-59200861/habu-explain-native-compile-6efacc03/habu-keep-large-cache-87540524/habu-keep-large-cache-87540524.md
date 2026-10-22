---
title: Keep large cache diagnostics usable
status: open
priority: 2
issue-type: task
created-at: "2026-10-19T13:02:18.010733+03:00"
---

A real native self-build with HABU_NATIVE_CACHE_DIAG=1 reuses support but exits E-PROC-TRUNCATED before completion because per-definition stderr diagnostics overflow the outer builder capture. Without diagnostics the same command can proceed. Bound and repair diagnostic delivery at the responsible process/output boundary; do not silently drop lines or raise arbitrary fixed limits. Existing real engine replay is the acceptance path.
