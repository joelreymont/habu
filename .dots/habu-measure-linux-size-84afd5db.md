---
title: Measure Linux size-attribution per-region rows
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T22:28:40.640839+02:00"
---

test/gate-size-attribution-test.f commits the macOS per-region byte decomposition (header, code-text, text-pad, __DATA_CONST, __LINKEDIT, signature) measured at the byte-fixpoint, and couples both targets' committed totals to the live engine. The Linux per-region split (header, code-text, text-pad, rw-segment; distance-to-page-floor) is not yet captured because it needs a Linux host. On the Orin: build the stdin engine, run the metabuild host with HABU_ENGINE_SIZE_MAP=1 to capture a map, run tools/size-report.f to reconcile it against the linux bin/hb, then add the Linux constants (LINUX-CODE-TEXT, LINUX-RW=192, LINUX-FLOOR-DIST, per-region rows) to test/gate-size-attribution-test.f and extend RUN/VALIDATE to check the Linux decomposition (today only the Linux total is coupled).
