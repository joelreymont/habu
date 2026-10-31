---
title: Stop the stripped AOT scan refusing byte data by mapping layout
status: open
priority: 1
issue-type: task
created-at: "2026-10-31T19:53:59.141641+02:00"
---

kevin 2026-10-31: hb-build's stripped AOT persistent-data scan refuses Tender's server image in 2 of 6 identical builds: 'stripped AOT persistent data holds a pointer into memory the build mapped word=<unknown> data-off=18452731 value=4311810305' (0x0101010101 at an odd offset), exit 70. SCAN-DATA-CELL (src/habu/aot-common.f:178) appears to read a cell at byte offsets over byte data with no owner; AOT-CELL:MAPPED? (aot-data-cell.f:294) is true only when the value lands in a PROC-MAPS area, so the result follows ASLR. Fix at the root (which data is scanned as cells is decided by structure, not by the value or the mapping layout), with a regression test that fails deterministically. kevin makes the change in .jj-ws.noindex/kevin-aotscan and hands it to carl.
