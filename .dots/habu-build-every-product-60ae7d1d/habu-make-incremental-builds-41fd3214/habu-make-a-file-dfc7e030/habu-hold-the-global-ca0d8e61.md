---
title: Hold the global and package wids in cells
status: open
priority: 2
issue-type: task
created-at: "2026-11-04T13:51:16.732492+02:00"
---

Step 3a, lane wlcell. BOOT-LAYOUT:GLOBAL-WL-CELL $7F0 and NAMESPACE-WL-CELL $7F8, written 0 and DICT-WL:NAMESPACE by every arm64 boot and copied into task regions; habu1.f LFIND, LHEADROW, BPOLICYADMIT and the package-row tests (FORK walk, LRECLAIM, LMATCH, LRETIRE) read them. Waits on habu-bound-dp-by-365f1f4a landing. Workspace .jj-ws.noindex/jerry-wlcell; brief /tmp/jerry-big/wlcell-brief.md.
