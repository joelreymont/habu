---
title: T0 keeps pre-window created words as calls in an AOT window
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T17:17:34.956409+02:00"
---

Workspace .jj-ws.noindex/jerry-b0 on master e49421f0. Brief /tmp/jerry-big/t0-b0-prewindow-brief.md: t0.f WORD-CALL folds a DKIND:ADDR word only when its entry is at or above AOT-CELLS:B0-CELL (the JIT's rule, habu2.f faddr). Repro /tmp/jerry-big/b0/prewindow.f (T0 refuses 'recorded address site outside both window spans', JIT captures) and test/aot-window-base-call.f under test/t0-child.f. Closes habu-keep-pre-window-ab33f2dc when it lands.
