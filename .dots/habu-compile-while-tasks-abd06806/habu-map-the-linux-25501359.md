---
title: Map the Linux arm64 region through two memfd views
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T20:32:19.668405+02:00"
---

W^X lane W3L, stacked on W2 (oustvtmq) in .jj-ws.noindex/carl-wx, brief carl-wx/.out/brief-wx-3l.md. W3 deletes the mprotect flips that macOS and Linux arm64 share (habu1.f/habu2.f), so Linux needs two views first: memfd_create (MFD_EXEC, retry without on EINVAL), RX MAP_SHARED|MAP_FIXED at the reserved base, RW alias, delta in REGION-ALIAS:DELTA-CELL; the raw-fork child re-owns the region (new memfd, copy the used extent, MAP_FIXED alias at the parent's alias address and RX over DBASE). Proofs on spark (ssh spark, aarch64 Linux 6.17; Joel's pick 2026-10-29): baseline master Linux engine, then boot + /proc maps, code-window, task-test, process-fork-test, snapshot and debugger rows, self-rebuild, spawn cost.
