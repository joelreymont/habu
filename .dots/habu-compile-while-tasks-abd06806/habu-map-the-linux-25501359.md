---
title: Map the Linux arm64 region through two memfd views
status: closed
priority: 2
issue-type: task
created-at: "2026-10-29T20:32:19.668405+02:00"
closed-at: "2026-10-30T17:25:07.705622+02:00"
close-reason: "landed on master: 679c7755 'Map the region through two views' and efc74616 'Let main compile while tasks run'; e11 engine g1==g2, e10 suite 498/498, overlap rows rc 0 on e11, spark route5 g2==g3; installed"
---

W^X lane W3L, stacked on W2 (oustvtmq) in .jj-ws.noindex/carl-wx, brief carl-wx/.out/brief-wx-3l.md. W3 deletes the mprotect flips that macOS and Linux arm64 share (habu1.f/habu2.f), so Linux needs two views first: memfd_create (MFD_EXEC, retry without on EINVAL), RX MAP_SHARED|MAP_FIXED at the reserved base, RW alias, delta in REGION-ALIAS:DELTA-CELL; the raw-fork child re-owns the region (new memfd, copy the used extent, MAP_FIXED alias at the parent's alias address and RX over DBASE). Proofs on spark (ssh spark, aarch64 Linux 6.17; Joel's pick 2026-10-29): baseline master Linux engine, then boot + /proc maps, code-window, task-test, process-fork-test, snapshot and debugger rows, self-rebuild, spawn cost.
