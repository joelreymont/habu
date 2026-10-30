---
title: "R2b: the engine build hands the host's compilers back after the capture"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T17:17:34.938078+02:00"
---

Workspace .jj-ws.noindex/jerry-r2b on R1 (ulzktzrp). Brief /tmp/jerry-big/t0-r2b-handback-brief.md: tools/native-build-core.f SAVE-HOST-DISPATCH/RESTORE-HOST-DISPATCH (placeholders) over $358, $3810, $3818, $3838; save before the window and the cache guard, restore after CAPTURE/CAPTURE-NAMED; LOAD-SOURCE-WRITER keeps its tier switch. Proofs: g1, g2 convergence, arm64->x86-64 cross build, prefix-provided-unfilled, check.f.
