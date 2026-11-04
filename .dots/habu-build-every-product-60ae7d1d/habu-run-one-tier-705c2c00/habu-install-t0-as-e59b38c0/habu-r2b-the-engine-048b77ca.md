---
title: "R2b: the engine build hands the host's compilers back after the capture"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T17:17:34.938078+02:00"
closed-at: "2026-10-31T12:12:38.698958+02:00"
close-reason: "Landed with the T0 release (ttloqwsx b17a1ee3): full suite 510/510, two generations converge"
---

Workspace .jj-ws.noindex/jerry-r2b on R1 (ulzktzrp). Brief /tmp/jerry-big/t0-r2b-handback-brief.md: tools/native-build-core.f SAVE-HOST-DISPATCH/RESTORE-HOST-DISPATCH (placeholders) over $358, $3810, $3818, $3838; save before the window and the cache guard, restore after CAPTURE/CAPTURE-NAMED; LOAD-SOURCE-WRITER keeps its tier switch. Proofs: g1, g2 convergence, arm64->x86-64 cross build, prefix-provided-unfilled, check.f.
