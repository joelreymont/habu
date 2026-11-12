---
title: "Declaring a callback in a cross-build reads the builder's memory at the target's address"
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T08:59:28.770124+02:00"
---

Problem: src/lib/task.f ROW-GROW (:958-971) and src/lib/ffi-callback.f GROW (:113) address main through MAIN-BASE (the target's DATA-VA) while a declaration runs inside the build: a program with one CALLBACK: builds natively and throws E-TASK-STATE (-3604) with --target linux-aarch64 on macOS. Brief .jj-ws.noindex/carl-layout/.out/briefs/fix-callback-declare-cross.md. Acceptance: the program builds for linux-aarch64 and its callback runs on Linux arm64 as natively. Depends: habu-a-cross-build-5bc5c122 (the build stops there next). Claim: agent=carl workspace=.jj-ws.noindex/carl-cb (commit 6fca1734 waits for its run on Linux)
