---
title: "A slot claim in a cross-build reads the builder's memory at the target's address"
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T08:53:58.582593+02:00"
---

Problem: src/lib/task.f:1821 USER-NEXT reads the live-task count through MAIN-BASE (task.f:372, the target's DATA-VA); a claim runs inside the build, so a --target linux-aarch64 engine build on macOS reads host memory at Linux's DATA-VA and throws E-TASK-USER (hb-build: src/lib/c2-owner.f threw -3605). Reproducer /tmp/jerry-linux-3605.md; cause .jj-ws.noindex/carl-layout/.out/band3-cause.md. It blocks jerry's Linux evidence. Acceptance: the linux-aarch64 engine build ends rc 0 and its hb runs on Linux arm64; test/run.f task process-task pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-band3
