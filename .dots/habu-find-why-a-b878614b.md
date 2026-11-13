---
title: Find why a fresh kernel inputs log fails to hold beside concurrent kernel builds
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T11:21:51.811503+02:00"
---

Problem: in a full suite run on a new engine (4e7a8884's, .out/obj fresh), arm64-link-run's 'the fresh inputs log holds' failed (test/compiler/arm64-link-run.f CHECK-LR: LINKER:INPUTS-HOLD? false on HB-BUILD-CORE:KERNEL-INPUTS$) 33 s in, while arm64-link-data, arm64-place and file-objects were building the host kernel into the same <cwd>/.out/obj/<stamp>/aarch64-apple-darwin/kernel/<cwd>/ directory; it passes alone and in a four-test run. Concurrent builds in one tree share that directory, so a warm build may adopt or replay another build's log. Acceptance: the interleaving that makes a build's own fresh log not hold is named with a reproducer that fails every time, and its fix makes the check hold under any interleaving. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-race
