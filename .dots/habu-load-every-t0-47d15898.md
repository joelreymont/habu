---
title: Load every T0 emitter from runtime.f
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T11:20:28.974655+01:00"
---

Problem: src/habu/runtime-arm64.f loads the arm64, x86-64 and Wasm T0 emitters and t0.f for every target (tools/hb-build-core.f:198 RUNTIME-FILES), so its name says arm64 where it serves every machine, and dot habu-move-the-arch-e2e2c1d0 would move it to src/arch/arm64/. Acceptance: its requires move into src/habu/runtime.f ahead of t0.f, the file and its RUNTIME-FILES line are gone, e2e2c1d0 no longer moves it, and the engine build converges. Depends: none. Claim: unassigned.
