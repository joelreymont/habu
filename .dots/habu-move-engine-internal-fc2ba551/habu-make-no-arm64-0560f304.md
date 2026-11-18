---
title: Make no arm64-only package in a Wasm build
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T23:00:56.829230+01:00"
closed-at: "2026-11-18T16:07:04.910930+01:00"
---

Problem: a Wasm build makes the FFI and TASK packages from hb's own primitive table, packages of arm64 kernel entries no Wasm module can call: OBJECT-ROWS? is false on Wasm (src/habu/linker.f), so PRIMS-IN states hb's own rows and OWNED-RECORDS makes them, and tools/hb-build-core.f APP-FILES skips their files on Wasm only because of them. Acceptance: a Wasm build's namespace holds neither package and APP-FILES has no WASM? skip. Depends: none. Claim: unassigned.
