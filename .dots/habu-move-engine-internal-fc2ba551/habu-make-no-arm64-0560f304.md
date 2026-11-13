---
title: Make no arm64-only package in a Wasm build
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T23:00:56.829230+01:00"
blocks:
  - habu-lay-a-private-94bc42e7
---

Problem: a Wasm build makes the FFI and TASK packages from hb's own primitive table, packages of arm64 kernel entries no Wasm module can call: STATES-PRIMS? is false on Wasm (src/habu/linker.f), so OWNED-TEXTS and OWNED-RECORDS make them, and tools/hb-build-core.f APP-FILES skips their files on Wasm only because of them. Acceptance: a Wasm build's namespace holds neither package and APP-FILES has no WASM? skip. Depends: habu-lay-a-private-94bc42e7 (the open's private records). Claim: unassigned.
