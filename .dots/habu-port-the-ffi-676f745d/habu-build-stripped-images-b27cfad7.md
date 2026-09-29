---
title: Build stripped images and tools for x86-64
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.808875+03:00"
blocks:
  - habu-resolve-x86-entry-cb671d4d
  - habu-run-bin-hb-6378f297
---

Problem: `src/habu/aot-lib.f:38-39,190-196,560-586` emits an ARM64 minimal entry; `app-image-core.f:11-13` requires the arm64 assembler; `tools/hb-build-lib.f:669` keys on `hb-arm64-v1`; `tools/engine-size.f` and `imgdump.f` read `EM_AARCH64` only.
Acceptance: `AOT-LINK` uses `link-x64.f`'s shared layout and relocation on x86; `hb-build` (whose maker child is the product engine, `tools/hb-build-lib.f:574-580`) produces a stripped x86 image that runs; the `stripped-*`, `stripped-does`, `native-resource-image` and `hb-build-fixtures` rows green on the ThinkPad; a per-target compiler-ABI key; both readers accept `EM_X86_64`.
Files: `src/habu/aot-lib.f`, `src/habu/app-image-core.f`, `src/habu/link-x64.f`, `tools/hb-build-lib.f`, `tools/engine-size.f`, `tools/imgdump.f`.
Verify: ThinkPad: the `stripped-*`, `stripped-does`, `native-resource-image` and `hb-build-fixtures` rows; spark gate.
Depends: habu-resolve-x86-entry-cb671d4d (X4d), habu-run-bin-hb-6378f297 (X6).
Route: Alder (shared: src/habu/aot-lib.f, src/habu/app-image-core.f, tools/hb-build-lib.f, tools/engine-size.f, tools/imgdump.f).
Ownership: krait (Intel lane).
Claim: unassigned.
