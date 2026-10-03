---
title: "Keep the maker's literal out of AOT images"
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T10:28:49.711798+02:00"
---

Problem (review 261 of spanrow 6000470d): tools/aot-build.f:8 LOAD-CORE's `s" tools/aot-build-core.f"` literal is interned into the application's NSTR pool (opened inside the capture window by tools/aot-build-open.f, src/habu/aot-window-latch.f:64-70) before LOAD (:10-14) switches to the linker's pool, so every stripped image a production engine-run maker builds (tools/hb-build-lib.f:572-575 compiles the linker per build) carries that 22-byte string in its literal arena: span program data 508 bytes in 91 cells on the engine vs 483 in 88 on the linker image (decoder and cell dumps: $HOME/.cache/tmp/kestrel-r4-rev261/decode.py, span-img.cells, span-eng.cells). On the linker image the require is a no-op, so nothing leaks there. Acceptance: a failing row first through the real build path; the span program built by the engine-run maker has data 483 in 88 cells with a cell stream identical to the linker-image build; no maker literal in any engine-built image's arena (find the general cause: any literal compiled by the maker while the application pool is active, not only this one); hb-build-aot-test rc 0. Base: after the master merge (aot-window-latch.f changed on master).
