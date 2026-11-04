---
title: Build x86-64 files through the linker
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T14:09:36.119795+02:00"
---

Joel 2026-11-03: "we don't have the loader anymore. you need to implement the linker and then switch x86 (then wasm?) over to it". Today every x86-64 application build, hb-build and incremental file build stops at SOURCE-EXEC:RUN ('hb: source-exec-run is not in the x86-64 kernel', kernel-x64.f :2674-2679 REFUSE-WID; about 35 suite rows, closed dot habu-give-the-x86-4f58a6ff lists them). The observer is the old cache's way of watching a file's re-run; the linker replaces it, so x86-64 gets no observer: once the linker lands on arm64, x86-64 files link through it (the design's second CODE row per machine, design-linker.md :200-202). Then SOURCE-EXEC goes whole: package src/habu/source-exec.f, the six engine rows, the frame cells (layout.f :1919-1945, SOURCE-EXEC-CTX-CELL) and outer.f's observer half (:592-722). Whether Wasm follows is open: tools/wasm-build.f builds modules through tier 1 and uses no file objects today. Depends: the linker on arm64. Claim: agent=jerry, not started.
