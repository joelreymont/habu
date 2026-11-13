---
title: "Link a build's space for arm64 (step 5 S6)"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T15:11:55.348222+02:00"
---

Problem: step 5's arm64-to-arm64 cross-build writes the space LINKER:WITH opens as an arm64 program through one linker. Stripped arm64 programs are written and run; writing hb's dictionary is the child habu-s6-l6-write-b8a1a80d (step 6). Acceptance: S7's stripped app, cross-built in-process, runs on Linux arm64; it proves the linker's Linux file writer and runs when Linux builds move onto the linker. A product file that requires a file the build itself has loaded (debug.f requires asm.f, which the build loads) gets that file in the product's own space; the in-process capture cannot (a window debugger calls A64ASM:ENC-B in the prelude band and the capture dies with exit 74). src/lib/error.f calls util.f's ERROR-TEXT without requiring src/core/util.f (its comment at :9); when the capture window goes, error.f requires util.f and that comment goes. Depends: none. Claim: agent=carl.
