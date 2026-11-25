---
title: Place x86 data below 4 GB
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T17:47:58.473741+01:00"
---

Problem: an x86-64 program's DATA starts at 0x340000000, so each data address is a 10-byte movabs where a 5-byte mov r32, imm32 would do below 4 GB; tenderd holds 18,144 of them, about 75 KB. The link patches the immediate, so imm32 needs its own site kind (src/habu/t0-x64.f, src/habu/link-kernel.f). Acceptance: tenderd's x86 code shrinks by what the shorter form saves, and the engine rebuilds itself byte for byte on x86. Depends: none. Claim: unassigned.
