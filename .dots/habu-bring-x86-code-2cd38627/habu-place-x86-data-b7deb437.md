---
title: Place x86 data below 4 GB
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T17:47:58.473741+01:00"
---

Problem: a program's DATA starts at 0x340000000 (src/os/linux/layout.f DATA-VA), so each x86-64 data address is a 10-byte movabs (src/habu/t0-x64.f ADDR) where a 5-byte `mov r32, imm32` reaches anything below 4 GiB, and code addresses, which do, take the movabs too: about 132 KB of the x86 engine. Approach: DATA-VA at 2 GiB, then a 32-bit form for both, which the link patches through its own site kind and which rejects an address out of reach. Acceptance: x86 code shrinks by what the shorter form saves, the engine rebuilds itself byte for byte on x86, and a cross build between targets whose DATA-VA differ still works. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-x-mark.
