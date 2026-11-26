---
title: Load x86 data addresses in five bytes
status: closed
priority: 2
issue-type: task
created-at: "2026-11-25T17:47:58.473741+01:00"
closed-at: "2026-11-26T19:11:04.247492+01:00"
---

Problem: each x86-64 data address is a 10-byte movabs (src/habu/t0-x64.f ADDR), about 133 KB of the x86 engine (26,841 sites; the 1,092 code addresses, which fit 32 bits, are 5 KB), although a program's data now starts at 2 GiB in its image (DATA-VA, src/os/linux/layout.f) and, while it is built, at 1 GiB into hb's own data window (BUILD-OFF, src/habu/layout.f), where a native build's one emission both runs and is linked, so the first GiB of each lies below 4 GiB. Acceptance: x86 code shrinks by what a 5-byte `mov r32, imm32` saves, a native build still walks each definition once and takes no longer, a program with more than 2 GiB of data still links, and the engine rebuilds itself byte for byte. Depends: none. Claim: unassigned.
