---
title: Map the x86-64 code region through two views
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T10:48:28.669317+02:00"
---

arm64 master 679c7755 maps the code region twice (RX at DBASE, RW alias at DBASE plus REGION-ALIAS:DELTA-CELL; src/habu/habu2.f EM-MMAP-CODE-REGION) and deleted the write-protection flips and PROT-EMIT. The x86-64 kernel still flips bands with mprotect: src/habu/kernel-x64.f LSPAN-HELPER, LOPEN-HELPER, and LCLOSE-HELPER, (~2767-2820), WINDOW-SPAN, WINDOW-OPEN, and WINDOW-CLOSE, (~3399) and their call sites, over src/habu/layout.f package PROT ($27D0-$47C8). Port: Linux x86-64 maps a memfd twice as Linux arm64 does, every store into the region goes through the alias, and the window helpers, their call sites and package PROT are deleted. Waits for the x86-64 stage (arm64 first).
