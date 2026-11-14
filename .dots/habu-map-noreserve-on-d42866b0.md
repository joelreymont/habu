---
title: Map NORESERVE on Linux x86-64
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T22:26:11.264488+02:00"
---

Problem: src/os/linux-x86-64/sys.f OS-MMAP-FLAGS (:194-199) keeps MAP_SHARED|MAP_PRIVATE|MAP_FIXED ($13) and moves MAP_ANON $1000 to $20 but drops macOS MAP_NORESERVE $40, so LINKER:WITH's open (src/habu/linker.f:4673, a private anonymous NORESERVE map) would be charged in full on Linux x86-64, as habu-map-noreserve-on-55c86706 fixes for Linux arm64. On Linux x86-64 $40 is MAP_32BIT, so the bit is translated, never passed through (today the $13 mask drops it). Acceptance: `R10 $40 $4000 OS-FLAG-BIT,` beside the MAP_ANONYMOUS line and the comment above names it. Depends: x86-64 moving onto the linker (habu-re-enable-the-959502b5). Claim: unassigned.
