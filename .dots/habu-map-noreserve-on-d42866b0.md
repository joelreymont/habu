---
title: Map NORESERVE on Linux x86-64
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T22:26:11.264488+02:00"
---

Problem: src/os/linux-x86-64/sys.f OS-MMAP-FLAGS (:194-201) keeps MAP_SHARED|MAP_PRIVATE|MAP_FIXED ($13) and moves MAP_ANON $1000 to $20 but drops macOS MAP_NORESERVE $40, so LINKER:WITH's open (src/habu/linker.f:86, DATA-SIZE private anonymous NORESERVE) would be charged in full on Linux x86-64, as habu-map-noreserve-on-55c86706 fixes for Linux arm64. Waits: Joel 2026-11-05, no x86-64 work until the arm64-to-arm64 cross-build works. On Linux x86-64 $40 is MAP_32BIT, so the bit is translated, never passed through (today the $13 mask drops it). Acceptance: `R10 $40 $4000 OS-FLAG-BIT,` beside the MAP_ANONYMOUS line and the comment above names it. Files: src/os/linux-x86-64/sys.f. Verify: when x86-64 moves onto the linker, its open/close test on pulsar with /proc/self/smaps VmFlags nr while the space is open. Depends: the arm64-to-arm64 cross-build working. Ownership: src/os/linux-x86-64/sys.f. Claim: unassigned.
