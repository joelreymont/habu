---
title: Reserve the code region in the arm64 ELF
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:55:05.514275+01:00"
---

Problem: src/habu/habu2.f EM-MMAP-CODE-REGION maps the code region at a hint without MAP_FIXED; a preloaded library's malloc before entry starts the brk heap at a random page up to 1 GiB above the image, and when it covers the hint the kernel moves the region out of BL reach and boot dies E-BL-RANGE (a Maki image with OCCT preloaded, 5 of 200 starts; hb under the same preload, 11 of 400; /tmp/tim-blrange.md). The x86-64 image reserves its region as a LOAD segment and has no race. Acceptance: the arm64 Linux image reserves the code region as a LOAD segment and boot maps it there; hb under the OCCT preload starts 1000 of 1000. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-bl2.
