---
title: Route x86-64 header stores into an open space
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T12:25:05.606763+02:00"
---

Problem: on arm64, xt! and ptr-cell! route a store into the engine's header to the open image while BOOT:IMAGE-BASE-CELL is set (src/habu/habu2.f:1971 ROUTE-HEADER), and MARK records an image cell in the image's own table; src/habu/kernel-x64.f's xt! and ptr-cell! bodies do neither, and boot-x64.f does not write the cell 0. The x86-64 ptr-cell! body has never run, and test/x86-64-kernel-engine.f has no case for it. Acceptance: kernel-x64.f xt! and ptr-cell! route as arm64 does; its MARK takes the image's table for an image cell; boot-x64.f writes IMAGE-BASE-CELL 0; test/x86-64-kernel-engine.f gains a ptr-cell! case (store and declare, cell then holds the pointer and is a declared data cell) that passes on pulsar. Depends: habu-build-x86-64-3ed6f45d (x86-64 onto the linker). Claim: unassigned.
