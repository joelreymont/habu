---
title: Route x86-64 header stores into an open space
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T12:25:05.606763+02:00"
---

Step 4's x86-64 half, joining step 8 (one arch first). Problem: on arm64, xt! and ptr-cell! route a store into the engine's header to the open image while BOOT:IMAGE-BASE-CELL is set (habu2.f ROUTE-HEADER), and MARK records an image cell in the image's own table; src/habu/kernel-x64.f's xt! and ptr-cell! bodies do neither, and boot-x64.f does not write the cell 0. The x86-64 ptr-cell! body has never run, and test/x86-64-kernel-engine.f has no case for it. Acceptance: kernel-x64.f xt! and ptr-cell! route as arm64 does; its MARK takes the image's table for an image cell; boot-x64.f writes IMAGE-BASE-CELL 0; test/x86-64-kernel-engine.f gains a ptr-cell! case (store and declare, cell then holds the pointer and is a declared data cell). Files: kernel-x64.f, boot-x64.f, test/x86-64-kernel-engine.f. Verify: the x86-64 build, test/x86-64-kernel-engine.f on pulsar. Depends: step 8 (x86-64 onto LINKER). Ownership: those files. Claim: unassigned.
