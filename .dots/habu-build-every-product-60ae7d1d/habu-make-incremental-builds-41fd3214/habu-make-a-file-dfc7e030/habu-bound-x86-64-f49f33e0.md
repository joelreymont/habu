---
title: Bound x86-64 DP by the header cells
status: open
priority: 2
issue-type: task
created-at: "2026-11-04T15:21:21.308135+02:00"
---

Problem: arm64's DP-CHECK (src/habu/habu1.f) reads BOOT:DP-FLOOR-CELL and DP-CEILING-CELL, which every arm64 start writes (habu2.f EM-DATA-INIT, EM-STARTUP-RUNTIME-STATE, aot-link-arm.f); x86-64's twin (src/habu/kernel-x64.f DP-CHECK,) compares against DATA + DATA-START and the constant DP-CEILING, and boot-x64.f DATA-INIT, writes neither cell, so task.f TASK-REGION-INIT copies unwritten cells. A machine asymmetry (DECISIONS.md one compiler); the cross-build needs the cells on x86-64 once x86-64 moves onto LINKER (/tmp/jerry-big/xbuild-order.md item 8). Found by the 3a review (2026-11-04); src/habu/boot-cells.f:8-9 says "every arm64 start" until this lands. Acceptance: boot-x64.f DATA-INIT, writes both cells on every x86-64 start (cold, saved image, task inherits); DP-CHECK, reads them; boot-cells.f's comment (:8-9) drops "arm64"; a probe reads both cells on pulsar, and test/x86-64-kernel-engine.f and the allot/here rows pass there. Files: src/habu/boot-x64.f, src/habu/kernel-x64.f, src/habu/boot-cells.f. Verify: the probe and rows on pulsar. Depends: none (before item 8). Ownership: kernel-x64.f DP-CHECK,, boot-x64.f DATA-INIT,. Claim: unassigned.
