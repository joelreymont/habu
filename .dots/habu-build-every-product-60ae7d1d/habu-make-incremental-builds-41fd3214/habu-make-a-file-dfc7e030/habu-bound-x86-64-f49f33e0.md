---
title: Bound x86-64 DP by the header cells
status: open
priority: 2
issue-type: task
created-at: "2026-11-04T15:21:21.308135+02:00"
---

Problem: arm64's DP-CHECK (src/habu/habu1.f:1978) reads BOOT:DP-FLOOR-CELL and DP-CEILING-CELL, which every arm64 start writes (src/arch/arm64/kernel-object.f:171-172, src/habu/link-data.f:141). x86-64's twin (src/habu/kernel-x64.f DP-CHECK,) compares against DATA + DATA-START and the constant DP-CEILING (:58), and boot-x64.f DATA-INIT, writes neither cell, so src/lib/task.f:1120-1121 copies unwritten cells into a task. A machine asymmetry; the move of x86-64 onto the linker (habu-build-x86-64-3ed6f45d) needs the cells. src/habu/boot-cells.f:7 says "every arm64 start" until this lands. Acceptance: boot-x64.f DATA-INIT, writes both cells on every x86-64 start (cold, saved image, task inherits); DP-CHECK, reads them; boot-cells.f's comment (:7-8) drops "arm64"; a probe reads both cells on pulsar, and test/x86-64-kernel-engine.f and the allot/here tests pass there. Depends: none; lands before habu-build-x86-64-3ed6f45d. Claim: unassigned.
