---
title: Make the x86 target layout linkable
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T10:44:57.119935+01:00"
---

Problem: src/os/linux-x86-64/target-layout.f includes src/os/linux/layout.f inside an open package, so its pieces are unlinkable and every build that loads it (each x86 build) rewrites its kernel and three objects, breaking warm builds. Acceptance: an x86 warm build rewrites nothing; file-objects' rule holds for x86. Depends: none. Claim: unassigned.
