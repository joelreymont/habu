---
title: Name each x86-64 exit status once
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T11:07:40.700037+01:00"
---

Problem: x86-64 files still name exit statuses outside package EXIT-STATUS (src/core/exit-status.f): src/habu/prof-x64.f :179 and :681 and test/x86-64-kernel-prof.f :794 read PROF-MAP-RC and PROF-LIMIT-RC, which no file defines; src/habu/boot-x64.f :497 mints `134 constant CRASH-RC`; src/habu/snap-x64.f:35 exits `74 die`; src/os/linux-x86-64/elf.f :108 and :297 exit `73 die`. Acceptance: each names its status from EXIT-STATUS, as src/habu/prof.f and the other arm64 sites do, and an x86-64 engine builds. Depends: none. Claim: unassigned.
