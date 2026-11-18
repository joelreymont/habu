---
title: Read the x86 image base from LINUX-LAYOUT
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T11:43:43.651957+01:00"
---

Problem: src/os/linux-x86-64/elf.f:50 ($400000 constant VMBASE) and src/arch/x86-64/kernel-object.f:24 (TEXT-VA) spell out LINUX-LAYOUT:IMAGE-VA's value (src/os/linux/layout.f:40) again. Acceptance: both read LINUX-LAYOUT:IMAGE-VA and VMBASE is gone; the x86-64 cross build's image is byte-identical. Depends: none. Claim: unassigned.
