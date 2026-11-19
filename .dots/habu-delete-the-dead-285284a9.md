---
title: Make x86 images with the shared ELF writer only
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T10:48:16.907351+01:00"
---

Problem: src/os/linux-x86-64/elf.f is a second ELF writer used only by test/x86-64-boot-harness.f, test/x86-64-peer-harness.f and test/x86-64-seam.f. Its DATA carrier comment (elf.f:86-89) names a boot copy, src/habu/boot-x64.f CARRIER-COPY,, that no boot has, and boot-x64.f START, (:939) still says its carrier is empty. Acceptance: x86 images come from the shared ELF writer only. Depends: none. Claim: unassigned.
