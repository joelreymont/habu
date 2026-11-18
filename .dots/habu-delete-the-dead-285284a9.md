---
title: Delete the dead x86 snapshot start
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T10:48:16.907351+01:00"
---

Problem: boot-x64.f SNAP-START,, RECORD-CALL,, TEXT-END, and the snap-decode-x64.f require have no caller; src/os/linux-x86-64/elf.f is a second ELF writer used only by image-x64.f, snap-x64.f and three tests. Acceptance: the dead path is deleted and x86 images come from the shared ELF writer only. Depends: none. Claim: unassigned.
