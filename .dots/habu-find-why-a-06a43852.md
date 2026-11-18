---
title: Read the Linux layout package on a macOS host
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T23:34:17.891863+01:00"
---

Problem: x86-64 code reads the Linux layout as package LINUX-LAYOUT (src/os/linux/layout.f); a macOS hb carries it through src/habu/link-code.f's require of src/arch/x86-64/icode.f, and a macOS engine build loads it at icode.f's top level. That is shown only by reading: nothing has run on a macOS host. Acceptance: on a macOS host, an engine build run again with nothing changed writes no object, the x86-64 cross build exits 0, and tools/check.f -- --verify-only src/os/linux-x86-64/elf.f exits 0. Depends: none. Claim: unassigned.
