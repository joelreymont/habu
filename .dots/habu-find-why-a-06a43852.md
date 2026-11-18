---
title: Link the x86 target layout warm
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T23:34:17.891863+01:00"
---

Problem: every engine build compiles src/os/linux-x86-64/target-layout.f's two parts and src/os/linux/layout.f's third load again, whatever changed: target-layout.f opens package X64LAYOUT and then loads layout.f inside it (:25-26), and a piece that starts or ends inside a package, or a load requested inside one, never links, since an object records no scope (src/habu/linker.f :5245-5251, RESUME :5540, NEST :5560, SCOPE-CK :5496 and :5596). Fix: src/os/linux/layout.f holds the Linux layout facts as package LINUX-LAYOUT's public constants, and the boot rows of every native engine load it at top level (src/habu/boot-files.f :64-66, plus a macOS row, since a macOS host cross-builds x86-64); a Linux engine's bare layout names are copies a boot row of their own makes; the x86-64 files read LINUX-LAYOUT: names, so X64LAYOUT and target-layout.f's load inside a package go. Acceptance: an engine build run again with nothing changed writes no object, and the x86-64 cross build and a check of an x86 file still read the Linux layout's values on a Linux and a macOS host. Depends: none. Claim: unassigned.
