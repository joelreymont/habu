---
title: Prune the kernel from x86 programs
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T10:48:16.820508+01:00"
---

Problem: NATIVE:CUT (src/habu/link-kernel.f:677) returns at once on x86, so a stripped x86 program keeps the whole kernel. Acceptance: a stripped x86 program links only the kernel words it reaches, as on arm64. Depends: none. Claim: unassigned.
