---
title: Delete the Mach-O payload path
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T11:00:44.471971+02:00"
---

Problem: no image carries a stripped DATA payload (src/os/image-bytes.f:24-25 only ever stores 0), yet macho.f keeps ASM-CODELEN!'s payload branch, MACHO-EXTRA,'s copy and the multi-page sizing in MACHO-FIXUPS-MAX and MACHO-IMAGE-MAX. Acceptance: the payload path is gone and the writer's sizes follow from what it writes. Depends: habu-bind-the-kernel-472ad374. Claim: unassigned.
