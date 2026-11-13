---
title: Delete the Mach-O payload path
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T11:00:44.471971+02:00"
closed-at: "2026-11-13T20:48:41.752358+01:00"
close-reason: Payload path deleted; the Mach-O buffer is sized from the file it writes.
---

Problem: no image carries a stripped DATA payload (src/os/image-bytes.f:24-31 only ever stores 0), yet macho.f keeps ASM-CODELEN!'s payload branch, MACHO-EXTRA,'s copy and the multi-page sizing in MACHO-FIXUPS-MAX and MACHO-IMAGE-MAX. Acceptance: the payload path is gone and the writer's sizes follow from what it writes. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-macho.
