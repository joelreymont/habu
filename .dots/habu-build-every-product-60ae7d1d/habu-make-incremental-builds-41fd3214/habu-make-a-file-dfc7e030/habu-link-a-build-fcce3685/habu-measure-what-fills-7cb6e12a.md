---
title: Measure what fills a 3.3 MB stripped arm64 program
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T01:22:40.156027+02:00"
---

Problem: each program test/compiler/arm64-link-run.f writes (LR-HI prints Hi; LINKER:WRITE in src/habu/link-data.f) is 3,335,676 bytes (the L5b2b worker's observation, 2026-11-06). Its parts are unmeasured: the boot and kernel code (ARM64:OPEN, A64BOOT:START,, ARM64:KERNEL), the routines, the DATA image (DATA-IMAGE-LEN, the space's bytes up to DP, header bands included), the DATA list, the Mach-O and code-signature overhead. Acceptance: each part's byte count for LR-HI's program, and for each part over 10% of the file, whether a stripped program needs it; a part it does not need becomes its own dot. Files: src/habu/link-data.f, test/compiler/arm64-link-run.f (read only). Verify: the measurement's numbers sum to the file size. Depends: habu-s6-l5b2b-linker-b34bfda1. Ownership: unassigned. Claim: unassigned.
