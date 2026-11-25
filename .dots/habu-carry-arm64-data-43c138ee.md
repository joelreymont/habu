---
title: Carry arm64 data in 4 KiB pages
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T18:46:47.562134+01:00"
---

Problem: an arm64 ELF program carries its data in 64 KiB pages (src/os/linux/elf.f PAGE-BYTES), the largest page its kernels have, since the boot moves whole pages into place (src/arch/arm64/boot.f), so zeros beside a non-zero cell fill its file: about 1.6 MB of tenderd's 7.67 MB, 0.8 MB of hb's 6.03 MB. A kernel with 4 KiB pages could move 4 KiB pages; one with larger pages has to copy them. Acceptance: an arm64 program's file is as small as its x86-64 one's data allows, starts as fast as today's on a 4 KiB kernel, and runs on a 64 KiB kernel. Depends: none. Claim: unassigned.
