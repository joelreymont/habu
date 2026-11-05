---
title: Measure a fixed code-region address at hb start
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T15:28:59.028374+02:00"
closed-at: "2026-11-05T15:32:59.325673+02:00"
close-reason: "measured: both candidates free 200/200 on macOS and Linux arm64"
---

Design 9 m5: is REGION-VA free in fresh hb processes on macOS arm64 and Linux arm64 (spark)? Candidates DATA-VA - 1 GiB and DATA's end; hint-and-compare on macOS, MAP_FIXED_NOREPLACE on Linux; 200 runs each, refusals counted. Brief .jj-ws.noindex/carl-dots/.out/carl-file/brief-m5-region.md. Workspace .jj-ws.noindex/carl-m5 (measurement, nothing lands). Ownership: carl. Claim: carl.

Result: in 200 fresh processes per OS both candidates were free every time and mprotect R|X never failed. macOS 27.0.1 (installed hb, master 94bc4127 engine): BELOW $43FC0000000 200/200 ok, ABOVE $54000000000 200/200 ok (MAP_ANON|MAP_PRIVATE hint, result compared). spark Linux arm64 6.17, CONFIG_ARM64_VA_BITS=48, legacy_va_layout 0, stack 8192 KiB: BELOW $300000000 200/200 ok, ABOVE $4340000000 200/200 ok (MAP_FIXED_NOREPLACE). Size probed $2310000 (REGION + PROT-PAGE-MAX). Linux maps then: image $400000, code region memfd RX $1410000-$3710000, heap $412f000, DATA $340000000-$4340000000, libc/ld.so and the RW alias near $e7c4..., stack near $fffff266d000. The refusal path works: probing DATA-VA itself gave EEXIST on Linux and a moved result on macOS. Not measured: the boot's own earlier map, suite starts and child engines, a 39-bit or 36-bit Linux kernel, legacy layout or an unlimited stack, other macOS versions. The probe (libc mmap/mprotect/munmap through FUNCTION:, then compare) ran after hb's boot, a stricter test than the boot's.
