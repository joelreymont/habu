---
title: Emit x86 atomics, fence and code publication
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.577804+03:00"
blocks:
  - habu-boot-and-exit-367c46f5
  - habu-represent-x86-live-729a7ac6
---

Problem: `X64ASM` has no `lock`-prefixed encoders (`INTEL.md:178-180`) and the kernel has no atomic or code-publication rows.
Acceptance: `lock`-prefixed encoders added to `X64ASM`; bodies for `atomic@ atomic! atomic-add atomic-cas fence patch32 code-publish callmap-set addrmap-set xref-retarget int-mark min-in-mark reloc-maps-clear does-patch does-record` with the site-band semantics of P2, the region protection toggles (mprotect), no icache flush; the rows join the `docs/x86-64.md` kernel inventory.
Files: `src/arch/x86-64/asm.f`, `test/compiler/x86-64-asm.f`, `src/habu/kernel-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: spark `bin/hb --load test/compiler/x86-64-asm.f`; ThinkPad: routine images.
Depends: habu-boot-and-exit-367c46f5 (K3), habu-represent-x86-live-729a7ac6 (P2).
Route: direct (test/compiler/x86-64-asm.f is the X64ASM encoder suite, x86-only).
Ownership: krait (Intel lane).
Claim: unassigned.
