---
title: Add lock encoders and x86 atomic rows
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.577804+03:00"
blocks:
  - habu-size-the-x86-fb635c93
---

Problem: `X64ASM` has no `lock`-prefixed encoders (`INTEL.md:178-180`) and the kernel has no atomic or code-publication rows.
Acceptance: `lock`-prefixed encoders added to `X64ASM`; bodies for `atomic@ atomic! atomic-add atomic-cas fence patch32 code-publish callmap-set addrmap-set xref-retarget int-mark min-in-mark reloc-maps-clear does-patch does-record` with the site-band semantics of P2, the region protection toggles (mprotect), no icache flush; the rows join the `docs/x86-64.md` kernel inventory.
Files: `src/arch/x86-64/asm.f`, `test/compiler/x86-64-asm.f`, `src/habu/kernel-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: spark `bin/hb --load test/compiler/x86-64-asm.f`; ThinkPad: routine images.
Depends: habu-boot-and-exit-367c46f5 (K3), habu-represent-x86-live-729a7ac6 (P2).
Route: direct (test/compiler/x86-64-asm.f is the X64ASM encoder suite, x86-only).
Ownership: krait (Intel lane).
Claim: unassigned.

K-lane corrections (design 2026-09-30; these override the lines above where they differ):
- Depends add habu-scaffold-the-x86-9af80979 (the kernel scaffold). Files: replace `test/x86-64-peer-routines.f` with this leaf's `test/x86-64-kernel-<name>.f` from the scaffold. Bodies are hand-written through `X64ASM` (no allocator dependency). Every body reads DATA through rbp, so cases run in the booted harness (`test/x86-64-boot-harness.f`). The row table goes in this leaf's `docs/x86-64.md` subsection. Verify: host engine K3's product `264c829e…`; the ThinkPad runs the images natively, each with its negative twin. Base: the scaffold on master.
- Split: this dot is K8a, encoders and atomics; K8b (habu-emit-x86-code-973a0074) takes the code window, `code-publish`, the map inserts, `xref-retarget`, the marks, `does-record` and `patch32`. `does-patch` moves to I7 (slot contract there).
- `INTEL.md:178-180` does not exist; the fact is K3 `docs/x86-64.md:133-136`. Encoders `ENC-XCHG-MR`, `ENC-LOCK-XADD-MR`, `ENC-LOCK-CMPXCHG-MR`, `ENC-MFENCE`, pinned against `/usr/bin/llvm-mc` in `test/compiler/x86-64-asm.f`. Correct `src/arch/x86-64/asm.f:729-731`: only the memory form of `xchg` is implicitly locked.
- Rows: `atomic@` (`mov`), `atomic!` (`xchg [m], r`), `atomic-add` (`lock xadd`), `atomic-cas` (`lock cmpxchg`, rax = expected, push the actual value), `fence` (`mfence`). `atomic!`, `atomic-add` and `atomic-cas` call `PROT-SPAN-CALL,` (twins `habu1.f:1724-1735`).
- Files: `src/arch/x86-64/asm.f`, `test/compiler/x86-64-asm.f`, `src/habu/kernel-x64.f`, `test/x86-64-kernel-atomics.f`, `docs/x86-64.md`. Depends: the scaffold, P2 (landed).

Preflight note (Fable, 2026-09-30): READY. `PROT-SPAN-CALL,` (`kernel-x64.f:149-163`) preserves no scratch register, so unlike `BATCAS`'s x14 save a body reads its operands from `[r12-8]`/`[r12-16]` after the guard, or pushes and pops them. ARM64 twins are `habu1.f:1623-1634`. `docs/x86-64.md:133-136` ("not yet encoded") and `asm.f:729` (xchg "implicitly locked" wording) are this leaf's to correct. Tests are single positive images. Host engine: master's product `5f4d3321…`.
