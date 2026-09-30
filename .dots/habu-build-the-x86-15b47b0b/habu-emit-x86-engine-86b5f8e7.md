---
title: Emit x86 engine-state bodies
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.586158+03:00"
blocks:
  - habu-scaffold-the-x86-9af80979
  - habu-emit-the-x86-f74e1d26
---

Problem: the kernel has no engine-state rows.
Acceptance: bodies for `cp@ cp! dbase@ rbase ndict@ ndict! seed-ndict! ndict-append data-base prot-wid-add prot-wid-room SEAL-* DRAIN-PRETRUST set-check check@ set-preflight set-top-check top-check@ set-tier tier@ code-origin executable-build-enter/leave wordlist get-current set-current wide-mark xt! ptr-cell-mark addr-cells-abi here allot align , c, . u. .s depth emit cr space type`; `set-tier 0` refuses on x86 (precedent `src/habu/prof.f:47-49`); the rows join the `docs/x86-64.md` kernel inventory.
Files: `src/habu/kernel-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: routine images (C8).
Depends: habu-boot-and-exit-367c46f5 (K3).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.

K-lane corrections (design 2026-09-30; these override the lines above where they differ):
- Depends add habu-scaffold-the-x86-9af80979 (the kernel scaffold). Files: replace `test/x86-64-peer-routines.f` with this leaf's `test/x86-64-kernel-<name>.f` from the scaffold. Bodies are hand-written through `X64ASM` (no allocator dependency). Every body reads DATA through rbp, so cases run in the booted harness (`test/x86-64-boot-harness.f`). The row table goes in this leaf's `docs/x86-64.md` subsection. Verify: host engine K3's product `264c829e…`; the ThinkPad runs the images natively, each with its negative twin. Base: the scaffold on master.
- Split: this dot is K9a. K9d (habu-emit-the-x86-f74e1d26, dictionary index and `search-wl`/`xref-search-wl`) precedes it; K9c (habu-emit-x86-heap-0b1d01f0, heap, printers, checker hooks) and K9b (habu-model-the-code-c40c75d1, the provenance band) are siblings.
- Rows: `cp@ cp! dbase@ rbase data-base ndict@ ndict! seed-ndict! ndict-append` (`habu1.f:1394-1486`; `cp!`, `ndict!`, `seed-ndict!` and `ndict-append` behind `TASK-LIVE-GUARD,`, `cp!` through `GUARD-CODE-WORD`); `SEAL-CAPTURE seal-captured? SEAL-FRIEND` (`habu1.f:3065-3089`); `DRAIN-PRETRUST` (`habu2.f:4178`); `prot-wid-add prot-wid-room` (`habu1.f:3168-3200`); `wordlist get-current set-current` (`habu1.f:2885-`); `wide-mark` (`habu1.f:3097`); `xt! ptr-cell-mark` (`habu2.f:6744-6790`); `addr-cells-abi snapshot-format` (`habu2.f:6741-6742`, constants); `executable-build-enter/leave` through `PRIM-WID`; `set-tier` (1 stores `NCOMP-DISPATCH:TIER-CELL`; anything else writes `set-tier: x86-64 runs tier 1 only` on fd 2 and exits 70, the `BSETTIER` bad arm `habu1.f:2975-2988`); `tier@`; `code-origin` (K9b adds its provenance query).
- Decision, `snap-rebase`: a `REFUSE` row (R7: the x86 image has a fixed region and DATA; restore is the ordinary boot).
- `cp!` and `ndict!` move r15 and r14, which C8's `CLOSE,` pins; the booted harness checks `cp@` after `cp!` instead.
- Files: `src/habu/kernel-x64.f`, `test/x86-64-kernel-engine.f`, `docs/x86-64.md`.
