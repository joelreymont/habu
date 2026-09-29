---
title: Write ELF segments for a linked x86 image
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.732552+03:00"
blocks:
  - habu-boot-and-exit-367c46f5
---

Problem: the x86 ELF writer (`src/os/linux-x86-64/elf.f`, `VMBASE $400000` at line 33) has no segment for the code region or for DATA; the ARM64 region is mapped wherever the kernel gives it (`src/habu/habu2.f:5113-5145`), which is why snapshot call displacements are relocated at load. On Linux an ET_EXEC `PT_LOAD` is mapped at its `p_vaddr` or exec fails, so the x86 image fixes both (DATA is already fixed: `MAP-ANON-PRIVATE-FIXED` at `DATA-VA`, `habu2.f:6247`). First of X4a-d.
Acceptance: `elf.f` writes a `PT_LOAD` for the region at `VMBASE + REGION-OFF` (64 KiB aligned for `PROT-PAGE-MAX`) and one for DATA at `DATA-VA` (memsz `DATA-SIZE`), `ELF-PHDR-N` grown; `test/x86-64-seam.f` pins the headers; `docs/x86-64.md` gains the fixed-segment section.
Files: `src/os/linux-x86-64/elf.f`, `test/x86-64-seam.f`, `docs/x86-64.md`.
Verify: spark `bin/hb --load test/x86-64-seam.f`; ThinkPad `readelf -l` on a written image.
Depends: habu-boot-and-exit-367c46f5 (K3).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
