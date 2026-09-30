---
title: Write ELF segments for a linked x86 image
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.732552+03:00"
---

Problem: the x86 ELF writer (`src/os/linux-x86-64/elf.f`, `VMBASE $400000` at line 33) has no segment for the code region or for DATA; the ARM64 region is mapped wherever the kernel gives it (`src/habu/habu2.f:5113-5145`), which is why snapshot call displacements are relocated at load. On Linux an ET_EXEC `PT_LOAD` is mapped at its `p_vaddr` or exec fails, so the x86 image fixes both (DATA is already fixed: `MAP-ANON-PRIVATE-FIXED` at `DATA-VA`, `habu2.f:6247`). First of X4a-d.
Acceptance: `elf.f` writes a `PT_LOAD` for the region at `VMBASE + REGION-OFF` (64 KiB aligned for `PROT-PAGE-MAX`) and one for DATA at `DATA-VA` (memsz `DATA-SIZE`), `ELF-PHDR-N` grown; `test/x86-64-seam.f` pins the headers; `docs/x86-64.md` gains the fixed-segment section.
Files: `src/os/linux-x86-64/elf.f`, `test/x86-64-seam.f`, `docs/x86-64.md`.
Verify: spark `bin/hb --load test/x86-64-seam.f`; ThinkPad `readelf -l` on a written image.
Depends: habu-boot-and-exit-367c46f5 (K3).
Route: direct.
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-write-and-link-f6e6017f.

Preflight corrections (2026-09-30; override the lines above where they differ):
- Acceptance: `elf.f` appends two `PT_LOAD`s after the RW tail in ascending `p_vaddr` order: the region at `VMBASE + REGION-OFF` (`$1400000`, `PROT-PAGE-MAX` aligned), memsz `REGION`; DATA at `DATA-VA` (`$340000000`), memsz `DATA-SIZE`; both `p_filesz 0`, `p_offset 0` (a bare image carries no region or DATA bytes; `boot-x64.f CODE-REGION,`/`DATA-REGION,` still `MAP-FIXED,` over them, a no-op until a later leaf writes content), flags `PF_RW`. `ELF-PHDR-N` 6, so `ELF-INTERP-OFF` … `ELF-RELA-OFF` (`elf.f:37-49`) move past `$190` (`M-PAD-OFF` has no backwards check, `src/os/image-bytes.f:151-152`).
- The target's `DATA-VA`/`DATA-SIZE` come from `src/os/linux-x86-64/layout.f` `included` under a package inside `elf.f` (as `boot-x64.f:32` does), never the host's bare globals: macOS spells them `$44000000000`/`$10000000000` (`src/os/macos/layout.f:9-10`) and the seam suite runs on the pooled Mac gate (`test/gate-stdlib-cases.f:1316-1318`). New words are `elf.f` globals beside `ELF-RX-PHDR,` (the peer harness binds the writer bare, `test/x86-64-peer-harness.f:248`).
- `test/x86-64-seam.f` pins e_phnum 6, the moved offsets and both segments' fields as literals; `docs/x86-64.md` replaces "four phdrs" (line 445) and the "Kernel inventory" fixed-region sentence with the six-segment section.
- Verify adds: ThinkPad `test/x86-64-skel-image.f` (writes and executes `hb-x64-skel`, lines 59-60) beside `readelf -l`. Measured: kernel 7.2.5 maps both far zero-filesz `PT_LOAD`s at `p_vaddr` (writes at both ends of both segments, either phdr order).
- Refs: `VMBASE` is `elf.f:38`; `EM-MMAP-CODE-REGION` ~`habu2.f:5125`; `EM-MMAP-DATA-REGION` ~`6259`.
