---
title: "M1a: dynamic Linux ELF (first-symbol bootstrap)"
status: open
priority: 1
issue-type: task
created-at: "\"2026-06-25T13:48:30.803398+02:00\""
---

PIVOTAL. The Linux bin/hb is a static no-libc ELF (src/os/linux/elf.f: ET_EXEC, single PT_LOAD, no PT_INTERP/.dynamic) -> no dlopen exists. Decision: make it a DYNAMIC ELF so ld.so provides dlopen/dlsym. Extend elf.f: PT_INTERP=/lib/ld-linux-aarch64.so.1; .dynamic with DT_NEEDED libc.so.6 (glibc>=2.34 has dlopen/dlsym in libc); .dynsym/.dynstr/.gnu.hash; .rela.plt + PLT/GOT stubs for dlopen,dlsym. PRIMARY RISK: the self-host snapshot/image embedding (src/habu/snap.f) must still produce a working dynamic bin/hb that self-rebuilds. Verify on zed: dynamic bin/hb self-hosts + resolves dlopen via PLT.


## Implementation spec (de-risked on zed 2026-06-25)

PROVEN on Orin: dlopen("/usr/lib/aarch64-linux-gnu/nvidia/libcuda.so.1") + dlsym(cuInit) + cuInit(0)->0, driver 12060, 1 device. So once bin/hb is dynamic + has a call trampoline, the Driver harness works.

Current emitter (src/os/linux/elf.f): ET_EXEC, ONE PT_LOAD R+X at VMBASE=0x400000, entry=VMBASE+CODE-OFF, image self-contained. BUILD-SNAP-HDR writes the same header for the self-host snapshot.

Make it a minimal DYNAMIC ET_EXEC. Reference layout (gcc -no-pie on zed):
- Program headers (5): INTERP, LOAD(R+X), LOAD(R+W), DYNAMIC, GNU_RELRO.
- .interp = "/lib/ld-linux-aarch64.so.1\0"
- .dynsym: [0]=null, dlopen(UNDEF,GLOBAL,FUNC), dlsym(UNDEF). glibc syms are versioned (dlopen@GLIBC_2.34) but a default-version UNDEF usually resolves; add verneed only if needed.
- .dynstr: "\0libc.so.6\0dlopen\0dlsym\0"
- hash: use SysV .hash (DT_HASH) over the 2 syms — simpler to hand-build than GNU_HASH.
- relocs: R_AARCH64_GLOB_DAT in .rela.dyn writing dlopen/dlsym into 2 GOT slots; DT_FLAGS=DF_BIND_NOW (+ DT_BIND_NOW) so ld.so resolves at load -> NO .plt stubs. Forth reads [GOT+slot] for the fn address.
- .dynamic tags: NEEDED(libc.so.6), HASH, STRTAB, SYMTAB, STRSZ, SYMENT=24, RELA, RELASZ, RELAENT=24, FLAGS=DF_BIND_NOW, BIND_NOW, NULL.
- Two LOAD segments: R+X (0x400000) for hdr+interp+hash+dynsym+dynstr+rela+text; R+W (page-aligned) for .dynamic+.got. GOT must be writable.

STAGING (de-risk the self-host blast radius): add an opt-in BUILD-DYN-ELF path that emits a TEST binary (tiny: dlopen libcuda, cuInit, exit) FIRST; prove ld.so loads it + dlopen resolves on zed; THEN switch the main bin/hb snapshot to dynamic and re-verify the self-host fixpoint.

Build/test loop is on zed (macOS can't exercise the Linux ELF or ld.so).

## CROSS-PLATFORM (must work on macOS AND Linux)

FFI machinery is split: SHARED trampoline (M1b, arm64 AAPCS64 — same regs x0-x7/v0-v7 for our non-variadic Driver calls) + SHARED marshalling (M1c); per-OS dynamic-symbol IMPORT only.

- Linux (VALIDATED on zed): minimal dynamic ET_EXEC — PT_INTERP=/lib/ld-linux-aarch64.so.1, DT_NEEDED libc.so.6, R_AARCH64_GLOB_DAT into 2 GOT slots, DT_FLAGS=DF_BIND_NOW (no PLT). /tmp/t2 (6168 B) dlopen->dlsym(cuInit)->cuInit(0)=exit 0. Port into src/os/linux/elf.f.
- macOS: bin/hb ALREADY links libSystem via dyld (LC_DYLINKER + LC_LOAD_DYLIB libSystem.B.dylib) but imports NO symbols (no LC_SYMTAB/LC_DYLD_INFO; raw svc $80 in src/os/macos/sys.f). So dlopen/dlsym are in-process; just ADD an import: LC_DYLD_INFO_ONLY bind (BIND ordinal=libSystem, symbol=dlopen/dlsym, type=POINTER, seg=__DATA off=gotslot, DO_BIND) + LC_SYMTAB + a __DATA __got with 2 slots; dyld writes the addresses at load. Extend src/os/macos/macho.f.

STRATEGY: build the trampoline + marshalling + a generic DLOPEN/DLSYM/FFI-CALL, and the macOS import, FIRST on macOS (fast local loop — this dev machine). Then port the Linux ELF import. Then M1d (CUDA) on zed. macOS testability is the accelerator.

## Concrete layout worked out 2026-06-25 (M1b done; ready to implement)

STATUS: M1b `ffi-call` DONE + proven on macOS AND Linux/aarch64 (on master).
M3 emit+assemble smoke is on master; GPU launch still waits for M1d. M1a is the
single ready restart dot. Sync zed to origin/master and start here. Isolated
Linux build loop ready: zed `/tmp/habu-m1` worktree at origin/master; rebuild =
`cd /tmp/habu-m1 && cp ~/Work/habu/bin/hb bin/hb && mkdir -p native &&
env HB_TMP=$PWD/native bin/hb --load <libs> tools/build-fixpoint.f
tools/build-fixpoint-main.f -- install` (MUST mkdir HB_TMP; never `| tail` the
rc — it masks the real exit).

KEY DESIGN — fixed GOT vaddr (solves the "engine needs dlopen at a constant
address" problem): put the RW segment (.dynamic + GOT) at a FIXED high vaddr
`VMBASE+0x100000 = 0x500000`, file offset = TEXTSZ (right after text). Since
VMBASE/0x100000/TEXTSZ are all page-aligned, `(vaddr-offset) ≡ 0 mod 0x1000`, so
ld.so accepts it AND the GOT vaddr is a compile-time constant the engine bakes
in. DLOPEN-SLOT=0x5000B0, DLSYM-SLOT=0x5000B8; engine reads `DLOPEN-SLOT @`
after ld.so BIND_NOW fills it, then `argbuf <addr> ffi-call`.

EXACT RX-segment offsets (CODE-OFF=0x1000 leaves ample room; all fixed,
independent of code size — emit with M-PAD to each):
- 0x000 ELF header (64). e_phnum=4, e_phoff=0x40, e_entry=VMBASE+CODE-OFF.
- 0x040 phdrs: PT_LOAD(RX off0 va0x400000 filesz=memsz=TEXTSZ flags5 align0x1000),
  PT_LOAD(RW off TEXTSZ va0x500000 filesz=memsz=0xC0 flags6 align0x1000),
  PT_INTERP(off0x120 va0x400120 sz27 flags4 align1),
  PT_DYNAMIC(off TEXTSZ va0x500000 sz0xB0 flags6 align8). 4×56=224 → 0x120.
- 0x120 .interp "/lib/ld-linux-aarch64.so.1\0" = 27 B.
- 0x140 .hash (SysV) u32[6]: nbucket=1 nchain=3 bucket[0]=1 chain=[0,2,0]. 24 B.
- 0x158 .dynsym 3×24: sym0=null; sym1 dlopen(st_name=1,info=0x12,shndx=0);
  sym2 dlsym(st_name=8,info=0x12,shndx=0). Elf64_Sym=name4 info1 other1 shndx2 value8 size8.
- 0x1A0 .dynstr "\0dlopen\0dlsym\0libc.so.6\0" = 24 B (dlopen@1, dlsym@8, libc@14).
- 0x1B8 .rela.dyn 2×24: r_offset=GOT slot vaddr, r_info=(symidx<<32)|R_AARCH64_GLOB_DAT(0x401), r_addend=0.
- pad to 0x1000 → code.

RW segment (file off TEXTSZ, va 0x500000): .dynamic (11×16=0xB0) then GOT (2×8=16, zeros).
.dynamic tags (Elf64_Dyn=tag8 val8): HASH=0x400140, STRTAB=0x4001A0, SYMTAB=0x400158,
STRSZ=24, SYMENT=24, RELA=0x4001B8, RELASZ=48, RELAENT=24, NEEDED=14, FLAGS=DF_BIND_NOW(8), NULL.

BLAST RADIUS CONFIRMED — TWO image paths, BOTH must go dynamic (both produce an
executable bin/hb): (1) `BUILD-ELF`/`BUILD-IMAGE` (AOT/maker), (2) `BUILD-SNAP-HDR`
(src/habu/snap.f — "the image writer run by bin/hb", the self-host refresh path
that `-- install` actually uses). Factor shared dynamic emitters in elf.f used by
both. snap.f `SNAPGO` appends code+data+trailer after CODE-OFF as the text
content; it must ALSO append the RW segment (.dynamic+GOT) at file offset =
sfts (page-aligned text size), and BUILD-SNAP-HDR's returned sfts/phdr RW offset
must point there. Validate the self-host FIXPOINT (the snapshot path) on zed, not
just BUILD-ELF.

INCREMENTAL VALIDATION on zed: (a) emit a dynamic ELF, `readelf -hl` structural
check; (b) ld.so loads + runs it (exit 0); (c) it self-hosts (fixpoint); (d)
`DLOPEN-SLOT @` is non-zero and `ffi-call`ing it dlopens libcuda. Each step
gated before the next.
