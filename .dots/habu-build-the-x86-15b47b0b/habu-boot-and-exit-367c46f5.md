---
title: Boot and exit the x86-64 kernel skeleton
status: active
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.534293+03:00"
blocks:
  - habu-add-the-x86-a8bf9973
  - habu-run-emitted-x86-b704f918
---

Problem: no x86 kernel exists and `tools/native-emit.f` cannot build one: its lines 2-4 and 44-61 require the arm64 assembler, `rt.f`, `crash.f`, `prof.f`, `regalloc.f`, `habu1.f`, `jit.f` and `habu2.f` unconditionally, and `src/habu/prof.f:47-50` dies on x86. Milestone M2.
Acceptance: `src/habu/boot-x64.f` emits `_start`: `rsp` from the kernel entry, `rbp` user area, `r12` data stack, `r13` DATA base, `r14` dictionary, `r15` code pointer (`docs/x86-64.md:47-57`), runtime stacks with guard pages (`STACK-ABI`), argc/argv/envp/heap-floor cells, exit 0; `tools/native-emit.f` gains a target-dispatched require set, so the x86 arm requires `src/arch/x86-64/{asm,icode,rt}.f`, the x86 seam, `kernel-x64.f`, `boot-x64.f` and `link-x64.f`; skeleton only at this leaf; `docs/x86-64.md` gains the kernel inventory section.
Files: `src/habu/boot-x64.f`, `tools/native-emit.f`, `docs/x86-64.md` (kernel inventory).
Verify: spark builds `hb-x64-skel` and the ARM64 product unchanged (rebuild); ThinkPad `./hb-x64-skel; echo $?` prints 0; `readelf -l hb-x64-skel`.
Depends: habu-add-the-x86-aad02c7e (K1), habu-add-the-x86-a8bf9973 (K2). Serialise with X4d on `tools/native-emit.f`.
Route: Alder (shared: tools/native-emit.f).
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-boot-and-exit-367c46f5.
Load order (from K1): the x86 arm of `tools/native-emit.f` loads `lib/byte-buffer.f` and `src/arch/x86-64/icode.f` (`X64CODE`) before any x86 seam file (`sys.f` and `proc-watch.f` bind `using X64CODE`), loads `src/os/image-bytes.f` under `using X64CODE` (it sizes `MSIZE` from a bare `CODE-CAP-BYTES`), and does not load `src/arch/arm64/icode.f` (its globals collide with `X64CODE`, `E-USING-SHADOW-GLOBAL`).

Preflight corrections (these override the lines above where they differ):
- `tools/native-emit.f` is not changed at this leaf; its target-dispatched require set moves to X4d (`habu-resolve-x86-entry-cb671d4d`). `kernel-x64.f` (K5-K9) and `link-x64.f` (X4b) do not exist yet; the window's `target.f` follows the host's baked predicates (`src/os/linux/target.f:3-10`, `tools/native-build-core.f:159-175`), so no x86 arm is reachable before X3; and `NATIVE-EMIT:WRITE` (`native-emit.f:85-91`) writes a capture, not a bare skeleton.
- Register roles: `src/habu/layout.f` `ENGINE-GPR` names the five unnamed x86 VM registers, twins of ARM64's `layout.f:4-7` constants, and derives `X64-MASK` from them: `X64-INTERP 3` (rbx), `X64-RBASE 5` (rbp, the user area; twin `XREG-RBASE`, x20), `X64-DBASE 13` (twin `DBASE`, x26: the code-region base where records live), `X64-NDICT 14` (twin `NDICT`, x27: a record count), `X64-CP 15` (twin `CP`, x28). `docs/x86-64.md` "Machine model" says r13 is the data base in the `DBASE` sense (`habu2.f` `EM-SEED-DICT`), not the DATA region.
- `src/habu/boot-x64.f` (`package X64BOOT`): `_start` plus the guarded-stack mapper, twin of `STACK-GUARD:EMIT-MAP` (`src/habu/rt.f:116-140`). Skeleton values: rbp = `DATA-VA` mapped `MAP-ANON-PRIVATE-FIXED` (`src/os/linux-x86-64/sys.f`, as `habu2.f` `EM-DATA-INIT`); r13 = a region mapped fixed at `VMBASE REGION-OFF +` (`layout.f`), so X4a's `PT_LOAD` replaces the mmap in place; r14 = 0; r15 = r13 + `DICT-SIZE`; rbx = 0; the cells `EM-DATA-INIT`/`EM-FRAME-STACKS` fill (`RBASE-CELL`, `STACK-ABI:BASE/CAP-CELL`, `RETURN/LOOP-BASE-CELL`, `ARGC/ARGV/ENVP-CELL`, `BOOT-LAYOUT:HEAP-START-CELL`, `DP-CELL`); exit 0. `docs/x86-64.md` gains the kernel inventory: each x86 register and cell against its ARM64 twin and primitive (`data-base`, `dbase@`, `ndict@`, `cp@`; `prims.f:513-556`).
- Test: new `test/x86-64-skel-image.f` (`package X64SKEL`) writes `$HB_TMP/hb-x64-skel` and `hb-x64-skel-negative` (one push past `STACK-ABI:BOOT-BYTES`) through C8's `X64HARNESS` (`test/x86-64-peer-harness.f`): add a plain `X64HARNESS:WRITE` beside `WRITE-ELF` (which appends the peer `EXIT,`), with no copied load block. `SUITE x86-64-skel-image` beside the x86 rows of `test/gate-stdlib-cases.f`.
- Files: `src/habu/boot-x64.f`, `src/habu/layout.f`, `test/x86-64-peer-harness.f`, `test/x86-64-skel-image.f`, `test/gate-stdlib-cases.f`, `docs/x86-64.md`.
- Verify: ThinkPad `./hb-x64-skel a b; echo $?` prints 3 (argc read back through rbp from `ARGC-CELL`); `./hb-x64-skel-negative` dies SIGSEGV (the shell prints 139); `readelf -l hb-x64-skel`. `layout.f` is in every engine's prefix, so the ARM64 product changes: on spark rebuild it from K3's tree with the host engine, then run the five-generation chain (`tools/two-generation-build.f`) and the gate with the product engine. There is no failing check before the change: the skeleton is a new artefact.
- Base: K2 (`xlvxmoou`, `intel/habu-add-the-x86-a8bf9973`) merged with C8 (`wptxqqns`, `intel/habu-run-emitted-x86-b704f918`), or master once Alder lands both. C8 changes only tests and docs, so the host engine is K2's product `6f6ae5b9…`: master's `a3a6224b…` dies `E-UNDEFINED: ENGINE-GPR:X64-DSTACK` on `require src/arch/x86-64/rt.f`, because `require src/habu/layout.f` is a no-op on a booted engine.
- Route: Alder (`src/habu/layout.f`, `test/gate-stdlib-cases.f`).
