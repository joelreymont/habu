---
title: Resolve x86 entry cells and dispatch the writer
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:29.008645+03:00"
blocks:
  - habu-link-the-shadow-74e41be7
---

Problem: nothing resolves the entry cells in an x86 image, and `NATIVE-EMIT:WRITE` has no x86 arm.
Acceptance: `ENGINE-MAIN:XT-CELL`/`APP-ENTRY:XT-CELL` resolution, the `.names` sidecar, the `NATIVE-EMIT:WRITE` x86 arm (over K3's require set) dispatching to the linker; `docs/x86-64.md` gains the write-time link section; `docs/porting.md` states that a port is seam + kernel + link arm with no cold route.
Files: `src/habu/link-x64.f`, `tools/native-emit.f`, `docs/x86-64.md`, `docs/porting.md`.
Verify: spark writes a linked image; ThinkPad `readelf -l`; X5 executes it.
Depends: habu-link-the-shadow-74e41be7 (X4c), habu-add-the-x86-a8bf9973 (K2: declares `ENGINE-MAIN:XT-CELL`, so X5 never waits on lane I). Serialise with K3 on `tools/native-emit.f`.
Route: Alder (shared: tools/native-emit.f, docs/porting.md).
Ownership: krait (Intel lane).
Claim: unassigned.

Preflight corrections from K3 (these override the lines above where they differ):
- This leaf, not K3, restructures `tools/native-emit.f`: lines 2-4 and 44-61 load under the target dispatch. The x86 arm loads `lib/byte-buffer.f`, `src/arch/x86-64/{asm,icode,rt}.f`, the x86 seam, `src/os/image-bytes.f` under `using X64CODE`, `boot-x64.f`, `kernel-x64.f` and `link-x64.f`, and never `src/arch/arm64/icode.f` (its globals collide with `X64CODE`). `src/habu/prof.f:47-50` dies there once the window's target is x86-64.
- The x86-host loaders of the seam files must load `src/arch/x86-64/asm.f`, `icode.f` (`sys.f` binds `using X64CODE`) and `rt.f` (`proc-watch.f` binds `using X64RT`), and key them: `tools/native-emit.f` `LOAD-IMAGE`'s x86 branch, `tools/build-fixpoint.f`'s fixpoint bundle, and `tools/hb-build-lib.f` `HBB-KEY-LINUX-X86-64-SOURCES`. Today an x86 host dies E-UNDEFINED at `proc-watch.f`'s `using X64RT`, and an `rt.f` edit leaves the build key unchanged (K2 review).
- Depends also: X3 (`habu-select-the-build-610b4492`), which makes the x86 arm reachable, and K3 (`habu-boot-and-exit-367c46f5`), which writes `boot-x64.f`.
- Files add: `tools/build-fixpoint.f`, `tools/hb-build-lib.f`.
C6 landing note: call/tail/trap rows name the instruction's first byte; the rel32 is at +1, today only the peer harness's `1 constant REL32-AT` (`test/x86-64-peer-harness.f`); the linker names that offset once, beside `asm.f`'s `MOV-RI64-IMM-OFF`.

Lead note (2026-10-01, NX `habu-name-the-exit-ab1864c1` landed): a writer loaded after the capture compiles for the window's machine, because `CHECKER-REG:SEAL` (`src/habu/native-runtime.f`) runs `NCOMP:INSTALL` and the window's `NABI:BINDING` then reads the window's `target.f`; in the x86 window `native-emit.f`'s closure compiled to x86 inside the AArch64 engine and threw -8787 (`E-X64EMIT-PLACE`) at `ICODE-MAP>PTR`. So the x86 writer is compiled before the window opens, or the post-capture compiler handover changes. Until then `WRITER-MACHINE-CK` (`tools/native-build-core.f`) stops a foreign-machine window before the writer with rc 74, pinned by `FOREIGN-WINDOW-CASE` in `test/native-build-entry.f`; this leaf replaces that stop. From X2b: native-build does not yet load `src/habu/aot-shadow.f` or call `AOT-CAPTURE:SHADOW-CAPTURE`; the driver loads it before the window and calls it after `CAPTURE`, and the writer refuses empty shadow sections. The shadow sections count against `AOT-SECTION-CAP`; their size for a whole-engine cross-build is unmeasured.

Lead note (2026-10-01, X4b `habu-link-records-and-647852d3` landed): `src/habu/link-x64.f` (`X64LINK:LAYOUT`) builds the records, wids, protected-wid bitmap and name index at write time (the index is deterministic across hosts, so the writer builds it and the boot does not run `seed-ndict!`); this leaf writes `DICT$`/`CODE$` into the region and `BITS$`, `INDEX$`, `WIDN`, `T0-CELL`, `HIDXP-CELL`, `CLAIMS` and the heap floor into DATA, and stops the boot replacing them. Load order: `aot-decl.f` needs `AOT-SECTION-CAP`, which only `src/arch/arm64/icode.f` defines, and that file's globals `CODE`, `LBL`, `ASM-LEN` make a later `using X64CODE` refuse (`E-USING-SHADOW-GLOBAL`); the x86 writer's closure cannot load arm64/icode.f, so this leaf moves `AOT-SECTION-CAP` to a target-neutral file or otherwise settles the order.
