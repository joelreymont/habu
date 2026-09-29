---
title: Resolve x86 entry cells and dispatch the writer
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:29.008645+03:00"
blocks:
  - habu-link-the-shadow-74e41be7
  - habu-add-the-x86-a8bf9973
  - habu-select-the-build-610b4492
  - habu-boot-and-exit-367c46f5
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
