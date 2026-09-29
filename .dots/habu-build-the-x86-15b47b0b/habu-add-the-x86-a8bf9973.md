---
title: Add x86-64 runtime moves and per-target DSTACK
status: active
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.526025+03:00"
---

Problem: `src/habu/layout.f:34` `ENGINE-GPR:DSTACK` is the shared `19` (consumed by `src/arch/arm64/machine.f:84,150` and `src/habu/rt.f:23`; the x86 side already has `X64IR:R-DSP 12` and `X64M:DSTACK-GPR`, `src/compiler/native/x64ir.f:180,346`), and the x86 data-stack moves have no home: the seam's `G-POP`/`G-PUSH` take x86 register numbers and `test/x86-64-emit.f` carries recording stubs (cross-build obligation (5)).
Acceptance: `ENGINE-GPR` selects `DSTACK` (`12`) and the reserved mask (`rbx rbp r12-r15`) on `HB-TARGET-LINUX-X86-64?`, fail-closed otherwise; `src/arch/x86-64/rt.f`: `G-PUSH`/`G-POP` with x86 register numbers (7 rdi, 6 rsi, 2 rdx, 0 rax), the byte-appending stencil consumer (`C-EMIT-STENCIL`'s x86 twin), an `RT:DSTACK-AGREE` twin against `X64IR:R-DSP`; `test/x86-64-emit.f` drops its recording stubs; `layout.f` also declares `ENGINE-MAIN:XT-CELL`, a DATA cell declared like `NCOMP-DISPATCH:XT-CELL` (`src/compiler/native/compiler.f:646`) and unused until a boot reads it (I10c on ARM64, X4d on x86); the ARM64 engine byte-identical (chain). Discharges cross-build obligation (5).
Files: `src/habu/layout.f`, new `src/arch/x86-64/rt.f`, `test/x86-64-emit.f`.
Verify: spark `bin/hb --load test/x86-64-emit.f`; rebuild; chain gen2==gen3; gate.
Depends: habu-add-the-x86-aad02c7e (K1). Serialise with P2 on `layout.f`.
Route: Alder (shared: src/habu/layout.f).
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-add-the-x86-a8bf9973.
Preflight corrections (these override the lines above where they differ):
- Base: P2's bookmark `intel/habu-represent-x86-live-729a7ac6` (`69ce2484`), which serialises `layout.f`; P2's product engine is `ffa95423…`.
- DSTACK and mask: `ENGINE-GPR` publishes `19 constant A64-DSTACK`, `12 constant X64-DSTACK` and the x86 reserved mask (bits 3 5 12 13 14 15) unconditionally. `DSTACK ( -- n )` and `MASK ( -- n )` are local-free colon definitions that select on `HB-TARGET-LINUX-X86-64?` / `HB-TARGET-LINUX? HB-TARGET-MACOS? or`, else `die`. `X64RT:DSTACK-AGREE` compares `X64IR:R-DSP` with `ENGINE-GPR:X64-DSTACK`, not with the host-selected `DSTACK`: `src/os/linux/target.f:9-10` makes the x86 predicate false on spark, so a twin against `DSTACK` would compare 12 with 19 and die at load (the `src/habu/rt.f:22-25` precedent). `layout.f` cannot branch at top level (P2 `layout.f:1619-1621`), and its mask members are ARM64 numbers (`layout.f:4-7`).
- Engine bytes: a new constant in `layout.f` cannot leave the engine byte-identical (`native-runtime.f:55` marks `layout.f` provided). Acceptance is: the product engine differs from `ffa95423` only by the `layout.f` additions; chain gen2 == gen3; gate green, including `test/compiler/native-effect.f:205,467`; record the new sha256.
- `src/arch/x86-64/rt.f` is package `X64RT`: `G-PUSH ( n -- )` is `mov [r12],r; add r12,8`, `G-POP ( n -- )` is `sub r12,8; mov r,[r12]` (r12 points past TOS, matching `emit-x64.f:527-536,551`), and `EMIT-STENCIL ( ptr u8 n -- )` appends via `BUF:APPEND-SPAN` into `X64CODE:ASM-SINK`. The encoders exist (`asm.f:409,439,476,479`; r12 SIB `asm.f:214-218`). The ARM64 `G-PUSH`/`G-POP` are globals (`rt.f:158-161`) that `src/os/linux-x86-64/proc-watch.f:19,24` and `proc-control.f:16-23` consume bare; those two files move to `using X64RT` (`proc-watch.f:9-11` says the ownership gate flags globals), and `test/x86-64-emit.f` `PROC-CASES` (241-262) pin the move bytes. K3 inherits the A64-global/`using` collision (`habu2.f:3985-3988`), as K1 recorded.
- `ENGINE-MAIN:XT-CELL`: `package ENGINE-MAIN public $3808 constant XT-CELL ;package` in `layout.f`, following `$358 constant XT-CELL` (P2 `layout.f:960-962`) and the `layout.f:920-923` precedent; its claim row and NAME go in `src/habu/data-claims.f` (claim `data-claims.f:222`, name `:104`). No claimant exists in `$3808..$39F8` (`data-claims.f:275`, then `$3A00 $288`). The `tools/native-layout.f` SLOTS row belongs to I10c.
- x86 `SYS-PUSH` (`docs/x86-64.md:417`, a `setc`) is not K2's: it moves to K6 (`habu-emit-x86-syscall-a0d501db`), its first consumer.
- Pre-change failing check: the master ARM64 engine loading `src/arch/x86-64/asm.f`, `icode.f`, then `s" src/os/linux-x86-64/proc-control.f" required` dies `E-UNDEFINED: G-POP`, exit 70. The x86 `DSTACK` half is unobservable until an x86 engine exists.
- Files: `src/habu/layout.f`, `src/habu/data-claims.f`, new `src/arch/x86-64/rt.f`, `src/os/linux-x86-64/proc-watch.f`, `src/os/linux-x86-64/proc-control.f`, `test/x86-64-emit.f`. Route stays Alder (`layout.f`, `data-claims.f`).
