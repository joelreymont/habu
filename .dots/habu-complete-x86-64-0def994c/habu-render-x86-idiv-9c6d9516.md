---
title: Render x86 idiv with Habu semantics
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.451004+03:00"
---

Problem: `src/compiler/native/emit-x64.f:40-51` refuses `idiv`; Habu requires `MIN-N -1 /` to give `MIN-N` and a zero divisor to throw `E-DIV-ZERO` (`src/habu/arith-abi.f`; `docs/forth-card.md:184-186`).
Acceptance: quotient and remainder over every sign combination; a zero divisor reaches `E-DIV-ZERO` through `x64.throw-entry`; `MIN-N -1` gives `(MIN-N, 0)`; the RDX early-clobber hazard is tested through allocation plus render, not bytes alone; executed natively on the ThinkPad.
Files: `src/compiler/native/emit-x64.f`, `src/compiler/native/select-x64.f` if the copy shape changes, `test/compiler/x64-emit.f`, `test/x86-64-peer-routines.f`.
Verify: spark `bin/hb --load test/compiler/x64-emit.f` and `bin/hb --load test/compiler/x64-chain.f`; ThinkPad routine images (C8).
Depends: habu-run-emitted-x86-b704f918 (C8), habu-render-x86-trap-33eba82b (C5).
Route: direct.
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-render-x86-idiv-9c6d9516.

Preflight corrections (2026-09-30; override the lines above where they differ):
- HIR `div` bakes the host `throw` as `x64.throw-entry` (`select-x64.f:1184-1187,1203-1204`; `NDICT:CALL-TARGET` reads this engine's dictionary, `dict.f:288`), so no C8 image can stage a stand-in at that address; the rows read only the quotient (`select-x64.f:1206`) and HIR has no remainder opcode (`hir.f:56`). Proof is therefore split:
- Native positives: HIR `div` (shape `test/compiler/x64-select.f:322-327`) through `ROWS,`, `CASE2,` images over the four sign pairs and `MIN-CELL -1 -> MIN-CELL`; the baked host `throw` is never reached. rel32 reach holds: both bases are `$400000` (`src/os/linux/elf.f:29`, `src/os/linux-x86-64/elf.f:38`), as the trap case `test/compiler/x64-emit-fixture.f:1528-1539` proves with `die`.
- Native cold side and remainder: a machine-dialect `x64.idiv` module in `x64-emit-fixture.f` (as `test/compiler/x64-regalloc.f:475-489 M-IDIV`; allocated as `NEGATOR-EMIT`, fixture 1036-1040) whose `x64.throw-entry` is the stand-in's `POSITION`, staged as `TERMINAL-IMAGE` (`test/x86-64-peer-routines.f:418-426`). The render pushes `ARITH-ABI:E-DIV-ZERO` in the store-then-advance `G-PUSH` shape (`src/arch/x86-64/rt.f:60-65`) that x86 `throw` pops (`src/habu/kernel-x64.f:691,1251`), so the existing `TERMINAL-CASE,` plus `STAND-IN, a E-DIV-ZERO` (`test/x86-64-peer-harness.f:181-186,226-230`) check it unchanged. Remainder: the same module reads result 1 and publishes it (`x64.dstore`/`x64.dpublish` M-helpers beside `M-STORE`, fixture 862-882, or a harness `CASE0,`); this is the first machine-dialect trip through an image, and refusals met there are in scope.
- Emitter: `BND-THROW-ENTRY`/`THROW-ENTRY-OF` beside `emit-x64.f:196,268,1096` (`BIND-DIALECT`, `emit-x64.f:1081-1100`, binds no `KEY-THROW-ENTRY` today, `x64ir.f:722-723`); `require src/habu/arith-abi.f` (as `emit.f:42`); the throw call as `PUT-TRAP` writes it (`emit-x64.f:682-685`); intra-op displacements are zero under `MEAS` (`390-393`) because `OP-SIZE` (764-772) measures the whole op in the scratch sink. Semantics: `docs/x86-64.md:307-310`.
- Files: `test/compiler/x64-emit-fixture.f` (the `x64-emit.f` entry is three lines); add `test/x86-64-peer-harness.f` (if `CASE0,`) and `docs/x86-64.md` (219-223, 391-392).
- Cross-leaf: the throw-entry call is an absolute-entry site; C6 (`habu-record-symbolic-x86-10037f07`) files its call row. Whichever of C3 and C6 lands second adapts.
- Pre-change failure: HIR `div` through `2 1 DSTACK-EMITTED` and `EMITTED` throws `E-X64EMIT-FORM` (`emit-x64.f:730`).
