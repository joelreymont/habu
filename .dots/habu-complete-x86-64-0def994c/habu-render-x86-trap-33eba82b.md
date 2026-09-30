---
title: Render x86 trap and codeaddr
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.467577+03:00"
---

Problem: `src/compiler/native/emit-x64.f:40-51` refuses `trap` and `codeaddr`.
Acceptance: `trap` reaches the runtime `throw` entry; `codeaddr` is a recorded CODE site (a `MOVABS` site row); quotations execute natively on the ThinkPad.
Files: `src/compiler/native/emit-x64.f`, `test/compiler/x64-emit.f`, `test/x86-64-peer-routines.f`.
Verify: spark `bin/hb --load test/compiler/x64-emit.f` and `bin/hb --load test/compiler/x64-chain.f`; ThinkPad routine images (C8).
Depends: habu-run-emitted-x86-b704f918 (C8).
Route: direct.
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-render-x86-trap-33eba82b.

Preflight corrections (these override the lines above where they differ):
- Target: `x64.trap` carries `x64.trap-entry` = `NTRAP:ROUTINE$` = `die` (`trap.f:135-136`, `select-x64.f:781-784`, `x64ir.f:1466-1476`); `throw` is `idiv`'s `x64.throw-entry` (C3). The render publishes the three trap cells (`TRAP-CELLS`, `select-x64.f:202,797-803`) and branches to `x64.trap-entry`, twin of `emit.f:1441-1445`.
- Native check: a trap never returns. Fixtures use HIR `terminal` (the entry attribute, `test/compiler/x64-select.f:416-427`) with a stand-in entry staged as `WORDCALL-IMAGE` stages its callee; the stand-in checks the three cells and exits with its own status, listed in the manifest. HIR `trap` resolves `die` from the host dictionary (`NDICT:CALL-TARGET`), meaningless in an x86 image. The stand-in is a harness case in `test/x86-64-peer-harness.f`, which K3 edits, so this leaf stacks on K3.
- `codeaddr`: x64ir has no indirect call (`x64ir.f:60-110`) and `quot` selects `codeaddr` only (`select-x64.f:1275-1281`); the check is that the answer equals `POSITION` + `FUNCTION-OFFSET@ 1`, executed natively. The site is the existing `SITE+` with `ADDR-CODE` (`emit-x64.f:392-397`, `x64ir.f:244`); C6 converts it later.
- Binds `BND-TRAP-ENTRY` and `BND-FUN` in `BIND-DIALECT` (`emit-x64.f:1000-1015`), beside C1's `KEY-SLOT`/`KEY-FRAME`: stack on C1 as well.
- Files add: `test/x86-64-peer-harness.f`. Base: K3 and C1. Route: direct once its bases are on master.

Preflight corrections, rev 2 (Fable, 2026-09-30; these override everything above where they differ):
- Base: master `37ba1863` (K3, C1, C8 landed). Route: direct. Files: `src/compiler/native/emit-x64.f`, `test/compiler/x64-emit-fixture.f` (`x64-emit.f` is a three-line entry), `test/x86-64-peer-harness.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (remove trap and codeaddr from the refused lists at `emit-x64.f:44-51` and docs 385-387; drop "trap has a rule but no positive case" at 327).
- Acceptance (replaces the Acceptance line): `x64.trap` renders as `DBYTES-OF PUT-DMOVE`, then `call rel32` (`PUT-CALL-TO`) to `x64.trap-entry` less the placement (as `ENTRY-TARGET`, `emit-x64.f:653-656`), the twin of `emit.f:1441-1445`. `x64.codeaddr` renders as `mov r64, imm64` = placement + `FUN-START` of `x64.fun`, filed with `SITE+ ADDR-CODE`. `die` and `execute` on x86 are K7's; no image at this leaf runs HIR `trap`.
- Terminal image: the routine declares `1 0 NBACK:L-DEAD NBACK:L-CALLED NBACK:WITH` (`X64ABI:NORET-FRAMED`, `passes.f:91-95`; `VNORET-CK`, `regalloc-verify.f:2354-2357`, needs every path to trap). This is the first x86 NORET trip through the rows; refusals met there are in scope. The stand-in (`X64HARNESS`, staged as `WORDCALL-IMAGE` stages its callee, `peer-routines.f:350-360`) checks the published argument cells below r12 (`BUILD-TERMINAL` publishes two, `x64-select.f:416-427`; HIR `trap` publishes `TRAP-CELLS` 3) and exits 0 on a match, nonzero on a mismatch; the calling case exits another nonzero status if control returns. The image's expected status is 0, as `WRITE-IMAGE` (`test/x86-64-peer-routines.f:156-166`) and `docs/bootstrap.md:176-177` define: no image has a status of its own.
- HIR `trap` bytes are checked host-side in `x64-emit-fixture.f` under `X64ABI:NORET-LEAF-FRAMED` (three cell operands, `hir.f:795-805`; template `test/compiler/native-trap.f`).
- Codeaddr image: a two-function HIR module (the second function named apart, `E-IR-FUN-DUP`, as `x64-emit-fixture.f:615-629` does for the machine dialect), `hir.quot` with `HIR:KEY-FUN 1`. The expectation is staged through a label: `RCX <lbl> MOVABS,` (an ABS64 site patched at `ASM-LINK`, `icode.f:198-209`), and `APPEND-ROUTINE` binds the label at `at + 1 FUNCTION-OFFSET@`. Host-side, `ADDR-SITE-KIND@` of the codeaddr site is `ADDR-CODE`.
- Lines on master: refusal `emit-x64.f:724-725`; `SITE+` 402-407; `BIND-DIALECT` 1050-1067; `select-x64.f` `TRAP-CELLS` 212, `TRAP-ENTRY`..`EMIT-TERMINAL` 791-821, `QUOT-FUN`/`EMIT-QUOT` 1294-1308; `x64ir.f` `DEF-TRAP` 1450-1460, `DEF-CODEADDR` 1464-1473, `ADDR-CODE` 237.
- Verify: ThinkPad, master's product `5f4d3321…` under qemu: `test/compiler/x64-emit.f`, `test/compiler/x64-chain.f`, `test/x86-64-peer-image.f`; the images natively.
