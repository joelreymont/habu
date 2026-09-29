---
title: Render x86 trap and codeaddr
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.467577+03:00"
blocks:
  - habu-emit-x86-frame-d8d25223
  - habu-boot-and-exit-367c46f5
  - habu-run-emitted-x86-b704f918
---

Problem: `src/compiler/native/emit-x64.f:40-51` refuses `trap` and `codeaddr`.
Acceptance: `trap` reaches the runtime `throw` entry; `codeaddr` is a recorded CODE site (a `MOVABS` site row); quotations execute natively on the ThinkPad.
Files: `src/compiler/native/emit-x64.f`, `test/compiler/x64-emit.f`, `test/x86-64-peer-routines.f`.
Verify: spark `bin/hb --load test/compiler/x64-emit.f` and `bin/hb --load test/compiler/x64-chain.f`; ThinkPad routine images (C8).
Depends: habu-run-emitted-x86-b704f918 (C8).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.

Preflight corrections (these override the lines above where they differ):
- Target: `x64.trap` carries `x64.trap-entry` = `NTRAP:ROUTINE$` = `die` (`trap.f:135-136`, `select-x64.f:781-784`, `x64ir.f:1466-1476`); `throw` is `idiv`'s `x64.throw-entry` (C3). The render publishes the three trap cells (`TRAP-CELLS`, `select-x64.f:202,797-803`) and branches to `x64.trap-entry`, twin of `emit.f:1441-1445`.
- Native check: a trap never returns. Fixtures use HIR `terminal` (the entry attribute, `test/compiler/x64-select.f:416-427`) with a stand-in entry staged as `WORDCALL-IMAGE` stages its callee; the stand-in checks the three cells and exits with its own status, listed in the manifest. HIR `trap` resolves `die` from the host dictionary (`NDICT:CALL-TARGET`), meaningless in an x86 image. The stand-in is a harness case in `test/x86-64-peer-harness.f`, which K3 edits, so this leaf stacks on K3.
- `codeaddr`: x64ir has no indirect call (`x64ir.f:60-110`) and `quot` selects `codeaddr` only (`select-x64.f:1275-1281`); the check is that the answer equals `POSITION` + `FUNCTION-OFFSET@ 1`, executed natively. The site is the existing `SITE+` with `ADDR-CODE` (`emit-x64.f:392-397`, `x64ir.f:244`); C6 converts it later.
- Binds `BND-TRAP-ENTRY` and `BND-FUN` in `BIND-DIALECT` (`emit-x64.f:1000-1015`), beside C1's `KEY-SLOT`/`KEY-FRAME`: stack on C1 as well.
- Files add: `test/x86-64-peer-harness.f`. Base: K3 and C1. Route: direct once its bases are on master.
