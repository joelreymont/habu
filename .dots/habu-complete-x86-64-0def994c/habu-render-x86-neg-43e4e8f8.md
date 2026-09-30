---
title: Render x86 shl and shr
status: active
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.441939+03:00"
---

Problem: `src/compiler/native/emit-x64.f:40-51` refuses `neg`, `shl` and `shr`; the variable shifts take their count in the fixed register `rcx` (`x64ir.f` schema).
Acceptance: the count is copied into `rcx` and the source and count stay live after; counts 0, 1, 63 and 64 (masked to 6 bits per `INTEL.md:221-223`) and `neg` execute natively on the ThinkPad.
Files: `src/compiler/native/emit-x64.f`, `test/compiler/x64-emit.f`, `test/x86-64-peer-routines.f`.
Verify: spark `bin/hb --load test/compiler/x64-emit.f` and `bin/hb --load test/compiler/x64-chain.f`; ThinkPad routine images (C8).
Depends: habu-run-emitted-x86-b704f918 (C8).
Route: direct.
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-render-x86-neg-43e4e8f8.

Preflight corrections (these override the lines above where they differ):
- Scope: `shl` and `shr` only. `neg` stays refused: HIR has no `negate`, so nothing selects `x64.neg` (`docs/x86-64.md:287`), and K5 builds its bodies from HIR. `MACHINE-REFUSE-CASES` keeps `NEGATOR-EMIT` as its `E-X64EMIT-FORM` case (`test/compiler/x64-emit.f:984-988`). Rendering `neg` belongs to the slice that first selects it.
- Seam: `SHIFT-RULE` (`select-x64.f:1144-1149`) and `EMIT-SHIFT-CL` (`1131-1140`) copy the count into the operand the schema fixes to rcx (`x64ir.f:1044`); `ENC-SHL-CL`/`ENC-SHR-CL` (`asm.f:667-668`). Fixtures: HIR `( a n -- )` modules that read `a` and `n` after the shift, run natively through `ROWS,`/`CASE2,`, counts 0, 1, 63 and 64 (the hardware masks to 6 bits, `x64ir.f:141-144`, `docs/x86-64.md:279`; `INTEL.md:221-223` does not exist).
- Update `select-x64.f:56-58` ("renders none of `x64.shl`, `x64.shr`").
- Base: K2 (`xlvxmoou`) merged with C8 (`wptxqqns`); host engine K2's product `6f6ae5b9…`. Route: direct once K2 and C8 are on master.
- The clause "count live after" moved to habu-pin-schema-fixed-1983d191 (the allocator grants a fixed operand only as a want); this leaf's fixtures do not read the count after the shift.
