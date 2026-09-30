---
title: Render x86 cmpsel and selz
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.459377+03:00"
---

Problem: `src/compiler/native/emit-x64.f:40-51` refuses `cmpsel` and `selz`; RFLAGS is never an SSA value.
Acceptance: both selects render, and the result/source aliasing cases execute natively on the ThinkPad.
Files: `src/compiler/native/emit-x64.f`, `test/compiler/x64-emit.f`, `test/x86-64-peer-routines.f`.
Verify: spark `bin/hb --load test/compiler/x64-emit.f` and `bin/hb --load test/compiler/x64-chain.f`; ThinkPad routine images (C8).
Depends: habu-run-emitted-x86-b704f918 (C8).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.

Preflight corrections (these override the lines above where they differ):
- Parked, off the path to X6: nothing selects `x64.cmpsel` or `x64.selz` (`select-x64.f:14`, `docs/x86-64.md:268-269`) and HIR has no select op (`hir.f:55-100`), so no routine the kernel or the engine compiles reaches them. K5 no longer depends on this dot. The render lands with the slice that first selects the fused forms, whose fixtures then run natively through C8's family.
- When it runs, enumerate the aliasing cases as IR identities, since the allocator picks registers: `cmpsel` `op2 = op0`, `op3 = op1`, `op3 = op0`, `op2 = op3`; `selz` `op1 = op0`, `op2 = op0`; each with the value that must survive (`DEF-CMPSEL` `x64ir.f:1135-1149`, `DEF-SELZ` `1153-1165`).

Reach on ARM64 (measured at tier 1): if-conversion selects both forms from source (`src/compiler/native/select.f:3239-3276`); `: S1 ( n n -- n ) 2dup < if drop else nip then ;` compiles to `cmp`, `csel`. The x86 selector needs the same renders when it if-converts, so this dot stays open and the landed `DEF-CMPSEL` and `DEF-SELZ` stay. `neg` has no selector on either backend.
