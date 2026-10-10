---
title: If-convert diamonds in the x86 selector
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T18:17:44.368186+03:00"
---

Problem: the x86 selector is correct and unfused (`src/compiler/native/select-x64.f:12-17`): no selection becomes `x64.cmpsel` or `x64.selz`, so the renders habu-render-x86-cmpsel-279135be landed (`DEF-CMPSEL` `x64ir.f:1252`, `DEF-SELZ` `1270`) run only from staged machine IR. ARM64 if-converts a diamond into a select inline (`REGION-PICK`, `src/compiler/native/select.f:3268-3276`): `: S1 ( n n -- n ) 2dup < if drop else nip then ;` compiles to `cmp`, `csel`.
Acceptance: x86 selection turns the diamonds ARM64 converts into `x64.cmpsel` or `x64.selz`; S1 and a zero-test diamond compile to `cmp`/`test` and `cmovcc` with no conditional branch and execute natively on the ThinkPad with both arms taken. x86 `cmpsel` names the failing answer as operand 2 (tied to the result) and the holding answer as operand 3, the reverse of ARM64 (`docs/x86-64.md:231-236`), so a selector shared with ARM64 swaps them.
Files: `src/compiler/native/select-x64.f`, `test/compiler/x64-chain.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md`.
Verify: spark `bin/hb --load test/compiler/x64-chain.f`; ThinkPad routine images (C8 family).
Depends: none (off the path to X6).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
Superseded: the one-pass codegen (docs/architecture.md, "The codegen is one pass over the checked events") deletes the tier-1 code this fixes; its reproducers become that codegen's cases. Do not start.
