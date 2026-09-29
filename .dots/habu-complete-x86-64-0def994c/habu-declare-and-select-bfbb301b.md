---
title: Declare and select x86 scalar float forms
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.484363+03:00"
blocks:
  - habu-emit-x86-frame-d8d25223
---

Problem: `src/compiler/native/x64ir.f` declares no float forms; selection refuses with `E-X64SEL-FLOAT`. First half of the float work (dialect and selection); C7b emits and executes.
Acceptance: `x64ir.f` gains the SSE2 forms (dialect MINOR bump) for HIR `fconst fadd fsub fmul fdiv fneg fabs fsqrt flt fgt feq fltz feqz intreal realint bitsreal realbits` (`src/compiler/native/hir.f:81-97`); `select-x64.f` lowers each; XMM allocation through the fpr file; proven through allocation and validation.
Files: `src/compiler/native/x64ir.f`, `src/compiler/native/select-x64.f`, `src/arch/x86-64/machine.f`, `test/compiler/x64-select.f`, `test/compiler/x64-regalloc.f`.
Verify: spark `bin/hb --load test/compiler/x64-select.f`, `test/compiler/x64-regalloc.f`, `test/compiler/x64ir.f`.
Depends: habu-emit-x86-frame-d8d25223 (C1).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
