---
title: Lower trapping arithmetic in the native chain
status: open
priority: 3
issue-type: task
created-at: "2026-08-01T13:20:42.116954+02:00"
---

src/compiler/native/select.f refuses a source operation whose schema declares it may trap unless the rule that lowers it keeps the trap (TRAP-PRESERVED?). Division keeps it - a64.sdiv is the zero-divisor guard and the divide together - but hir.add, hir.sub and hir.mul under CNUM-OVERFLOW:TRAP do not: ARM64's Add, Sub and Mul wrap, and a trapping addition needs a flag-setting form, a conditional branch and a trap target, none of which is in the A64IR dialect (src/compiler/native/a64ir.f). Refused today as E-A64SEL-TRAP, pinned by test/compiler/native-select.f TRAP-REFUSE-CASES. Wanted: adds/subs/muls that set the flags, a branch on overflow, and whatever the engine's own trapping arithmetic does at the target, so a trapping unit compiles instead of being refused.
