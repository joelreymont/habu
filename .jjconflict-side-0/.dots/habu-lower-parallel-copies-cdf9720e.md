---
title: Lower parallel copies at a multi-result return
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T20:34:55.493590+02:00"
---

src/compiler/native/regalloc.f pre-colours a returned value to the register the routine contract declares for it, and plans one register-to-register move (a64.mov, lowered by src/compiler/native/spill.f) when the value cannot be given that register at its definition. One move per returned value is enough when the values it displaces are not themselves returned values that still need their own registers. A convention that PERMUTES - a routine returning (a b) where a already sits in the register b must leave in - needs the moves ordered, and a cycle needs a temporary register or an exchange, which is the parallel-copy problem design section 7.10 names ('lower block-argument parallel copies'). Today the second allocation of the lowered module refuses such a shape by name (E-A64RA-FIXED: the declared register is held by a value the same return needs), which is fail-closed but not a compilation. Owner: A64RA plus the lowering pass. Blocked on nothing; wanted by any convention that returns more than one value in registers.
