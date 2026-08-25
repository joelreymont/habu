---
title: Build exit inside a dispatch arm
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T18:45:01.484445+02:00"
---

Found by the return-stack join landing (8db52e9e): exit inside a case/MATCH arm refuses E-NELAB-CTRL (-8502) while the same exit inside an if compiles - measured minimal: : XM1 ... case 1 of drop 7 exit endof ... refuses with NO return stack in the body, so this is the arm walk not accepting a path that ends inside it, pre-existing and unrelated to parked values. Real population: PTXIR-NODE-INTERN (lib/ptx/ir.f), the one former >r row that now stops here. Files: src/compiler/native/elaborate.f (the SK/DO arm walks around open-match). Depends: none.

CENSUS 2026-08-14 (master aa16d854, `src lib`, one process, 366 files,
3947 examined / 3358 compiled / 589 refused): E-NELAB-CTRL is TWO rows
and both are this class. PTXIR-NODE-INTERN (lib/ptx/ir.f), which this
leaf already names, and a second member it does not: ENV-PCT
(lib/test/budget.f:63), `GETENV STR>NUMBER? MATCH option none OF
T-BUDGET-SELF-PCT exit ENDOF some OF ENDOF ;MATCH T-BUDGET-CLAMP`.
Reduced by a two-arm fixture through the census's own entry
(NMIGRATE:MEASURE-HELD): the same MATCH body refuses -8502 with `exit`
in the `none` arm and COMPILES with the arm falling through, no return
stack anywhere in either - so the arm walk, not the exit, and not
parked values. Acceptance should name both rows; closing this leaf
takes the census's whole CTRL bucket to zero.
