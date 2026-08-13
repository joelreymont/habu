---
title: Build exit inside a dispatch arm
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T18:45:01.484445+02:00"
---

Found by the return-stack join landing (8db52e9e): exit inside a case/MATCH arm refuses E-NELAB-CTRL (-8502) while the same exit inside an if compiles - measured minimal: : XM1 ... case 1 of drop 7 exit endof ... refuses with NO return stack in the body, so this is the arm walk not accepting a path that ends inside it, pre-existing and unrelated to parked values. Real population: PTXIR-NODE-INTERN (lib/ptx/ir.f), the one former >r row that now stops here. Files: src/compiler/native/elaborate.f (the SK/DO arm walks around open-match). Depends: none.
