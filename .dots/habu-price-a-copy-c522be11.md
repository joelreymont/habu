---
title: "Price a copy under the caller's own register pressure"
status: open
priority: 2
issue-type: task
created-at: "2026-08-04T23:13:36.124967+02:00"
---

The inline size rule (src/compiler/native/inline.f, SMALL?) now compares the callee's MEASURED body (A64EMIT:BODY-INSNS) with the most a call site of that arity can cost. What it still cannot see is what those operations cost in the CALLER: the splice replays them into a caller holding its own live values, and the caller's register allocator may spill where the callee's did not. The rule's header argues this does not favour the call - a call site writes every live value the callee could destroy into the caller's data stack and reads it back, minus the few KEEP-N leaves in registers - but that argument is not measured. The two corpus shapes that would show it, CODEGEN-CORPUS4:CALL-PRESSURE and PRESSURE-LOOP, are the two rows the chain cannot compile yet (spilling inside a loop), so what the committed tables adjudicate today is only that it does not appear in the ten rows they reach. Work: get those two rows compiling, then measure a copied body against a called one under real pressure and decide whether the record needs a pressure term at all. Blocked on the loop-spilling capability.
