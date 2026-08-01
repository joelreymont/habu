---
title: Test the multi-block allocator directly
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-01T17:42:23.518802+02:00\""
---

test/compiler/native-regalloc.f covers only the straight-line path of src/compiler/native/regalloc.f: every fixture in it is one function of one block. The whole multi-block section - the linear order, the backward liveness, the hull intervals, the block-argument classes, the schema-tie unions and the copy coalescing of step five - is exercised only end to end, through test/compiler/native-chain.f, which compiles real source and runs the bytes. That is a real test but it is the wrong grain for this pass: it cannot state a class, cannot assert which values ended up sharing a register, and cannot build the shapes that should be REFUSED (a class whose members are live at once, a tie whose ends are live at once, a routine whose classes do not fit the pool). Two of those refusals - E-A64RA-EDGE and E-A64RA-TIE on the multi-block path - are today reachable only by mutating the compiler. Wanted: multi-block fixtures in native-regalloc.f built the way the straight-line ones are, asserting the assignment (which values got one register) and the refusals by name. Found while landing habu-coalesce-the-edge-5ac08118, which exposed a real defect in that untested section: schema ties were never unioned and held only because FREE-REG happened to hand back the register the tied operand had just released.

Claim: agent=mbtestlane workspace=.jj-ws/habu-test-the-multi-139f37ac
