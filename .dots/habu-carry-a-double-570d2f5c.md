---
title: Carry a double across a block edge and a call
status: open
priority: 2
issue-type: task
created-at: "2026-08-02T23:04:01.314182+02:00"
---

A double may not cross a block edge or a call in the scalar float leaf: src/compiler/native/elaborate.f NO-REAL-CK refuses one on the compile-time vector at TERM-BR-H and at CALL-OPERANDS+, and NO-REAL-LOCAL-CK refuses a crossing local that holds one, all with E-NELAB-TYPE. The reason is that OPEN-ARGS-H types a block argument before the values that will reach it are known, and CALL-RESULTS+ does the same for a call's answers - both state CELL-TYPE positionally. What is needed is the type per vector slot at the seam, so a block argument and a call result take the type of the value that really arrives. The machine side is already done: src/compiler/native/select.f OPEN-ARG1 gives a float-typed block argument an FPR, EMIT-COPY copies one with a64.fmovdd, and the allocator refuses a union across the two files with E-A64RA-FILE. Removing the guard today lands on E-IR-VERIFY-SUCCARG (-8088) for an edge and E-IR-VERIFY-OPTYPE (-8093) for a call, measured. This unblocks the three accumulation rows, T-SGD! and T-REL-L2 of tools/codegen-compare-corpus3.f.
