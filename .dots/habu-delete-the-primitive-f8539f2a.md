---
title: Delete the primitive reach classes
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T23:56:43.974174+01:00"
---

Problem: src/habu/prims.f's REACH classes (:22-33), each row's REACH clause (REACH-CODE :239, F-REACH :89) and PRIM-SPEC:REACH@ (:284) are read by no build: test/prim-parity.f REF-REACH (:534-540) is their only reader, so a row's class decides nothing. Acceptance: the classes, the clause on every row, the field and REF-REACH are gone and an engine build gives the same bytes. Depends: none. Claim: unassigned.
