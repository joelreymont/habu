---
title: Derive eq on a wide PRODUCT
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T18:36:56.441942+01:00"
---

Problem: DERIVE eq on a 128-field PRODUCT is rejected E-TOO-MANY-LOCALS (`local q63:n is one over the 64 locals` in tdq128:eq): the derived EQ takes locals per field, and a definition holds at most 64 locals (src/habu/interpreter.f:1673), a limit Habu set itself. A 63-field PRODUCT's EQ is not certified either, at its local p61:n. Acceptance: a PRODUCT of 256 one-cell fields with DERIVE eq hash loads, and its EQ answers true for equal values and false for values that differ in one field, as a type-decl test. Depends: none. Claim: unassigned.
