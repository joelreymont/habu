---
title: Derive eq on a PRODUCT of more than 63 fields
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T18:36:56.441942+01:00"
---

Problem: DERIVE eq on a 127-field PRODUCT is rejected E-TOO-MANY-LOCALS: the derived EQ takes locals per field, and a definition holds at most 64 locals (src/habu/interpreter.f:1709), a limit Habu set itself. Acceptance: a PRODUCT of 256 one-cell fields with DERIVE eq hash loads, and its EQ answers true for equal values and false for values that differ in one field, as a type-decl test. Depends: habu-load-a-product-69feaada. Claim: unassigned.
