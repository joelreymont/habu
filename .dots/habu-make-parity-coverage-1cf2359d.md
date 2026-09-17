---
title: Make parity coverage row-exact for overloaded names
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T00:03:49.511291+03:00"
---

Problem: test/prim-parity.f marks coverage by NAME (habu-specify-the-engine-fcbcee25 worker 2): 'CASES +' covers all three + rows in src/habu/prims.f although only the numeric arm's instantiation ran, because the table has no vocabulary for naming one overload of a primitive; the pointer overloads of + - 1+ 1- = < > <> <= >= cell+ char+ are therefore counted covered by their numeric sibling's cases. Acceptance: a type tag on the CASES line (the row's first input type, or the row index) and, if needed, in the table, so a case set names one row and the coverage report counts rows, with the pointer overloads gaining their own case sets where an (inputs -> outputs) case exists and being reported uncovered otherwise. Files: test/prim-parity.f, src/habu/prims.f (only if the row needs a tag). Verify: the gate; test/run.f. Depends: habu-specify-the-engine-fcbcee25 landing. Ownership: parity gate. Claim: unassigned.
