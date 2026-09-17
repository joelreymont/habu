---
title: Specify the engine primitives in one table
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T17:32:42.537487+03:00"
---

Problem: docs/x86-64.md (campaign habu-campaign-c6-targets-86bb56bb): the engine's primitives are an ARM64 assembly body in src/habu/habu1.f and a checker PRIM: row for the same name, so a second backend would fork the list. Acceptance: one machine-independent table src/habu/prims.f listing every primitive with its checker row and a reference implementation in checked Habu where one exists; the arm64 engine draws its rows from that table (no second copy of the effects); a parity gate test/engine-suite.f that runs the same cases against each backend's primitive bodies and the reference, green on arm64 first; the table is the contract the x86_64 and Cortex-M backends implement. Files: src/habu/prims.f, src/habu/habu1.f, src/core/checker.f (row source), test/engine-suite.f. Verify: bin/hb --load test/engine-suite.f; byte fixpoint; tools/bootstrap.sh check; test/run.f. Depends: none. Ownership: Joel (x86_64 lane). Claim: unassigned.
