---
title: Specify the engine primitives in one table
status: active
priority: 2
issue-type: task
created-at: "2026-09-17T17:32:42.537487+03:00"
---

Problem: docs/x86-64.md (campaign habu-campaign-c6-targets-86bb56bb): the engine's primitives are an ARM64 assembly body in src/habu/habu1.f and a checker PRIM: row for the same name, so a second backend would fork the list. Acceptance: one machine-independent table src/habu/prims.f listing every primitive with its checker row and a reference implementation in checked Habu where one exists; the arm64 engine draws its rows from that table (no second copy of the effects); a parity gate (test/engine-suite.f is already the engine's behaviour suite, so the gate is its own file, test/prim-parity.f, driven by the table) that runs the same cases against each backend's primitive bodies and the reference, green on arm64 first; the table is the contract the x86_64 and Cortex-M backends implement. Files: src/habu/prims.f, src/habu/habu1.f, src/core/checker.f (row source), test/prim-parity.f. Verify: bin/hb --load test/prim-parity.f; byte fixpoint; tools/bootstrap.sh check; test/run.f. Depends: none. Ownership: hazel runs it on the arm64 host in two sequential workers (the table and the arm64 row source first, the references and the parity gate second). Claim: agent=hazel-prims-table workspace=.jj-ws/hazel-prims-table.
