---
title: Refuse integer division by zero by name
status: active
priority: 2
issue-type: task
created-at: "2026-09-17T20:02:48.707330+03:00"
---

Problem: '7 0 /' (and mod, /mod) on the arm64 engine prints the crash handler's register dump and exits 134 (SIGTRAP), so a checked program dies with no named refusal and no catch (measured 2026-09-17 by the parity-gate lane; the parity gate keeps division by zero out of its in-process cases for that reason). A checked language's arithmetic primitive should refuse by name, and the same answer must hold on the x86_64 backend, whose idiv raises SIGFPE instead. Acceptance: a zero divisor throws a named error (E-DIV-ZERO or the family's existing name) from /, mod, /mod and any other dividing primitive, catchable in checked code, the same on every backend, with the cost measured on the arm64 division sequence; the parity gate gains the case; docs/forth.md states the contract. Files: src/habu/habu1.f (the arm64 bodies), src/habu/prims.f (rows unchanged unless an effect changes), lib/errors.f, test/prim-parity.f, docs/forth.md. Verify: the parity gate; test/engine-suite.f; byte fixpoint; test/run.f. Depends: habu-specify-the-engine-fcbcee25. Ownership: engine primitives. Claim: agent=hazel-numeric workspace=.jj-ws/hazel-numeric.
