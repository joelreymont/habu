---
title: "One table for the compiler's bare-name lookups"
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T13:17:55.798065+03:00"
---

Problem: src/habu/habu2.f spells the words the compiled code looks up by bare name as string literals (defer-unset, tfl-cvar?, rec-wide-publish, rec-min-in@, NEUTRAL-PARSE-IMM?, checker-export, lower-cert:bytes, tfl-con-fam?, tfam-name$, tfl-match-fam?, check-does!, CK-AOT-REG-INSTALL), bootstrap/cg/forth.fs mirrors them, and the AOT capture keep-set (aot-capture.f ACAP-KEEP?) must agree with that list by hand; the strip lane found two missing (defer-unset, tfl-cvar?) only by diffing sidecars. tfl-cvar? is also the only one of the four TFL bridge names without a PRIM: axiom, which is why the seal catches it while its siblings escape; test/internal-word-gate.f:1102 pins that asymmetry. Acceptance: one table in the tree that both the emitter and the keep-set read, mirrored by the seed under the two-stage rule, a test that fails when an emitter literal is not in the table, and a decision on tfl-cvar?'s axiom recorded in the gate. Files: src/habu/habu2.f, src/habu/aot-capture.f, bootstrap/cg/forth.fs, test/internal-word-gate.f. Verify: the new test; fixpoint; bootstrap check. Depends: none. Ownership: emitter. Claim: unassigned.
