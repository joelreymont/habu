---
title: End a passed record when a program catch takes it
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T11:42:44.835652+01:00"
closed-at: "2026-11-16T04:07:36.233184+01:00"
---

Problem: REJECTION:PASS (src/core/rejection.f:826) writes the nearest handler into PASSED-TO-CELL (src/habu/layout.f:818); LEFT and BEGIN-TEXT (src/habu/interpreter.f:3185-3207) and BRETHROW (src/habu/habu1.f:3072) read it, but nothing ends a pass a program catch takes, so the cell keeps a dead frame's address: test/linker.f cases f, g, h fail (header cell $27F8 changed after a build whose body caught a rejection), and a later frame at the same address would take the stale pass for its own.

Design (approved): each thread counts the throws it begins (THROWS-CELL, in place of PASSED-TO-CELL); a throw on (BRETHROW, BCATCH-FORWARD's inner rethrow; BFINALLY and BC2-INVOKE when the count has not moved since their code landed) keeps the count. REJECTION keeps a stamp, the number of the throw that carries its record: PASS sets it to count+1 and throws; the record is carried while the stamp equals the count. OWN-HND, SAVED-CODE and the cell move go.

Acceptance: test/linker.f and test/diag-position.f pass; a rejection leaving a build body uncaught keeps its record. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
