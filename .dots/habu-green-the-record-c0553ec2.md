---
title: Green the record and effect API suites
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T10:57:56.507343+03:00"
---

Problem: on the complete run of 2026-09-12 (engine 04701ef9, LESSONS.md red set) the dictionary-record and effect-API suites are red, exit code and first failure: xt-effect (1: F11); xt-cell (1: F20); create-axiom (1: F27); internal-word-gate (1: F449); verify-prim (1: F19); does-clause-record (78: duplicate definition: DR-PAD-FAIL, F12); using-import (1: F13); engine-error-package (1: checker-package occurrence count); native-gate-dictionary (1: FAIL: hb trusted CREATE...DOES> effect recording output); c-call-emitter-shape (1: F12). These never reached the pool before habu-run-every-registered-56d4962d, so each needs a first diagnosis: a stale expectation, a retired surface, or a live defect. Acceptance: each suite green on the root engine, or its case retired with the reason in the commit and the LESSONS.md red set updated in the same commit; one commit per distinct defect. Files: the suite files named in test/gate-stdlib-cases.f for these suites, src/core and src/habu where a live defect is found. Verify: each suite standalone on the root engine, then test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.
