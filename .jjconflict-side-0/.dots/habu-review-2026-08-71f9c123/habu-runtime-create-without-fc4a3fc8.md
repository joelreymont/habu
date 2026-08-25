---
title: runtime create without does> publishes nothing
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.809228+02:00"
---

Problem: test/create-axiom-test.f:10 says a runtime-created name receives '-- ptr a'; in both engines LCREATE called from a definer body runs with x15=0 (bootstrap/cg/forth.fs:671, native EMIT-CREATE x15 flag habu2.f:3150-3156) and skips the hook and the definer-side publication, so ': MAKE ( -- ) create ; MAKE X : USE ( -- ptr a ) X ;' rejects X as undefined to the checker. Fail-closed, but a documented capability that does not exist. Acceptance: Checker-Miss RCA per docs/forth.md; the common LCREATE tail in both engines publishes '-- ptr a' for a runtime-created word and DOESPATCH replaces it when a does> clause declares otherwise; the probe certifies and runs; a regression in test/create-axiom-test.f and the stage0 fixtures. Files: src/habu/habu2.f, bootstrap/cg/forth.fs, test/create-axiom-test.f, test/bootstrap-wide-memory-src.f. Verify: the probe under native and under the recovery gate. Depends: 9269e3a3. Ownership: definer publication. Claim: unassigned.
