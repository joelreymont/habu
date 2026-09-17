---
title: primitive overload trials are unmodelled and unrecorded
status: closed
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:26.066928+02:00"
closed-at: "2026-09-16T14:34:51.298776+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: Overload trial and its rollback are still outside everything the effect model can state; the row counts in the dot have drifted"
---

Problem: src/core/checker.f registers 300 PRIM: + 91 PPRIM: rows with 15 overloaded names (+ - < <= <> = > >= 1+ 1- and or xor cell+ char+) resolved by trial (checker.f:188 TRIAL-REST, :261 TRIAL-DEPTH/TRY-EFF); Effects.v:1656 'prim' is one effect and run_calls (Effects.v:1535) takes one effect per call; no MODEL GAP names it, so a bug in trial rollback (TRIAL-REST restoring TVK-ANY, :217) is outside everything the model can state. Acceptance: recorded as a MODEL GAP with this dot, or the trial modelled with a vector that exercises a rollback. Files: formal/Common/Effects.v, test/compiler/checker-model-schema.f. Verify: proof slice. Depends: prover. Ownership: proofs. Claim: unassigned.
