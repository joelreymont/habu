---
title: "Check a does> clause's tail as a colon body's"
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T18:08:04.649979+02:00"
---

Problem: CHECK-DOES-RUN (src/core/checker.f) ends a clause differently from CHECK ends a colon body, so the same body is admitted as a colon word and refused as a clause, or the reverse (lane dfrsh review, probes /tmp/jerry-dfrshrv-tmp). (1) It coerces the declared outputs even when the body never returns: `does> ( n -- ptr u8 ) drop 5 throw` is refused on master while its colon twin is admitted; CHECK runs SUNI-COERCE only under CHECK-RETURNS? and otherwise FRESH-CLAIM. (2) It never runs NP-COLLECT / NP-CHECK, so `does> ( a -- n ) drop` and a clause unifying two declared quantifiers are admitted where every colon twin is E-NONPARAMETRIC-EFFECT. Acceptance: both cases answer as their colon twins, rows in test/certify-does-definer.f or test/rigid-region-suite.f failing on master; engine-provided clauses still build. Fix: CHECK's tail in CHECK-DOES-RUN, NP-COLLECT after RAW-SIG! and before SIG-BODY-FRESH, `OK @ IF NP-CHECK THEN` before CHECK-VERDICT. Files: src/core/checker.f, a test file. Verify: engine build, full suite (shared checker behaviour). Depends: lane dfrsh landed. Ownership: checker.f CHECK-DOES-RUN. Claim: unassigned.
