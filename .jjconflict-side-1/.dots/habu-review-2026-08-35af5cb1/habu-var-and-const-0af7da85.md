---
title: variable and constant axiom rows certify a lie
status: closed
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.818696+02:00"
closed-at: "2026-08-23T12:59:09.391646+02:00"
close-reason: implemented, reviewed, merged, gates green: PRIM: variable is the empty row and PRIM: constant is PE-N PE-IN (7eb8a2f0, landed on master); test/create-axiom-test.f carries a seven-assertion matrix per row (10 red on the old rows, 0 on the new; four discriminators, one labelled control, two fooling fixtures); maki, native-migrate, native-chain, check-all-errors, mirror-lint and codegen tests, both diff lints, error-code-lint, schedule-lint, lint-libs and the recovery probe green on the landed tree. Correction from the lane: the bad caller certified through the checker's source verifier (tools/check.f preverify), not the engine's inference - the engine cannot load a body naming these keywords at all; the deeper gap is habu-checker-certifies-bodies-803cff1d.
---

Problem: src/core/checker.f:6542-6543 'PRIM: variable PE-PTR-A PE-OUT PRIM;' and 'PRIM: constant PE-A PE-OUT PRIM;' state the effect of the word the definer CREATES, not of calling the definer - the exact class 760e9c90 fixed for create. DEFINER-TOK (8976-8981) models them only under a declared signature; with no signature DO-TOK falls to TRY-PRIMS and CHECK records the inferred row under the word's name, so ': MKV variable ;' publishes '( -- ptr a )' and ': MKC constant ;' '( -- a )' while the machine (habu2.f:3165-3195) pushes nothing / pops one. NDICT:SPELL-ARITY answers 0/1. Acceptance: rows become 'PRIM: variable PRIM;' and 'PRIM: constant PE-N PE-IN PRIM;'; test/create-axiom-test.f gains the same seven-assertion matrix for both; the mirror publishes definer-side rows first (else the recovery leg regresses the way create did). Files: src/core/checker.f, test/create-axiom-test.f. Verify: the matrix red on the old rows, green on the new; recovery gate green. Depends: habu-fix-gforth-recovery-9269e3a3 MUST land first. Ownership: checker axioms. Claim: closed (landed on master 7eb8a2f0).
