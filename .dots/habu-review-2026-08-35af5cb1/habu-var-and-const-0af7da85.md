---
title: variable and constant axiom rows certify a lie
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.818696+02:00"
---

Problem: src/core/checker.f:6542-6543 'PRIM: variable PE-PTR-A PE-OUT PRIM;' and 'PRIM: constant PE-A PE-OUT PRIM;' state the effect of the word the definer CREATES, not of calling the definer - the exact class 760e9c90 fixed for create. DEFINER-TOK (8976-8981) models them only under a declared signature; with no signature DO-TOK falls to TRY-PRIMS and CHECK records the inferred row under the word's name, so ': MKV variable ;' publishes '( -- ptr a )' and ': MKC constant ;' '( -- a )' while the machine (habu2.f:3165-3195) pushes nothing / pops one. NDICT:SPELL-ARITY answers 0/1. Acceptance: rows become 'PRIM: variable PRIM;' and 'PRIM: constant PE-N PE-IN PRIM;'; test/create-axiom-test.f gains the same seven-assertion matrix for both; the mirror publishes definer-side rows first (else the recovery leg regresses the way create did). Files: src/core/checker.f, test/create-axiom-test.f. Verify: the matrix red on the old rows, green on the new; recovery gate green. Depends: habu-fix-gforth-recovery-9269e3a3 MUST land first. Ownership: checker axioms. Claim: unassigned.
