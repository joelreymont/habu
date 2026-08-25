---
title: "proof scaffolding: nine CAS restatements, seven gate copies, a lexer that ignores Rocq comments"
status: open
priority: 3
issue-type: task
created-at: "2026-08-22T22:38:26.073425+02:00"
---

Problem: IdAllocator.v + IdAllocatorLaws.v (757 lines) restate the CAS axiom nine times (host_* theorems :349-464) about a three-line loop copied into arena.f:146, context.f:188, build.f:351 of which only id.f:76-78 is bound; 38 gate files / 17,057 lines carry byte-near-identical ADMITTED-TOKEN?/DECL-HEAD?/QUERY-HEAD?/WALK-TOKEN/PHASE-DECLARATIONS (ir-intern-proof.f:175-230, ir-structure-proof.f:187-245, ir-storage-proof.f:174-229, reloc-proof.f:225-280 ...); lib/errors.f:429-431 mislabels the storage block; test/compiler/ir-id-source.f:21,90-91 lexes .v files as Forth (no '(* *)'), so prose must avoid the words admit/Theorem/Module/End (LESSONS.md:3701-3707) while Print Assumptions already covers admits; Effects.v:393-399 claims small-bound monotonicity without a theorem; docs/compiler-ir-design.md:1837-1873 lists 27 .v files (11 exist) and :1963 a CI that does not. Acceptance: one host_run_refines and one shared CAS word so one binding covers four counters; one proof-walk module parameterised by (model, manifest, prefix); the token scan deleted or taught (* *); the doc tree/CI text corrected. Files: formal/Common/IdAllocator*.v, test/compiler/*-proof.f, ir-id-source.f, docs/compiler-ir-design.md. Verify: proof slice. Depends: prover. Ownership: proofs. Claim: unassigned.
