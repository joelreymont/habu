---
title: Decide the arity rule for a qualified-spelled public over a private twin
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T21:03:35.082230+03:00"
---

Problem (adversarial review of 77fe4a87, MINOR): SHADOW-ARITY-CK (src/core/checker.f ~9291-9308, SBA-PRIVATE-TWIN) refuses ': P:F ( n -- n ) 1 + ;' when a private 'P F ( n n -- n )' exists (CHECKER-RECORD-SYM ~8325 records the qualified spelling under P's public sym) with E-SHADOWED-ARITY and a diagnostic saying the word 'does not bind its own name' - but the hazard the rule exists for does not arise for the qualified spelling: KEEP-TAPE-NAME (src/compiler/native/compiler.f ~180) feeds the QUALIFIED 'PKG:tail' to KEEP-ARITY -> SPELL-REC -> QUALIFIED-REC (dict.f ~95-107), which reads the public record. Qualified definitions are a supported form (test/type-ctor-suite.f:373, test/bootstrap-wide-memory-src.f:380, test/compiler/native-qualified-name.f:15). No in-tree instance. Acceptance: either exempt a qualified NMA (CHECKER-QUALIFIED?) from SHADOW-ARITY-CK with a case in test/shadowed-arity-test.f showing the qualified spelling accepted and ncomp reading the public record, or keep the refusal and make the diagnostic and docs/forth.md's arity bullet say the qualified form is refused too; measured on the engine either way. Files: src/core/checker.f, src/core/render.f, test/shadowed-arity-test.f, docs/forth.md. Verify: the suite; three generations; test/run.f. Depends: none. Ownership: checker. Claim: unassigned.
