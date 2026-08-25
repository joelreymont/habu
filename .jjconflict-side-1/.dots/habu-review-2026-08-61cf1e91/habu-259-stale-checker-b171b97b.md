---
title: 259 stale checker.f line citations in the checker model
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:26.054254+02:00"
---

Problem: formal/Common/Control.v:15-16 promises every definition names its checker word with a line reference; 30 of 30 sampled citations in Effects.v/Control.v point at unrelated code (e.g. Effects.v:1099 UNIFY 'checker.f:1662-1669' is LAYOUT-LINEAR-COUNT; UNIFY is at 1884; Control.v:11 CF-TOK? 8356 -> 10637; Effects.v:186 UNIFY-KIND 1671 -> 771). Nothing reads the citations; the live binding is the 7 frozen tables and 31 VEC-ROW vectors in test/compiler/checker-model-schema.f:783-826. Acceptance: line numbers removed; citations by word name, resolved structurally by checker-model-cases.f (it owns the lexer) so a missing name reds the gate. Files: formal/Common/Effects.v, Control.v, test/compiler/checker-model-cases.f. Verify: the proof slice plus the name check. Depends: prover. Ownership: proofs. Claim: unassigned.
