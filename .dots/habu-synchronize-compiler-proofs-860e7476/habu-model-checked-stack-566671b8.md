---
title: Model checked stack effects in Rocq
status: active
priority: 1
issue-type: task
created-at: "2026-07-28T11:11:36.383408+02:00"
---

Claim: agent=effects_model workspace=.jj-ws/habu-model-checked-stack-566671b8

Full context: the habu checker is a typed, row-polymorphic stack-effect system specified in docs/effects.md and implemented in src/core/checker.f (10135 lines). It is the artifact the project rests on: model-written Forth is trusted because the checker accepts it. Nothing formal covers it. The four existing Rocq files under formal/Common cover compiler IR identities only. Scope: define formal/Common/Effects.v modelling the effect language exactly as docs/effects.md specifies. An effect is four rows (Din Rin -- Dout Rout). A stack is an optional leading row variable followed by types. Row variables A..Z stand for the unseen tail below; type variables a..z are polymorphic and the same letter means the same type within one signature; stacks with no leading row variable share one implicit data row and one implicit return row. Model the concrete types (i64 u8 u32 cell bool char str addr), roles, declared roles, ptr, and nested quotation effects. Provide executable sequential composition of two effects and executable unification of two stacks, both total functions returning option so failure is a value and never an exception. Acceptance: Rocq 9.2 compiles it via the existing formal/Makefile after adding one line to formal/_CoqProject in dependency order; definitional examples cover composition of concrete effects, row-variable capture of a deeper tail, type-variable binding and the same-letter constraint, and rejected unifications including arity mismatch and conflicting concrete types; no theorem beyond definitional examples; no Admitted. Ownership: formal/Common/Effects.v and its _CoqProject line only. Excludes soundness theorems, the checker parser and diagnostics, IR identities, and any Habu edit. Laws and the acceptance-implies-no-underflow soundness theorem follow as separate leaves.
