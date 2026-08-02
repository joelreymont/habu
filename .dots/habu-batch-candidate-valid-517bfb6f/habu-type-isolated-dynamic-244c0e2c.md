---
title: Type isolated dynamic source execution
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T16:06:09.030541+02:00\""
---

Full context: lib/test/subject.f needs two named TRUSTED boundaries because the checker cannot express dynamic evaluate effects or a fresh child stack/handler region. Cause: dynamic source has no typed immutable artifact effect, and raw S0/HND/REPLH cells have no region, lifetime, handler-state, or alignment proof. Fix: add a typed compiler/metaprogramming capability that accepts an immutable digest-bound source artifact, executes it in an explicit isolated child context, and returns a typed raw outcome without direct recovery-cell mutation. Until retirement, each source boundary retains its rationale, this retirement owner, and focused lib/test/subject-test.f production proof. Acceptance: replace SUBJECT:EVAL and SUBJECT:STACK-ARM TRUSTED leaves without replacement; preserve exact exit/signal/stdout/stderr differential evidence; prove source digest identity, stack bounds/alignment, fresh handler state, and child-only lifetime; add negative checked regressions; add no runtime guards.
