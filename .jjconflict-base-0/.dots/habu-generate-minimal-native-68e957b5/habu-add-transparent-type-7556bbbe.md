---
title: Add transparent type aliases
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:38:51.572324+02:00"
---

Invariant: repeated structural types may have a short package-owned spelling without creating a new nominal identity or changing type checking. The language has no transparent alias surface. Long matrix and other structural signatures are therefore repeated in definitions and generated checked fixtures; TYPEFAMILY is nominal and would incorrectly make those spellings distinct types.

Specify and implement parameterized transparent type aliases with explicit package visibility. Resolve and canonicalize aliases before unification and lowering while retaining the source alias for useful diagnostics. Aliases must not create constructors, tags, layouts, runtime values, reflection identities, serialization changes, or implicit conversions. Reject duplicate parameters, wrong arity, direct and indirect cycles, private or ambiguous aliases, invalid nested applications, and aliases whose expansion violates existing kind or layout rules. Define deterministic snapshot, ahead-of-time, recovery, and fixpoint behavior.

First census repeated structural signatures across production source, generated checker strings, tests, and documentation, then introduce only aliases that materially reduce repetition and improve semantic names. Prove alias and expanded forms unify identically, nominally different families remain distinct, wrong extents and spaces still reject, nested generic aliases expand correctly, diagnostics identify both alias and canonical mismatch, and package, checker, PTX, snapshot, ahead-of-time, recovery, fixpoint, size, and full native gates pass. Measure source bytes and tokens, parser and checker JIT and DATA, CODELEN, compile time, and diagnostic size before and after; require a demonstrated net reduction.

Dependency review 2026-07-21: alias declaration must use the common generated-declaration transaction. It may not add a separate rollback path or publish a partially registered alias.
